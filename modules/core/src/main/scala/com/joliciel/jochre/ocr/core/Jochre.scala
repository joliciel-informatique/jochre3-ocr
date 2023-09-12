package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.analysis.TextAnalyzer
import com.joliciel.jochre.ocr.core.model.{Illustration, Page}
import com.joliciel.jochre.ocr.core.output.Alto4Writer
import com.joliciel.jochre.ocr.core.segmentation.{BlockPredictor, IllustrationSegment, ImageSegmentExtractor, TextSegment}
import com.joliciel.jochre.ocr.core.transform.{BoxTransform, Deskewer, GrayscaleTransform, ResizeImageAndKeepAspectRatio, Scale, SkewAngle}
import com.joliciel.jochre.ocr.core.utils.{FileUtils, OpenCvUtils, OutputLocation, XmlImplicits}
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.global.opencv_imgcodecs.{IMREAD_GRAYSCALE, imread}
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory

import java.awt.image.BufferedImage
import java.io.{File, FileWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.xml.{Elem, PrettyPrinter}

trait Jochre extends OpenCvUtils with XmlImplicits {
  private val log = LoggerFactory.getLogger(getClass)
  private val config = ConfigFactory.load().getConfig("jochre.ocr")
  private val longerSide: Int = config.getInt("corpus-transform.longer-side")
  private val boxImages: Boolean = config.getBoolean("corpus-transform.box-images")

  def outputDir: Option[Path]
  def textAnalyzer: TextAnalyzer

  private val transforms = List(
    // change to grayscale
    Some(new GrayscaleTransform()),

    // resize the image
    Some(new ResizeImageAndKeepAspectRatio(longerSide)),

    // deskew the image
    Some(new Deskewer()),

    // box the image if required
    Option.when(boxImages)(new BoxTransform(longerSide)),
  ).flatten

  def process(inputDir: Path, maxImages: Option[Int]): Seq[Elem] = {
    val allFiles = FileUtils.recursiveListImages(inputDir.toFile).map(new File(_))

    val inputFiles = allFiles
      .take(maxImages.getOrElse(allFiles.size))

    inputFiles.zipWithIndex.map { case (inputFile, i) =>
      log.debug(f"Processing file $i: ${inputFile.getPath}")
      val mat = imread(inputFile.getPath, IMREAD_GRAYSCALE)
      this.processImage(mat, inputFile.getName)
    }
  }

  def processImage(bufferedImage: BufferedImage, fileName: String): Elem = {
    val mat = fromBufferedImage(bufferedImage)
    this.processImage(mat, fileName)
  }

  def processImage(mat: Mat, fileName: String): Elem = {
    val outputLocation = outputDir.map(OutputLocation(_, fileName))

    // apply transforms
    val (transformed: Mat, outputData: Seq[Any]) = transforms.foldLeft(mat -> Seq.empty[Any]) {
      case ((mat, outputData), transformer) =>
        val (transformed, output) = transformer.transform(fileName, mat)
        transformed -> (outputData :+ output)
    }

    val skewAngle = outputData.collect{
      case SkewAngle(value) => Some(value)
      case _ => None
    }.flatten.headOption.getOrElse(0.0)

    val scale = outputData.collect {
      case Scale(value) => Some(value)
      case _ => None
    }.flatten.headOption.getOrElse(1.0)

    // detect blocks
    val blockPredictor = BlockPredictor(transformed, fileName, outputLocation)
    val labeledRectangles = blockPredictor.predict()

    // re-scale coordinates
    val rescaledRectangles = labeledRectangles.map(_.rescale(1 / scale))

    val uprightImage = this.unrotate(skewAngle, mat)

    val imageSegmentExtractor = ImageSegmentExtractor(uprightImage, rescaledRectangles, outputLocation)
    val segments = imageSegmentExtractor.segments

    val blocks = segments.flatMap{
      case TextSegment(block, subImage) =>
        // Analyze OCR on each text segment and extract the analyzed blocks
        val altoXml = textAnalyzer.analyze(subImage)
        val jochreSubImage = Page.fromXML(altoXml)
        val translatedSubImage = jochreSubImage.rotate().translate(block.left, block.top)
        translatedSubImage.blocks
      case IllustrationSegment(block) =>
        Seq(Illustration(block))
    }.sortBy(_.rectangle)

    val page = Page(
      id = "1",
      height = mat.rows(),
      width = mat.cols(),
      physicalPageNumber = 1,
      rotation = skewAngle,
      blocks = blocks
    )

    val rotatedPage = page.rotate()

    val alto4Writer = Alto4Writer(rotatedPage, fileName)
    val alto = alto4Writer.alto

    outputLocation.foreach { outputLocation =>
      val altoFile = outputLocation.resolve("_alto4.xml").toFile
      val prettyPrinter = new PrettyPrinter(80, 2)
      val writer = new FileWriter(altoFile, StandardCharsets.UTF_8)
      try {
        val xml = prettyPrinter.format(alto)
        writer.write(xml)
        writer.flush()
      } finally {
        writer.close()
      }
    }

    alto
  }
}