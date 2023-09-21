package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.analysis.{AltoProcessor, TextAnalyzer}
import com.joliciel.jochre.ocr.core.model.{ComposedBlock, Illustration, Page, TextBlock}
import com.joliciel.jochre.ocr.core.output.Alto4Writer
import com.joliciel.jochre.ocr.core.segmentation.{BlockPredictorService, IllustrationSegment, ImageSegmentExtractor, TextSegment}
import com.joliciel.jochre.ocr.core.transform.{BoxTransform, Deskewer, GrayscaleTransform, ResizeImageAndKeepAspectRatio, Scale, SkewAngle}
import com.joliciel.jochre.ocr.core.utils.{FileUtils, OpenCvUtils, OutputLocation, XmlImplicits}
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.global.opencv_imgcodecs.{IMREAD_GRAYSCALE, imread}
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import zio.{Task, ZIO}

import java.awt.image.BufferedImage
import java.io.{File, FileWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.xml.{Elem, PrettyPrinter}

trait Jochre {
  def process(inputDir: Path, outputDir: Option[Path], maxImages: Option[Int]): Task[Seq[Elem]]
  def processImage(bufferedImage: BufferedImage, outputDir: Option[Path], fileName: String): Task[Elem]
  def processImage(mat: Mat, outputDir: Option[Path], fileName: String): Task[Elem]
}

trait AbstractJochre extends Jochre with OpenCvUtils with XmlImplicits {
  private val log = LoggerFactory.getLogger(getClass)
  private val config = ConfigFactory.load().getConfig("jochre.ocr")
  private val longerSide: Int = config.getInt("corpus-transform.longer-side")
  private val boxImages: Boolean = config.getBoolean("corpus-transform.box-images")

  def blockPredictorService: BlockPredictorService
  def textAnalyzer: TextAnalyzer
  def altoProcessor: AltoProcessor

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

  def process(inputDir: Path, outputDir: Option[Path], maxImages: Option[Int]): Task[Seq[Elem]] = {
    val allFiles = FileUtils.recursiveListImages(inputDir.toFile).map(new File(_))

    val inputFiles = allFiles
      .take(maxImages.getOrElse(allFiles.size))

    ZIO.foreach(inputFiles.zipWithIndex)
      { case (inputFile, i) =>
        log.debug(f"Processing file $i: ${inputFile.getPath}")
        val mat = imread(inputFile.getPath, IMREAD_GRAYSCALE)
        this.processImage(mat, outputDir, inputFile.getName)
      }
  }

  def processImage(bufferedImage: BufferedImage, outputDir: Option[Path], fileName: String): Task[Elem] = {
    val mat = fromBufferedImage(bufferedImage)
    this.processImage(mat, outputDir, fileName)
  }

  def processImage(mat: Mat, outputDir: Option[Path], fileName: String): Task[Elem] = {
    val outputLocation = outputDir.map(OutputLocation(_, fileName))
    val baseName = FileUtils.removeFileExtension(fileName)
    for {
      // apply transforms
      transformedAndData <- ZIO.attempt{
        transforms.foldLeft(mat -> Seq.empty[Any]) {
          case ((mat, outputData), transformer) =>
            val (transformed, output) = transformer.transform(fileName, mat)
            transformed -> (outputData :+ output)
        }
      }
      transformed = transformedAndData._1
      outputData = transformedAndData._2
      skewAngle = outputData
        .collect {
          case SkewAngle(value) => Some(value)
          case _ => None
        }.flatten.headOption.getOrElse(0.0)
      scale = outputData
        .collect {
          case Scale(value) => Some(value)
          case _ => None
        }.flatten.headOption.getOrElse(1.0)

      blockPredictor <- blockPredictorService.getBlockPredictor(transformed, fileName, outputLocation)
      // detect blocks
      segments <- blockPredictor.predict()
        .mapAttempt{labeledRectangles =>
          // re-scale coordinates
          val rescaledRectangles = labeledRectangles.map(_.rescale(1 / scale))

          val uprightImage = this.unrotate(skewAngle, mat)

          val imageSegmentExtractor = ImageSegmentExtractor(uprightImage, rescaledRectangles, outputLocation)
          imageSegmentExtractor.segments
        }
      alto <- ZIO.attempt{
        // analyze OCR in text blocks
        val blocks = segments.map{
          case TextSegment(block, subImage) =>
            // Analyze OCR on each text segment and extract the analyzed blocks
            log.debug(f"About to perform OCR analysis for text segment $block")
            textAnalyzer.analyze(subImage).map { altoXml =>
              val jochreSubImage = Page.fromXML(altoXml)
              val translatedSubImage = jochreSubImage.rotate().translate(block.left, block.top)
              log.debug(f"OCR analysis complete for $block")
              translatedSubImage.blocks
            }.getOrElse(Seq.empty)
          case IllustrationSegment(block) =>
            Seq(Illustration(block))
        }.flatten.sortBy(_.rectangle)

        log.debug(f"Found ${blocks.size} blocks")

        val allConfidences = blocks.flatMap{
          case composedBlock:ComposedBlock => composedBlock.allWords
          case textBlock:TextBlock => textBlock.allWords
          case _ => Seq()
        }.map(_.confidence)

        val meanConfidence = if (allConfidences.size==0) {
          0.0
        } else {
          allConfidences.sum / allConfidences.size
        }

        val page = Page(
          id = baseName,
          height = mat.rows(),
          width = mat.cols(),
          physicalPageNumber = 1,
          rotation = skewAngle,
          language = "yi",
          confidence = meanConfidence,
          blocks = blocks
        )

        val rotatedPage = page.rotate()

        val alto4Writer = Alto4Writer(rotatedPage, fileName)
        val alto = alto4Writer.alto

        val prettyPrinter = new PrettyPrinter(80, 2)

        outputLocation.foreach { outputLocation =>
          val altoFile = outputLocation.resolve("_alto4.xml").toFile
          val writer = new FileWriter(altoFile, StandardCharsets.UTF_8)
          try {
            val xml = prettyPrinter.format(alto)
            writer.write(xml)
            writer.flush()
          } finally {
            writer.close()
          }
        }

        val fixedAlto = altoProcessor.process(alto, fileName)

        outputLocation.foreach { outputLocation =>
          val altoFile = outputLocation.resolve("_fixed_alto4.xml").toFile
          val writer = new FileWriter(altoFile, StandardCharsets.UTF_8)
          try {
            val xml = prettyPrinter.format(fixedAlto)
            writer.write(xml)
            writer.flush()
          } finally {
            writer.close()
          }
        }

        fixedAlto
      }
    } yield alto
  }
}