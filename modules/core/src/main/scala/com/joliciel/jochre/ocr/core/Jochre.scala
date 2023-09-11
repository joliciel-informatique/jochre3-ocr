package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.model.{Illustration, JochreImage, Paragraph, TextBlock}
import com.joliciel.jochre.ocr.core.output.Alto4Writer
import com.joliciel.jochre.ocr.core.segmentation.{Block, BlockPredictor}
import com.joliciel.jochre.ocr.core.transform.{BoxTransform, Deskewer, ResizeImageAndKeepAspectRatio, Scale, SkewAngle}
import com.joliciel.jochre.ocr.core.utils.{FileUtils, OpenCvUtils, OutputLocation}
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.global.opencv_imgcodecs.{IMREAD_GRAYSCALE, imread}
import org.bytedeco.opencv.opencv_core.Mat
import org.rogach.scallop.{ScallopConf, ScallopOption}
import org.slf4j.LoggerFactory

import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.Path

case class Jochre(outputDir: Path) extends OpenCvUtils {
  private val log = LoggerFactory.getLogger(getClass)
  private val config = ConfigFactory.load().getConfig("jochre.ocr")
  private val longerSide: Int = config.getInt("corpus-transform.longer-side")
  private val boxImages: Boolean = config.getBoolean("corpus-transform.box-images")

  private val transforms = List(
    // resize the image
    Some(new ResizeImageAndKeepAspectRatio(longerSide)),

    // deskewer the image
    Some(new Deskewer()),

    // box the image if required
    Option.when(boxImages)(new BoxTransform(longerSide)),
  ).flatten

  def process(inputDir: Path, maxImages: Option[Int]): Unit = {
    val allFiles = FileUtils.recursiveListImages(inputDir.toFile).map(new File(_))

    val inputFiles = allFiles
      .take(maxImages.getOrElse(allFiles.size))

    inputFiles.zipWithIndex.foreach { case (inputFile, i) =>
      log.debug(f"Processing file $i: ${inputFile.getPath}")
      val mat = imread(inputFile.getPath, IMREAD_GRAYSCALE)
      this.processImage(mat, inputFile.getName)
    }
  }

  def processImage(bufferedImage: BufferedImage, fileName: String): Unit = {
    val mat = fromBufferedImage(bufferedImage)
    this.processImage(mat, fileName)
  }

  def processImage(mat: Mat, fileName: String): Unit = {
    val outputLocation = Some(OutputLocation(outputDir, fileName))

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
    val blocks = blockPredictor.predict()

    // re-scale coordinates
    val rescaledBlocks = blocks.map(_.rescale(1 / scale))

    val textBlocks = rescaledBlocks
      .filter{ block => block.label == Block.Paragraph.entryName || block.label==Block.TextBox.entryName }
      .map { block =>
        TextBlock(
          paragraphs = Seq(Paragraph(Seq.empty, block)),
          rectangle = block
        )
      }
    val illustrations = rescaledBlocks
      .filter(_.label == Block.Illustration.entryName)
      .map { block =>
        Illustration(
          rectangle = block
        )
      }

    val jochreImage = JochreImage(
      id = "1",
      height = transformed.rows(),
      width = transformed.cols(),
      physicalPageNumber = 1,
      rotation = skewAngle,
      textBlocks = textBlocks,
      illustrations = illustrations,
    )

    // write Alto file
    val alto4Writer = new Alto4Writer(jochreImage, fileName)
    outputLocation.foreach { outputLocation =>
      val altoFile = outputLocation.resolve("_alto4.xml").toFile
      alto4Writer.write(altoFile)
    }
  }
}

object Jochre {
  class JochreCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val inputDir: ScallopOption[String] = opt[String](required = true)
    val outputDir: ScallopOption[String] = opt[String](required = true)
    val maxImages: ScallopOption[Int] = opt[Int](default = Some(0))
    verify()
  }

  def main(args: Array[String]): Unit = {
    val options = new JochreCLI(args.toIndexedSeq)

    val inputDir = Path.of(options.inputDir())
    val outDir = Path.of(options.outputDir())
    outDir.toFile.mkdirs()

    val maxImages = Option.when(options.maxImages() > 0)(options.maxImages())

    val jochre: Jochre = Jochre(outDir)
    jochre.process(inputDir, maxImages)
  }
}