package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.alto.AltoTransformer
import com.joliciel.jochre.ocr.core.model.{ComposedBlock, Page, TextBlock}
import com.joliciel.jochre.ocr.core.output.Alto4Writer
import com.joliciel.jochre.ocr.core.segmentation.SegmenterService
import com.joliciel.jochre.ocr.core.text.TextGuesserService
import com.joliciel.jochre.ocr.core.transform.{Deskewer, GrayscaleTransform, Scale, SkewAngle}
import com.joliciel.jochre.ocr.core.utils.{FileUtils, ImageUtils, OutputLocation, XmlImplicits}
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import zio.{Task, ZIO}

import java.awt.image.BufferedImage
import java.io.{File, FileWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.xml.{Elem, PrettyPrinter}

trait Jochre {
  def getImageFilesFromDir(inputDir: Path, maxImages: Option[Int]): Seq[(File, Mat)]
  def process(inputDir: Path, outputDir: Option[Path], debugDir: Option[Path], maxImages: Option[Int]): Task[Seq[Elem]]
  def processImage(bufferedImage: BufferedImage, outputDir: Option[Path], debugDir: Option[Path], fileName: String): Task[Elem]
  def processImage(mat: Mat, outputDir: Option[Path], debugDir: Option[Path], fileName: String): Task[Elem]
}

trait AbstractJochre extends Jochre with ImageUtils with FileUtils with XmlImplicits {
  private val log = LoggerFactory.getLogger(getClass)

  def altoTransformer: AltoTransformer
  def segmenterService: SegmenterService
  def textGuesserService: TextGuesserService

  private val transforms = List(
    // change to grayscale
    Some(new GrayscaleTransform()),

    // deskew the image
    Some(new Deskewer()),
  ).flatten

  override def getImageFilesFromDir(inputDir: Path, maxImages: Option[Int]): Seq[(File, Mat)] = {
    val allFiles = FileUtils.recursiveListImages(inputDir.toFile).map(new File(_))

    allFiles
      .take(maxImages.getOrElse(allFiles.size))
      .map{ inputFile =>
        val mat = loadImage(inputFile.getPath)
        inputFile -> mat
      }
  }

  override def process(inputDir: Path, outputDir: Option[Path], debugDir: Option[Path], maxImages: Option[Int]): Task[Seq[Elem]] = {
    val filesAndImages = getImageFilesFromDir(inputDir, maxImages)

    ZIO.foreach(filesAndImages.zipWithIndex)
      { case ((inputFile, mat), i) =>
        log.debug(f"Processing file $i: ${inputFile.getPath}")
        this.processImage(mat, outputDir, debugDir, inputFile.getName)
      }
  }

  override def processImage(bufferedImage: BufferedImage, debugDir: Option[Path], outputDir: Option[Path], fileName: String): Task[Elem] = {
    val mat = fromBufferedImage(bufferedImage)
    this.processImage(mat, outputDir, debugDir, fileName)
  }

  override def processImage(mat: Mat, outputDir: Option[Path], debugDir: Option[Path], fileName: String): Task[Elem] = {
    log.info(f"Processing image $fileName of size ${mat.cols()}X${mat.rows()}")

    val baseName = FileUtils.removeFileExtension(fileName)
    val outputLocation = outputDir.map(OutputLocation(_, baseName))
    val debugLocation = debugDir.map(OutputLocation(_, baseName))

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

      _ <- ZIO.attempt{
        debugLocation.foreach { debugLocation =>
          val transformedFile = debugLocation.resolve("_transformed.png").toFile
          saveImage(transformed, transformedFile.getAbsolutePath)
        }
      }

      segmenter <- segmenterService.getSegmenter()
      segmented <- segmenter.segment(transformed, fileName, debugLocation)
      textGuesser <- textGuesserService.getTextGuesser()
      pageWithContent <- textGuesser.guess(segmented, transformed, fileName, debugLocation)
      alto <- ZIO.attempt{
        val allConfidences = pageWithContent.blocks.flatMap{
          case composedBlock:ComposedBlock => composedBlock.allWords
          case textBlock:TextBlock => textBlock.allWords
          case _ => Seq()
        }.map(_.confidence)

        val meanConfidence = if (allConfidences.size==0) {
          0.0
        } else {
          allConfidences.sum / allConfidences.size
        }

        val page = pageWithContent.copy(
          id = baseName,
          rotation = skewAngle,
          language = "yi",
          confidence = meanConfidence,
        )

        val rotatedPage = page.rescale(1.0 / scale).rotate()

        val alto4Writer = Alto4Writer(rotatedPage, fileName)
        val alto = alto4Writer.alto

        val prettyPrinter = new PrettyPrinter(80, 2)

        debugLocation.foreach { debugLocation =>
          val altoFile = debugLocation.resolve("_initial_alto4.xml")
          writeFile(altoFile, prettyPrinter.format(alto))
        }

        val fixedAlto = altoTransformer.process(alto, fileName)

        outputLocation.foreach { outputLocation =>
          val altoFile = outputLocation.resolve("_alto4.xml")
          writeFile(altoFile, prettyPrinter.format(fixedAlto))
        }
        debugLocation.foreach { debugLocation =>
          val contentFile = debugLocation.resolve("_predicted.txt")
          writeFile(contentFile, Page.fromXML(fixedAlto).content)
        }

        fixedAlto
      }
    } yield alto
  }
}