package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.alto.AltoTransformer
import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.model.{Alto, ComposedBlock, Page, TextBlock}
import com.joliciel.jochre.ocr.core.output.OutputFormat
import com.joliciel.jochre.ocr.core.pdf.PDFToImageConverter
import com.joliciel.jochre.ocr.core.segmentation.SegmenterService
import com.joliciel.jochre.ocr.core.text.TextGuesserService
import com.joliciel.jochre.ocr.core.transform.{BrightnessAndContrastTransform, Deskewer, GrayscaleTransform, Scale, SkewAngle}
import com.joliciel.jochre.ocr.core.utils.{FileUtils, ImageUtils, OutputLocation, XmlImplicits}
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import zio.stream.ZSink
import zio.{Task, ZIO}

import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.{Files, Path}
import scala.xml.PrettyPrinter

trait Jochre {
  def processPdf(pdfFile: Path, fileName: Option[String] = None, outputFormats: Seq[OutputFormat] = Seq(OutputFormat.Alto4), outputDir: Option[Path] = None, debugDir: Option[Path] = None, startPage: Option[Int] = None, endPage: Option[Int] = None, dpi: Option[Int] = None, testRectangle: Option[Rectangle] = None): Task[Alto]
  def processDirectory(inputDir: Path, outputFormats: Seq[OutputFormat] = Seq(OutputFormat.Alto4), outputDir: Option[Path] = None, debugDir: Option[Path] = None, maxImages: Option[Int] = None, testRectangle: Option[Rectangle] = None): Task[Map[String, Alto]]
  def processImageFile(imageFile: Path, fileName: Option[String] = None, outputFormats: Seq[OutputFormat] = Seq(OutputFormat.Alto4), outputDir: Option[Path] = None, debugDir: Option[Path] = None, testRectangle: Option[Rectangle] = None): Task[Alto]
  def processImage(bufferedImage: BufferedImage, fileName: String, outputFormats: Seq[OutputFormat] = Seq(OutputFormat.Alto4), outputDir: Option[Path] = None, debugDir: Option[Path] = None, testRectangle: Option[Rectangle] = None): Task[Alto]
  def processMat(mat: Mat, fileName: String, outputFormats: Seq[OutputFormat] = Seq(OutputFormat.Alto4), outputDir: Option[Path] = None, debugDir: Option[Path] = None, testRectangle: Option[Rectangle] = None): Task[Alto]
}

trait AbstractJochre extends Jochre with ImageUtils with FileUtils with XmlImplicits {
  private val log = LoggerFactory.getLogger(getClass)

  private val config = ConfigFactory.load().getConfig("jochre.ocr.transforms")
  private val applyContrastAndBrightness = config.getBoolean("apply-contrast-and-brightness")
  private val contrast = config.getDouble("contrast")
  private val brightness = config.getInt("brightness")

  def altoTransformer: AltoTransformer
  def segmenterService: SegmenterService
  def textGuesserService: TextGuesserService

  private val transforms = List(
    // change to grayscale
    Some(GrayscaleTransform),

    // Increase contrast and brightness
    Option.when(applyContrastAndBrightness)(new BrightnessAndContrastTransform(contrast, brightness)),

    // deskew the image
    Some(new Deskewer()),
  ).flatten

  private def getFilesFromDir(inputDir: Path, maxImages: Option[Int]): Seq[File] = {
    val allFiles = FileUtils.recursiveListFiles(inputDir.toFile, ".*\\.pdf|.*\\.jpg|.*\\.png|.*\\.jpeg".r)
    allFiles
      .take(maxImages.getOrElse(allFiles.size))
  }

  override def processPdf(pdfFile: Path, fileName: Option[String] = None, outputFormats: Seq[OutputFormat] = Seq(OutputFormat.Alto4), outputDir: Option[Path], debugDir: Option[Path], startPage: Option[Int] = None, endPage: Option[Int] = None, dpi: Option[Int] = None, testRectangle: Option[Rectangle]): Task[Alto] = {
    val pdfFileName = fileName.getOrElse(pdfFile.toFile.getName)
    log.debug(f"Processing file: $pdfFileName")
    val baseName = FileUtils.removeFileExtension(pdfFileName)
    val converter = PDFToImageConverter(() => Files.newInputStream(pdfFile), startPage, endPage, dpi)
    for {
      pages <- converter.process((image: BufferedImage, i: Int) => {
        val imageFileName = f"${baseName}_$i%04d.png"
        processImageInternal(image, imageFileName, i, debugDir, testRectangle)
      }).run {
        ZSink.collectAll
      }
      altoXml = Alto(pdfFileName, pages)
      _ <- ZIO.attempt{
        val baseName = FileUtils.removeFileExtension(pdfFileName)
        val outputLocation = outputDir.map(OutputLocation(_, baseName))

        outputFormats.foreach { outputFormat =>
          val result = outputFormat.apply(altoXml)

          outputLocation.foreach { outputLocation =>
            val altoFile = outputLocation.resolve(outputFormat.suffix)
            writeFile(altoFile, result)
          }
        }
      }
    } yield altoXml
  }

  override def processDirectory(inputDir: Path, outputFormats: Seq[OutputFormat] = Seq(OutputFormat.Alto4), outputDir: Option[Path], debugDir: Option[Path], maxImages: Option[Int], testRectangle: Option[Rectangle] = None): Task[Map[String, Alto]] = {
    val files = getFilesFromDir(inputDir, maxImages)

    ZIO.foreach(files.zipWithIndex)
      { case (inputFile, i) =>
        log.debug(f"Processing file $i: ${inputFile.getPath}")
        val altoXml = if (inputFile.getName.endsWith(".pdf")) {
          this.processPdf(inputFile.toPath, None, outputFormats, outputDir, debugDir, testRectangle = testRectangle)
        } else {
          this.processImageFile(inputFile.toPath, None, outputFormats, outputDir, debugDir, testRectangle)
        }
        altoXml.map(altoXml => inputFile.getName -> altoXml)
      }.map(_.toMap)
  }

  override def processImageFile(imageFile: Path, fileName: Option[String], outputFormats: Seq[OutputFormat], outputDir: Option[Path], debugDir: Option[Path], testRectangle: Option[Rectangle]): Task[Alto] = {
    val mat = loadImage(imageFile)
    this.processMat(mat, fileName.getOrElse(imageFile.toFile.getName), outputFormats, outputDir, debugDir, testRectangle)
  }

  override def processImage(bufferedImage: BufferedImage, fileName: String, outputFormats: Seq[OutputFormat] = Seq(OutputFormat.Alto4), outputDir: Option[Path] = None, debugDir: Option[Path] = None, testRectangle: Option[Rectangle] = None): Task[Alto] = {
    val mat = fromBufferedImage(bufferedImage)
    this.processMat(mat, fileName, outputFormats, outputDir, debugDir, testRectangle)
  }

  override def processMat(mat: Mat, fileName: String, outputFormats: Seq[OutputFormat] = Seq(OutputFormat.Alto4), outputDir: Option[Path] = None, debugDir: Option[Path] = None, testRectangle: Option[Rectangle] = None): Task[Alto] = {
    for {
      page <- processMatInternal(mat, fileName, 1, debugDir, testRectangle)
      altoXml = Alto(fileName, Seq(page))
      _ <- ZIO.attempt {
        val baseName = FileUtils.removeFileExtension(fileName)
        val outputLocation = outputDir.map(OutputLocation(_, baseName))

        outputFormats.foreach { outputFormat =>
          val result = outputFormat.apply(altoXml)

          outputLocation.foreach { outputLocation =>
            val altoFile = outputLocation.resolve(outputFormat.suffix)
            writeFile(altoFile, result)
          }
        }
      }
    } yield altoXml
  }

  private def processImageInternal(bufferedImage: BufferedImage, fileName: String, physicalPageNumber: Int = 1, debugDir: Option[Path] = None, testRectangle: Option[Rectangle] = None): Task[Page] = {
    val mat = fromBufferedImage(bufferedImage)
    this.processMatInternal(mat, fileName, physicalPageNumber, debugDir, testRectangle)
  }

  private def processMatInternal(mat: Mat, fileName: String, physicalPageNumber: Int = 1, debugDir: Option[Path] = None, testRectangle: Option[Rectangle] = None): Task[Page] = {
    log.info(f"Processing image $fileName of size ${mat.cols()}X${mat.rows()}")

    val baseName = FileUtils.removeFileExtension(fileName)
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
          saveImage(transformed, transformedFile.toPath)
        }
      }

      segmenter <- segmenterService.getSegmenter()
      segmented <- segmenter.segment(transformed, fileName, debugLocation, testRectangle)
      textGuesser <- textGuesserService.getTextGuesser()
      pageWithContent <- textGuesser.guess(segmented, transformed, fileName, debugLocation)
      alto <- ZIO.attempt{
        val allConfidences = pageWithContent.blocks.flatMap{
          case composedBlock:ComposedBlock => composedBlock.allWords
          case textBlock:TextBlock => textBlock.allWords
          case _ => Seq()
        }.map(_.confidence)

        val meanConfidence = if (allConfidences.isEmpty) {
          0.0
        } else {
          allConfidences.sum / allConfidences.size
        }

        val page = pageWithContent.copy(
          id = baseName,
          rotation = skewAngle,
          language = "yi",
          confidence = meanConfidence,
          physicalPageNumber = physicalPageNumber,
        )

        val rotatedPage = page.rescale(1.0 / scale).rotate()

        val fixedPage = altoTransformer.process(rotatedPage)

        debugLocation.foreach { debugLocation =>
          val prettyPrinter = new PrettyPrinter(80, 2)

          val initialAltoXml = Alto(fileName, Seq(rotatedPage))
          val initialAltoFile = debugLocation.resolve("_initial_alto4.xml")
          writeFile(initialAltoFile, prettyPrinter.format(initialAltoXml.toXml))

          val contentFile = debugLocation.resolve("_predicted.txt")
          writeFile(contentFile, fixedPage.content)

          val transformedLabelled: Mat = toRGB(transformed.clone())
          page.draw(transformedLabelled)
          saveImage(transformedLabelled, debugLocation.resolve("_transformed_seg.png"))

          val labelled: Mat = toRGB(mat.clone())
          rotatedPage.draw(labelled)
          saveImage(labelled, debugLocation.resolve("_seg.png"))
        }

        fixedPage
      }
    } yield alto
  }
}