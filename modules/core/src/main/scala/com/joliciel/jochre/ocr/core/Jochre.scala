package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.alto.AltoTransformer
import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.model.{ComposedBlock, Page, TextBlock}
import com.joliciel.jochre.ocr.core.output.Alto4Writer
import com.joliciel.jochre.ocr.core.pdf.PDFToImageConverter
import com.joliciel.jochre.ocr.core.segmentation.SegmenterService
import com.joliciel.jochre.ocr.core.text.TextGuesserService
import com.joliciel.jochre.ocr.core.transform.{Deskewer, GrayscaleTransform, Scale, SkewAngle}
import com.joliciel.jochre.ocr.core.utils.{FileUtils, ImageUtils, OutputLocation, XmlImplicits}
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import zio.stream.{ZSink, ZStream}
import zio.{Task, ZIO}

import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.{Files, Path}
import scala.xml.{Elem, PrettyPrinter}

trait Jochre {
  def getImageFilesFromDir(inputDir: Path, maxImages: Option[Int] = None): Seq[(File, Mat)]
  def processPdf(pdfFile: Path, fileName: Option[String] = None, outputDir: Option[Path] = None, debugDir: Option[Path] = None, startPage: Option[Int] = None, endPage: Option[Int] = None, dpi: Option[Int] = None, testRectangle: Option[Rectangle] = None): Task[Elem]
  def processDirectory(inputDir: Path, outputDir: Option[Path] = None, debugDir: Option[Path] = None, maxImages: Option[Int] = None, testRectangle: Option[Rectangle] = None): Task[Seq[Elem]]
  def processImage(bufferedImage: BufferedImage, fileName: String, outputDir: Option[Path] = None, debugDir: Option[Path] = None, testRectangle: Option[Rectangle] = None): Task[Elem]
  def processMat(mat: Mat, fileName: String, outputDir: Option[Path] = None, debugDir: Option[Path] = None, testRectangle: Option[Rectangle] = None): Task[Elem]
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

  override def processPdf(pdfFile: Path, fileName: Option[String] = None, outputDir: Option[Path], debugDir: Option[Path], startPage: Option[Int] = None, endPage: Option[Int] = None, dpi: Option[Int] = None, testRectangle: Option[Rectangle]): Task[Elem] = {
    val pdfFileName = fileName.getOrElse(pdfFile.toFile.getName)
    log.debug(f"Processing file: ${pdfFileName}")
    val baseName = FileUtils.removeFileExtension(pdfFileName)
    val converter = PDFToImageConverter(() => Files.newInputStream(pdfFile), startPage, endPage, dpi)
    for {
      pages <- converter.process((image: BufferedImage, i: Int) => {
        val imageFileName = f"${baseName}_${i}%04d.png"
        processImageInternal(image, imageFileName, i, debugDir, testRectangle)
      }).run {
        ZSink.collectAll
      }
      alto <- ZIO.attempt{
        val altoWriter = Alto4Writer(pages, pdfFileName)
        val alto = altoWriter.alto

        val baseName = FileUtils.removeFileExtension(pdfFileName)
        val outputLocation = outputDir.map(OutputLocation(_, baseName))
        outputLocation.foreach { outputLocation =>
          val prettyPrinter = new PrettyPrinter(80, 2)
          val altoFile = outputLocation.resolve("_alto4.xml")
          writeFile(altoFile, prettyPrinter.format(alto))
        }

        alto
      }
    } yield alto
  }

  override def processDirectory(inputDir: Path, outputDir: Option[Path], debugDir: Option[Path], maxImages: Option[Int], testRectangle: Option[Rectangle] = None): Task[Seq[Elem]] = {
    val filesAndImages = getImageFilesFromDir(inputDir, maxImages)

    ZIO.foreach(filesAndImages.zipWithIndex)
      { case ((inputFile, mat), i) =>
        log.debug(f"Processing file $i: ${inputFile.getPath}")
        this.processMat(mat, inputFile.getName, outputDir, debugDir, testRectangle)
      }
  }

  override def processImage(bufferedImage: BufferedImage, fileName: String, outputDir: Option[Path] = None, debugDir: Option[Path] = None, testRectangle: Option[Rectangle] = None): Task[Elem] = {
    val mat = fromBufferedImage(bufferedImage)
    this.processMat(mat, fileName, outputDir, debugDir, testRectangle)
  }

  override def processMat(mat: Mat, fileName: String, outputDir: Option[Path] = None, debugDir: Option[Path] = None, testRectangle: Option[Rectangle] = None): Task[Elem] = {
    for {
      page <- processMatInternal(mat, fileName, 1, debugDir, testRectangle)
      alto <- ZIO.attempt {
        val altoWriter = Alto4Writer(Seq(page), fileName)
        val alto = altoWriter.alto

        val baseName = FileUtils.removeFileExtension(fileName)
        val outputLocation = outputDir.map(OutputLocation(_, baseName))
        outputLocation.foreach { outputLocation =>
          val prettyPrinter = new PrettyPrinter(80, 2)
          val altoFile = outputLocation.resolve("_alto4.xml")
          writeFile(altoFile, prettyPrinter.format(alto))
        }
        alto
      }
    } yield alto
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
          saveImage(transformed, transformedFile.getAbsolutePath)
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
          physicalPageNumber = physicalPageNumber,
        )

        val rotatedPage = page.rescale(1.0 / scale).rotate()

        val fixedPage = altoTransformer.process(rotatedPage)

        debugLocation.foreach { debugLocation =>
          val prettyPrinter = new PrettyPrinter(80, 2)

          val initialAltoWriter = Alto4Writer(Seq(rotatedPage), fileName)
          val initialAlto = initialAltoWriter.alto

          val initialAltoFile = debugLocation.resolve("_initial_alto4.xml")
          writeFile(initialAltoFile, prettyPrinter.format(initialAlto))

          val contentFile = debugLocation.resolve("_predicted.txt")
          writeFile(contentFile, fixedPage.content)

          val transformedLabelled: Mat = toRGB(transformed.clone())
          page.draw(transformedLabelled)
          saveImage(transformedLabelled, debugLocation.resolve("_transformed_seg.png").toString)

          val labelled: Mat = toRGB(mat.clone())
          rotatedPage.draw(labelled)
          saveImage(labelled, debugLocation.resolve("_seg.png").toString)
        }

        fixedPage
      }
    } yield alto
  }
}