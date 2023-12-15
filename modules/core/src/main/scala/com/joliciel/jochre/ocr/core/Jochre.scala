package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.analysis.AltoTransformer
import com.joliciel.jochre.ocr.core.model.{ComposedBlock, TextBlock}
import com.joliciel.jochre.ocr.core.output.Alto4Writer
import com.joliciel.jochre.ocr.core.segmentation.SegmenterService
import com.joliciel.jochre.ocr.core.text.TextGuesserService
import com.joliciel.jochre.ocr.core.transform.{BoxTransform, Deskewer, GrayscaleTransform, ResizeImageAndKeepAspectRatio, Scale, SkewAngle}
import com.joliciel.jochre.ocr.core.utils.{FileUtils, ImageUtils, OutputLocation, XmlImplicits}
import com.typesafe.config.ConfigFactory
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

trait AbstractJochre extends Jochre with ImageUtils with XmlImplicits {
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

  def process(inputDir: Path, outputDir: Option[Path], maxImages: Option[Int]): Task[Seq[Elem]] = {
    val allFiles = FileUtils.recursiveListImages(inputDir.toFile).map(new File(_))

    val inputFiles = allFiles
      .take(maxImages.getOrElse(allFiles.size))

    ZIO.foreach(inputFiles.zipWithIndex)
      { case (inputFile, i) =>
        log.debug(f"Processing file $i: ${inputFile.getPath}")
        val mat = loadImage(inputFile.getPath)
        this.processImage(mat, outputDir, inputFile.getName)
      }
  }

  def processImage(bufferedImage: BufferedImage, outputDir: Option[Path], fileName: String): Task[Elem] = {
    val mat = fromBufferedImage(bufferedImage)
    this.processImage(mat, outputDir, fileName)
  }

  def processImage(mat: Mat, outputDir: Option[Path], fileName: String): Task[Elem] = {
    log.info(f"Processing image $fileName of size ${mat.cols()}X${mat.rows()}")
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

      segmenter <- segmenterService.getSegmenter()
      segmented <- segmenter.segment(transformed, fileName, outputLocation)
      textGuesser <- textGuesserService.getTextGuesser()
      pageWithContent <- textGuesser.guess(segmented, transformed, fileName, outputLocation)
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

        val fixedAlto = altoTransformer.process(alto, fileName)

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