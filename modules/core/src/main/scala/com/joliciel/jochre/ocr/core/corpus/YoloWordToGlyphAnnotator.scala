package com.joliciel.jochre.ocr.core.corpus

import com.joliciel.jochre.ocr.core.model.Alto
import com.joliciel.jochre.ocr.core.utils.{FileUtils, ImageUtils}
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.opencv_core.Mat
import org.rogach.scallop._
import org.slf4j.LoggerFactory

import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, Color, Polygon}
import java.io.{File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import javax.imageio.ImageIO
import scala.util.Using

/** Annotates glyphs inside words, for training a word-to-glyph splitter.
  */
case class YoloWordToGlyphAnnotator(
    corpusDir: Path,
    outDir: Path,
    debugDir: Option[String] = None,
    maxFiles: Option[Int] = None,
    extension: String = "png",
    fileList: Option[Set[String]] = None,
    yamlFileName: Option[String] = None,
    validationOneEvery: Option[Int] = None,
    targetWidth: Int = 128,
    targetHeight: Int = 32,
    altoFinder: AltoFinder = AltoFinder.default
) extends CorpusAnnotator
    with ImageUtils {

  import YoloAnnotator.*

  private val config = ConfigFactory.load().getConfig("jochre.ocr.yolo")
  private val log = LoggerFactory.getLogger(getClass)

  private val lineThickness = config.getDouble("line-thickness-normalized")
  private val cropToPrintArea = config.getBoolean("crop-to-print-area")
  private val cropMargin = config.getDouble("crop-margin")
  private val tileMargin = config.getDouble("tile-margin")
  private val textBlockHorizontalMargin =
    config.getDouble("text-block-horizontal-margin")
  private val textBlockVerticalMargin =
    config.getDouble("text-block-vertical-margin")

  private def df(d: Double): String = String.format("%.6f", d)
  private def pad(i: Int): String = String.format("%-2s", i)

  yamlFileName.foreach { yamlFileName =>
    val yamlFile = new File(outDir.toFile, yamlFileName)
    Using(
      new OutputStreamWriter(
        new FileOutputStream(yamlFile),
        StandardCharsets.UTF_8
      )
    ) { writer =>
      writer.write(f"path: ${outDir.toFile.getName} # dataset root dir\n")
      writer.write("train: images/train # train images (relative to 'path')\n")
      writer.write("val: images/val # validation images (relative to 'path')\n")
      writer.write("test:  # test images (relative to 'path') - optional\n")
      writer.write("# Classes\n")
      writer.write("names:\n")
      writer.write("  0: Glyph")
      writer.flush()
    }
  }

  override def annotateOneFile(
      mat: Mat,
      alto: Alto,
      parentDir: File,
      baseName: String,
      index: Int
  ): Unit = {
    // YOLO format: see https://docs.ultralytics.com/datasets/segment/
    // Labels for this format should be exported to YOLO format with one *.txt file per image.
    // If there are no objects in an image, no *.txt file is required.
    // The *.txt file should be formatted with one row per object in class x_center y_center width height format.
    // Box coordinates must be in normalized xywh format (from 0 to 1).
    // If your boxes are in pixels, you should divide x_center and width by image width, and y_center and height by image height.
    // Class numbers should be zero-indexed (start with 0).

    val page = alto.pages.head

    val trainOrVal = validationOneEvery
      .map { validationOneEvery =>
        if ((index + 1) % validationOneEvery == 0) {
          "val"
        } else {
          "train"
        }
      }
      .getOrElse("train")

    // WordToGlyph objects are relative to the word in which the glyphs are contained
    page.combinedWords.zipWithIndex.map { case (word, i) =>
      val wordLeft = word.rectangle.left.toDouble
      val wordTop = word.rectangle.top.toDouble
      val wordWidth = word.rectangle.width.toDouble
      val wordHeight = word.rectangle.height.toDouble

      val glyphBoxes = word.glyphs
        .map { glyph =>
          YoloBox(
            YoloObjectType.Glyph,
            xCenter = (glyph.rectangle.xCenter.toDouble - wordLeft) / wordWidth,
            yCenter = (glyph.rectangle.yCenter.toDouble - wordTop) / wordHeight,
            width = glyph.rectangle.width.toDouble / wordWidth,
            height = glyph.rectangle.height.toDouble / wordHeight
          )
        }
        .map {
          case box if box.height > 1.0 => box.copy(height = 1.0)
          case box                     => box
        }
        .map {
          case box if box.width > 1.0 => box.copy(width = 1.0)
          case box                    => box
        }

      val wordMat = crop(mat, word.rectangle)

      debugDir.foreach { debugDir =>
        val imageFileName = f"$baseName-$i%04d-annotation.$extension"
        val debugParentDir = new File(outDir.toFile, debugDir)
        debugParentDir.mkdirs()
        val imageFile = new File(debugParentDir, imageFileName)
        log.debug(f"Writing debug image to $imageFile")
        val image = new BufferedImage(
          wordMat.cols(),
          wordMat.rows(),
          BufferedImage.TYPE_INT_RGB
        )
        val graphics = image.createGraphics

        val originalImage = toBufferedImage(wordMat)
        graphics.drawImage(originalImage, 0, 0, null)
        graphics.setComposite(AlphaComposite.SrcOver.derive(0.5f))
        glyphBoxes.zipWithIndex.foreach { case (YoloBox(yoloClass, xCenter, yCenter, boxWidth, boxHeight), i) =>
          val colors = (Color.YELLOW, Color.ORANGE)
          val color = if (i % 2 == 0) {
            colors._1
          } else {
            colors._2
          }
          graphics.setColor(color)
          val xPoints = Array(
            xCenter - boxWidth / 2.0,
            xCenter - boxWidth / 2.0,
            xCenter + boxWidth / 2.0,
            xCenter + boxWidth / 2.0
          ).map(_ * word.width.toDouble).map(_.toInt)
          val yPoints = Array(
            yCenter - boxHeight / 2.0,
            yCenter + boxHeight / 2.0,
            yCenter + boxHeight / 2.0,
            yCenter - boxHeight / 2.0
          ).map(_ * word.height.toDouble).map(_.toInt)
          val polygon = new Polygon(xPoints, yPoints, xPoints.length)
          log.debug(f"Color: $color. Polygon: ${polygon.xpoints
            .zip(polygon.ypoints)
            .map { case (x, y) => f"($x, $y)" }
            .mkString(", ")}")
          graphics.fillPolygon(polygon)
        }
        graphics.dispose()

        val widthRatio = word.width.toDouble / targetWidth.toDouble
        val heightRatio = word.height.toDouble / targetHeight.toDouble

        val scaledImage = new BufferedImage(targetWidth, targetHeight, image.getType)
        val scaledGraphics = scaledImage.createGraphics
        val scaledWidth = if (heightRatio > widthRatio) {
          word.width.toDouble / heightRatio
        } else {
          targetWidth.toDouble
        }
        val scaledHeight = if (heightRatio > widthRatio) {
          targetHeight.toDouble
        } else {
          word.height.toDouble / widthRatio
        }
        scaledGraphics.drawImage(
          image,
          ((targetWidth - scaledWidth) / 2.0).toInt,
          ((targetHeight - scaledHeight) / 2.0).toInt,
          scaledWidth.toInt,
          scaledHeight.toInt,
          null
        )
        scaledGraphics.dispose()

        ImageIO.write(scaledImage, extension, imageFile)
      }

      val imageFileName = f"${baseName}_$i%04d.$extension"
      val imageFile = new File(parentDir, f"images/$trainOrVal/$imageFileName")
      imageFile.getParentFile.mkdirs()
      log.info(f"Saving image to ${imageFile.getPath}")
      saveImage(wordMat, imageFile.toPath)

      val labelFileName = f"${baseName}_$i%04d.txt"
      val labelFile = new File(parentDir, f"labels/$trainOrVal/$labelFileName")
      labelFile.getParentFile.mkdirs()

      log.info(f"Saving label to ${labelFile.getPath}")

      Using(
        new OutputStreamWriter(
          new FileOutputStream(labelFile),
          StandardCharsets.UTF_8
        )
      ) { writer =>
        glyphBoxes.foreach { case YoloBox(yoloClass, xCenter, yCenter, boxWidth, boxHeight) =>
          val yoloBoxString = f"${df(xCenter)} ${df(yCenter)} ${df(boxWidth)} ${df(boxHeight)}"

          writer.write(
            f"${pad(0)} $yoloBoxString\n"
          )
          writer.flush()
        }
      }
    }
  }

  override def cleanUp(): Unit = {
    // Nothing to clean up
  }
}

object YoloWordToGlyphAnnotator {

  private class CLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val corpusDir: ScallopOption[String] = opt[String](
      required = true,
      descr = "The directory containing original images and labels in Alto4 format"
    )
    val outDir: ScallopOption[String] = opt[String](
      required = true,
      descr = "The directory where the processed images will be placed"
    )
    val debugDir: ScallopOption[String] = opt[String](
      required = false,
      descr = "A directory where to write debug images, relative to the out-dir"
    )
    val maxFiles: ScallopOption[Int] =
      opt[Int](descr = "If present, only transform this many files at most")
    val extension: ScallopOption[String] =
      choice(Seq("png", "jpg"), default = Some("png"))
    val fileList: ScallopOption[String] = opt[String](
      required = false,
      descr = "If present, limit the files to this list only"
    )
    val yamlFile: ScallopOption[String] = opt[String](
      required = false,
      descr = "If present, the file to which we write the YOLO YAML configuration, placed in the out-dir"
    )
    val validationOneEvery: ScallopOption[Int] = opt[Int](
      required = false,
      descr = "If present, add train/val sub-directories and mark one out of every n files for validation"
    )
    val targetWidth: ScallopOption[Int] = opt[Int](
      default = Some(128),
      descr = "The target width, must be a multiple of 32. Default is 4*32=128."
    )
    val targetHeight: ScallopOption[Int] = opt[Int](
      default = Some(32),
      descr = "The target width, must be a multiple of 32. Default is 1*32=32."
    )
    verify()
  }

  def execute(
      args: Array[String],
      altoFinder: AltoFinder = AltoFinder.default
  ): Unit = {
    val options = new CLI(args.toIndexedSeq)

    val corpusDir = new File(options.corpusDir())
    val corpusPath = corpusDir.toPath
    val outDir = new File(options.outDir())
    outDir.mkdirs()
    val outPath = outDir.toPath
    val debugDir = options.debugDir.toOption

    val fileList = options.fileList.toOption.map(FileUtils.readFile(_).toSet)
    val extension = options.extension()
    val yamlFile = options.yamlFile.toOption

    val validationOneEvery = options.validationOneEvery.toOption
    val targetWidth = options.targetWidth()
    val targetHeight = options.targetHeight()

    val yoloAnnotator = YoloWordToGlyphAnnotator(
      corpusPath,
      outPath,
      debugDir,
      options.maxFiles.toOption,
      extension,
      fileList,
      yamlFile,
      validationOneEvery,
      targetWidth,
      targetHeight,
      altoFinder
    )
    yoloAnnotator.annotate()
  }

  def main(args: Array[String]): Unit = {
    execute(args)
  }
}
