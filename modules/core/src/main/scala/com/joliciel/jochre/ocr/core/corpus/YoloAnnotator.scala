package com.joliciel.jochre.ocr.core.corpus

import com.joliciel.jochre.ocr.core.corpus.YoloAnnotator.YoloTask
import com.joliciel.jochre.ocr.core.graphics.{Rectangle, WithRectangle}
import com.joliciel.jochre.ocr.core.model.{Alto, ComposedBlock, Illustration, TextBlock}
import com.joliciel.jochre.ocr.core.utils.{FileUtils, ImageUtils}
import com.typesafe.config.ConfigFactory
import enumeratum._
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

case class YoloAnnotator(
    corpusDir: Path,
    outDir: Path,
    debugDir: Option[String] = None,
    maxFiles: Option[Int] = None,
    extension: String = "png",
    fileList: Option[Set[String]] = None,
    task: YoloTask,
    objectsToInclude: Seq[YoloObjectType],
    yamlFileName: Option[String] = None,
    validationOneEvery: Option[Int] = None,
    tileCount: Option[Int] = None,
    imageSize: Int = 1280,
    altoFinder: AltoFinder = AltoFinder.default
) extends CorpusAnnotator
    with ImageUtils {

  import YoloAnnotator._

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
      objectsToInclude.zipWithIndex.foreach { case (yoloObjectType, i) =>
        writer.write(f"  $i: ${yoloObjectType.entryName}\n")
      }
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

    val objectTypeSet = objectsToInclude.toSet

    val (baseAlto, baseMat) =
      if (
        cropToPrintArea && !(objectTypeSet.contains(
          YoloObjectType.TopLevelTextBlock
        ) || objectTypeSet.contains(YoloObjectType.Illustration))
      ) {
        val cropRectangle = page.croppedPrintArea(cropMargin)
        val croppedAlto = page.crop(cropRectangle)
        val croppedMat = crop(mat, cropRectangle)
        croppedAlto -> croppedMat
      } else {
        page -> mat
      }

    val width = baseAlto.width.toDouble
    val height = baseAlto.height.toDouble

    // Non-blocks are based on the page cropped down to the print area
    val yoloNonBlocks = baseAlto.allTextBlocks.flatMap { textBlock =>
      textBlock.textLinesWithRectangles.zipWithIndex.flatMap { case ((textLine, textLineRectangle), i) =>
        val baseLineType = if (i == textBlock.textLines.length - 1) {
          YoloObjectType.FinalBaseLine
        } else {
          YoloObjectType.NonFinalBaseLine
        }

        val baseLineY =
          (textLine.baseLine.y1 + textLine.baseLine.y2).toDouble / 2.0
        val baseLineHeightInPixels = lineThickness * height

        val baseLineBox = YoloBox(
          YoloObjectType.BaseLine,
          xCenter = ((textLine.baseLine.x1 + textLine.baseLine.x2).toDouble / 2.0) / width,
          yCenter = baseLineY / height,
          width = textLine.baseLine.width.toDouble / width,
          height = baseLineHeightInPixels / height
        )

        val baseLineBoxTyped = baseLineBox.copy(yoloClass = baseLineType)

        val wordBoxes = textLine.combinedWords.map { word =>
          YoloBox(
            YoloObjectType.Word,
            xCenter = word.rectangle.xCenter.toDouble / width,
            yCenter = word.rectangle.yCenter.toDouble / height,
            width = word.rectangle.width.toDouble / width,
            height = word.rectangle.height.toDouble / height
          )
        }

        val glyphBoxes = textLine.combinedWords.flatMap { word =>
          word.glyphs.map { glyph =>
            YoloBox(
              YoloObjectType.Glyph,
              xCenter = glyph.rectangle.xCenter.toDouble / width,
              yCenter = glyph.rectangle.yCenter.toDouble / height,
              width = glyph.rectangle.width.toDouble / width,
              height = glyph.rectangle.height.toDouble / height
            )
          }
        }

        val wordSeparatorBoxes = textLine.spaces.map { space =>
          YoloBox(
            YoloObjectType.WordSeparator,
            xCenter = (space.rectangle.left.toDouble + (space.rectangle.width.toDouble / 2.0)) / width,
            yCenter = ((textLineRectangle.top.toDouble + baseLineY) / 2.0) / height,
            width = space.rectangle.width.toDouble / width,
            height = ((baseLineY - textLineRectangle.top.toDouble) * 0.85) / height
          )
        }

        val letterSeparatorBoxes = textLine.combinedWords.flatMap { word =>
          word.glyphs
            .sorted(WithRectangle.HorizontalOrdering(word.isLeftToRight))
            .zipWithIndex
            .flatMap { case (glyph, i) =>
              Option.when(i > 0) {
                val xCenter = glyph.rectangle.left.toDouble
                val yCenter =
                  (textLineRectangle.top.toDouble + baseLineY) / 2.0
                val separatorHeight =
                  ((baseLineY - textLineRectangle.top.toDouble) * 0.85).toInt
                val separatorWidth = (lineThickness * width).toInt
                YoloBox(
                  YoloObjectType.GlyphSeparator,
                  xCenter,
                  yCenter,
                  separatorWidth,
                  separatorHeight
                )
              }
            }
        }
        val allBoxes = Seq(
          baseLineBox,
          baseLineBoxTyped
        ) ++ wordBoxes ++ glyphBoxes ++ wordSeparatorBoxes ++ letterSeparatorBoxes
        allBoxes.filter(box => objectTypeSet.contains(box.yoloClass))
      }
    }

    // Text blocks will be used to separate bigger blocks into paragraphs - so we already know the print area.
    val yoloTextBlocks = baseAlto.allTextBlocks
      .map { textBlock =>
        YoloBox(
          YoloObjectType.TextBlock,
          xCenter = textBlock.rectangle.xCenter.toDouble / width,
          yCenter = textBlock.rectangle.yCenter.toDouble / height,
          width = textBlock.rectangle.width.toDouble / width,
          height = textBlock.rectangle.height.toDouble / height
        )
      }
      .filter(box => objectTypeSet.contains(box.yoloClass))

    // Top-level blocks and illustrations are based on the uncropped page, since we don't yet know the print area
    val yoloTopLevelBlocks = page.blocks
      .map {
        case composedBlock: ComposedBlock =>
          YoloBox(
            YoloObjectType.TopLevelTextBlock,
            xCenter = composedBlock.rectangle.xCenter.toDouble / width,
            yCenter = composedBlock.rectangle.yCenter.toDouble / height,
            width = composedBlock.rectangle.width.toDouble / width + textBlockHorizontalMargin * 2.0,
            height = composedBlock.rectangle.height.toDouble / height + textBlockVerticalMargin * 2.0
          )
        case textBlock: TextBlock =>
          YoloBox(
            YoloObjectType.TopLevelTextBlock,
            xCenter = textBlock.rectangle.xCenter.toDouble / width,
            yCenter = textBlock.rectangle.yCenter.toDouble / height,
            width = textBlock.rectangle.width.toDouble / width + textBlockHorizontalMargin * 2.0,
            height = textBlock.rectangle.height.toDouble / height + textBlockVerticalMargin * 2.0
          )
        case illustration: Illustration =>
          YoloBox(
            YoloObjectType.Illustration,
            xCenter = illustration.rectangle.xCenter.toDouble / width,
            yCenter = illustration.rectangle.yCenter.toDouble / height,
            width = illustration.rectangle.width.toDouble / width,
            height = illustration.rectangle.height.toDouble / height
          )
      }
      .filter(box => objectTypeSet.contains(box.yoloClass))

    val yoloBoxes = yoloNonBlocks ++ yoloTextBlocks ++ yoloTopLevelBlocks

    val trainOrVal = validationOneEvery
      .map { validationOneEvery =>
        if ((index + 1) % validationOneEvery == 0) {
          "val"
        } else {
          "train"
        }
      }
      .getOrElse("train")

    val tiles: Seq[Rectangle] = tileCount
      .map { tileCount =>
        baseAlto.rectangle.tile(tileCount, tileCount, tileMargin)
      }
      .getOrElse(Seq(baseAlto.rectangle))

    tiles.zipWithIndex.foreach { case (tile, i) =>
      val tileLeft = tile.left.toDouble / width
      val tileTop = tile.top.toDouble / height
      val tileRight = tile.right.toDouble / width
      val tileBottom = tile.bottom.toDouble / height

      val tileBoxes = yoloBoxes
        .filter { case YoloBox(_, xCenter, yCenter, boxWidth, boxHeight) =>
          val boxLeft = xCenter - boxWidth / 2
          val boxRight = xCenter + boxWidth / 2
          val boxTop = yCenter - boxHeight / 2
          val boxBottom = yCenter + boxHeight / 2
          boxLeft >= tileLeft && boxRight <= tileRight && boxTop >= tileTop && boxBottom <= tileBottom
        }
        .map { case yoloBox @ YoloBox(_, xCenter, yCenter, boxWidth, boxHeight) =>
          val newXCenter =
            ((xCenter - tileLeft) * width) / tile.width.toDouble
          val newYCenter =
            ((yCenter - tileTop) * height) / tile.height.toDouble
          val newBoxWidth = (boxWidth * width) / tile.width.toDouble
          val newBoxHeight = (boxHeight * height) / tile.height.toDouble
          yoloBox.copy(
            xCenter = newXCenter,
            yCenter = newYCenter,
            width = newBoxWidth,
            height = newBoxHeight
          )
        }

      val tileMat = crop(baseMat, tile)

      debugDir.foreach { debugDir =>
        val imageFileName = f"$baseName-$i-annotation.$extension"
        val debugParentDir = new File(outDir.toFile, debugDir)
        debugParentDir.mkdirs()
        val imageFile = new File(debugParentDir, imageFileName)
        log.debug(f"Writing debug image to $imageFile")
        val image = new BufferedImage(
          tileMat.cols(),
          tileMat.rows(),
          BufferedImage.TYPE_INT_RGB
        )
        val graphics = image.createGraphics

        val originalImage = toBufferedImage(tileMat)
        graphics.drawImage(originalImage, 0, 0, null)
        graphics.setComposite(AlphaComposite.SrcOver.derive(0.5f))
        tileBoxes.zipWithIndex.foreach { case (YoloBox(yoloClass, xCenter, yCenter, boxWidth, boxHeight), i) =>
          val colors = yoloClass match {
            case YoloObjectType.TopLevelTextBlock =>
              (Color.YELLOW, Color.YELLOW)
            case YoloObjectType.Illustration => (Color.BLUE, Color.BLUE)
            case YoloObjectType.TextBlock =>
              (Color.YELLOW, Color.YELLOW)
            case YoloObjectType.BaseLine         => (Color.BLUE, Color.GREEN)
            case YoloObjectType.NonFinalBaseLine => (Color.BLUE, Color.GREEN)
            case YoloObjectType.FinalBaseLine    => (Color.RED, Color.ORANGE)
            case YoloObjectType.Word             => (Color.YELLOW, Color.ORANGE)
            case YoloObjectType.WordSeparator    => (Color.YELLOW, Color.ORANGE)
            case YoloObjectType.Glyph            => (Color.YELLOW, Color.ORANGE)
            case YoloObjectType.GlyphSeparator   => (Color.BLUE, Color.GREEN)
          }
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
          ).map(_ * tile.width.toDouble).map(_.toInt)
          val yPoints = Array(
            yCenter - boxHeight / 2.0,
            yCenter + boxHeight / 2.0,
            yCenter + boxHeight / 2.0,
            yCenter - boxHeight / 2.0
          ).map(_ * tile.height.toDouble).map(_.toInt)
          val polygon = new Polygon(xPoints, yPoints, xPoints.length)
          log.debug(f"Color: $color. Polygon: ${polygon.xpoints
            .zip(polygon.ypoints)
            .map { case (x, y) => f"($x, $y)" }
            .mkString(", ")}")
          graphics.fillPolygon(polygon)
        }
        graphics.dispose()

        val imageSizeDouble = imageSize.toDouble
        val scaledImage = new BufferedImage(imageSize, imageSize, image.getType)
        val scaledGraphics = scaledImage.createGraphics
        val scaledWidth = if (tile.height > tile.width) {
          (tile.width.toDouble / tile.height.toDouble) * imageSizeDouble
        } else {
          imageSizeDouble
        }
        val scaledHeight = if (tile.height > tile.width) {
          imageSizeDouble
        } else {
          (tile.height.toDouble / tile.width.toDouble) * imageSizeDouble
        }
        scaledGraphics.drawImage(
          image,
          ((imageSizeDouble - scaledWidth) / 2.0).toInt,
          ((imageSizeDouble - scaledHeight) / 2.0).toInt,
          scaledWidth.toInt,
          scaledHeight.toInt,
          null
        )
        scaledGraphics.dispose()

        ImageIO.write(scaledImage, extension, imageFile)
      }

      val imageFileName = f"${baseName}_$i.$extension"
      val imageFile = new File(parentDir, f"images/$trainOrVal/$imageFileName")
      imageFile.getParentFile.mkdirs()
      log.info(f"Saving image to ${imageFile.getPath}")
      saveImage(tileMat, imageFile.toPath)

      val labelFileName = f"${baseName}_$i.txt"
      val labelFile = new File(parentDir, f"labels/$trainOrVal/$labelFileName")
      labelFile.getParentFile.mkdirs()

      log.info(f"Saving label to ${labelFile.getPath}")

      val yoloObjectTypeToIndex = objectsToInclude.zipWithIndex.toMap
      Using(
        new OutputStreamWriter(
          new FileOutputStream(labelFile),
          StandardCharsets.UTF_8
        )
      ) { writer =>
        tileBoxes.foreach { case YoloBox(yoloClass, xCenter, yCenter, boxWidth, boxHeight) =>
          val yoloBoxString = task match {
            case YoloTask.Segmentation =>
              val xPoints = Array(
                xCenter - boxWidth / 2.0,
                xCenter - boxWidth / 2.0,
                xCenter + boxWidth / 2.0,
                xCenter + boxWidth / 2.0
              )
              val yPoints = Array(
                yCenter - boxHeight / 2.0,
                yCenter + boxHeight / 2.0,
                yCenter + boxHeight / 2.0,
                yCenter - boxHeight / 2.0
              )
              xPoints
                .zip(yPoints)
                .map { case (x, y) => f"${df(x)} ${df(y)}" }
                .mkString(" ")

            case YoloTask.ObjectDetection =>
              f"${df(xCenter)} ${df(yCenter)} ${df(boxWidth)} ${df(boxHeight)}"

          }
          writer.write(
            f"${pad(yoloObjectTypeToIndex(yoloClass))} $yoloBoxString\n"
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

object YoloAnnotator {
  sealed trait YoloTask extends EnumEntry

  object YoloTask extends Enum[YoloTask] {
    val values: IndexedSeq[YoloTask] = findValues

    case object Segmentation extends YoloTask
    case object ObjectDetection extends YoloTask
  }

  private class YoloLineAnnotatorForSegmentationCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val task: ScallopOption[String] = opt[String](
      required = true,
      descr = s"The annotation task among: ${YoloTask.values.map(_.entryName).mkString(", ")}"
    )
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
    val objectsToInclude: ScallopOption[List[String]] = opt[List[String]](
      required = true,
      descr = f"A comma-separated list from ${YoloObjectType.values.map(_.entryName).mkString(", ")}"
    )
    val imageSize: ScallopOption[Int] = opt[Int](
      default = Some(1280),
      descr = "The image size, must be a multiple of 32. Default is 40*32=1280."
    )
    val tileCount: ScallopOption[Int] = opt[Int](
      required = false,
      descr = "If provided, how many vertical and horizontal tiles will we break the image into."
    )
    verify()
  }

  def execute(
      args: Array[String],
      altoFinder: AltoFinder = AltoFinder.default
  ): Unit = {
    val options = new YoloLineAnnotatorForSegmentationCLI(args.toIndexedSeq)

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
    val tileCount = options.tileCount.toOption
    val imageSize = options.imageSize()

    val objectsToInclude = options.objectsToInclude.toOption
      .map(_.map(obj => YoloObjectType.withName(obj)))
      .get
    val task = options.task.toOption.map(YoloTask.withName).get

    val yoloAnnotator = YoloAnnotator(
      corpusPath,
      outPath,
      debugDir,
      options.maxFiles.toOption,
      extension,
      fileList,
      task,
      objectsToInclude,
      yamlFile,
      validationOneEvery,
      tileCount,
      imageSize,
      altoFinder
    )
    yoloAnnotator.annotate()
  }

  def main(args: Array[String]): Unit = {
    execute(args)
  }
}
