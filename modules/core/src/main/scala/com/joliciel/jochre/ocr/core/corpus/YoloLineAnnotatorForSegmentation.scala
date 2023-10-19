package com.joliciel.jochre.ocr.core.corpus

import com.joliciel.jochre.ocr.core.model.Page
import com.joliciel.jochre.ocr.core.utils.FileUtils
import com.typesafe.config.ConfigFactory
import enumeratum._
import org.bytedeco.opencv.opencv_core.Mat
import org.rogach.scallop.{ScallopConf, ScallopOption}
import org.slf4j.LoggerFactory

import java.awt.{Color, Polygon}
import java.awt.image.BufferedImage
import java.io.{File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import javax.imageio.ImageIO
import scala.util.Using

case class YoloLineAnnotatorForSegmentation(
  corpusDir: Path,
  outDir: Path,
  debugDir: Option[Path] = None,
  keepStructure: Boolean = false,
  maxFiles: Option[Int] = None,
  extension: String = "png",
  fileList: Option[Set[String]] = None,
  yamlFile: Option[Path] = None,
  altoFinder: AltoFinder = AltoFinder.default) extends CorpusAnnotator {

  import YoloLineAnnotatorForSegmentation._

  private val config = ConfigFactory.load().getConfig("jochre.ocr.yolo")
  private val log = LoggerFactory.getLogger(getClass)

  private val lineThickness = config.getInt("line-thickness")
  private val baseLineHeightInPixels: Double = lineThickness.toDouble

  private def df(d: Double): String = String.format("%.6f", d)
  private def pad(i: Int): String = String.format("%-2s", i)

  yamlFile.foreach{ yamlFile =>
    Using(new OutputStreamWriter(new FileOutputStream(yamlFile.toFile), StandardCharsets.UTF_8)) { writer =>
      writer.write("path: /some/path # dataset root dir\n")
      writer.write("train: images/train # train images (relative to 'path')\n")
      writer.write("val: images/val # validation images (relative to 'path')\n")
      writer.write("test:  # test images (relative to 'path') - optional\n")
      writer.write("# Classes\n")
      writer.write("names:\n")
      YoloBaseLineClass.values.foreach{ yoloClass =>
        writer.write(f"  ${YoloBaseLineClass.indexOf(yoloClass)}: ${yoloClass.entryName}\n")
      }
      writer.flush()
    }
  }

  override def annotateOneFile(mat: Mat, alto: Page, parentDir: File, baseName: String): Unit = {
    // YOLO format: see https://docs.ultralytics.com/datasets/segment/
    // Labels for this format should be exported to YOLO format with one *.txt file per image.
    // If there are no objects in an image, no *.txt file is required.
    // The *.txt file should be formatted with one row per object in class x_center y_center width height format.
    // Box coordinates must be in normalized xywh format (from 0 to 1).
    // If your boxes are in pixels, you should divide x_center and width by image width, and y_center and height by image height.
    // Class numbers should be zero-indexed (start with 0).
    val width = alto.width.toDouble
    val height = alto.height.toDouble

    val yoloSegments = alto.allTextBoxes.flatMap{ textBlock =>
      textBlock.textLinesWithRectangles.zipWithIndex.flatMap{ case ((textLine, textLineRectangle), i) =>
        val yoloClass = if (i==textBlock.textLines.length-1) { YoloBaseLineClass.FinalBaseLine } else { YoloBaseLineClass.BaseLine }

        val baseLineY = ((textLine.baseLine.y1 + textLine.baseLine.y2).toDouble / 2.0)
        val baseLineSegment = YoloSegment(yoloClass, Seq(
          YoloPoint(textLine.baseLine.x1.toDouble / width, (textLine.baseLine.y1.toDouble - (baseLineHeightInPixels / 2.0)) / height),
          YoloPoint(textLine.baseLine.x1.toDouble / width, (textLine.baseLine.y1.toDouble + (baseLineHeightInPixels / 2.0)) / height),
          YoloPoint(textLine.baseLine.x2.toDouble / width, (textLine.baseLine.y2.toDouble + (baseLineHeightInPixels / 2.0)) / height),
          YoloPoint(textLine.baseLine.x2.toDouble / width, (textLine.baseLine.y2.toDouble - (baseLineHeightInPixels / 2.0)) / height),
        ))

        val wordSeperatorSegments = textLine.spaces.map{ space =>
          val xCenter = space.rectangle.left.toDouble + (space.rectangle.width.toDouble / 2.0)
          val yCenter = (textLineRectangle.top.toDouble + baseLineY) / 2.0
          val spaceHeight = (baseLineY - textLineRectangle.top.toDouble) * 0.85
          val spaceWidth = space.rectangle.width.toDouble * 0.90
          YoloSegment(YoloBaseLineClass.WordSeparator, Seq(
            YoloPoint((xCenter - (spaceWidth / 2.0)) / width, (yCenter - (spaceHeight / 2.0)) / height),
            YoloPoint((xCenter - (spaceWidth / 2.0)) / width, (yCenter + (spaceHeight / 2.0)) / height),
            YoloPoint((xCenter + (spaceWidth / 2.0)) / width, (yCenter + (spaceHeight / 2.0)) / height),
            YoloPoint((xCenter + (spaceWidth / 2.0)) / width, (yCenter - (spaceHeight / 2.0)) / height),
          ))
        }

        val letterSeparatorSegments = textLine.words.flatMap { word =>
          word.glyphs.sorted.zipWithIndex.flatMap { case (glyph, i) =>
            Option.when(i > 0) {
              val xCenter = glyph.rectangle.left.toDouble
              val yCenter = (textLineRectangle.top.toDouble + baseLineY) / 2.0
              val separatorHeight = (baseLineY - textLineRectangle.top.toDouble) * 0.85
              val separatorWidth = baseLineHeightInPixels
              YoloSegment(YoloBaseLineClass.LetterSeparator, Seq(
                YoloPoint((xCenter - (separatorWidth / 2.0)) / width, (yCenter - (separatorHeight / 2.0)) / height),
                YoloPoint((xCenter - (separatorWidth / 2.0)) / width, (yCenter + (separatorHeight / 2.0)) / height),
                YoloPoint((xCenter + (separatorWidth / 2.0)) / width, (yCenter + (separatorHeight / 2.0)) / height),
                YoloPoint((xCenter + (separatorWidth / 2.0)) / width, (yCenter - (separatorHeight / 2.0)) / height),
              ))
            }
          }
        }
        Seq(baseLineSegment) ++ wordSeperatorSegments ++ letterSeparatorSegments
      }
    }

    val imageFileName = f"${baseName}.${extension}"
    val imageFile = new File(parentDir, imageFileName)
    saveImage(mat, imageFile.getPath)

    debugDir.foreach{ debugDir =>
      val imageFileName = f"${baseName}-annotation.${extension}"
      val imageFile = new File(debugDir.toFile, imageFileName)
      log.debug(f"Writing debug image to $imageFile")
      val image = new BufferedImage(width.toInt, height.toInt, BufferedImage.TYPE_INT_RGB)
      val graphics = image.createGraphics

      val originalImage = toBufferedImage(mat)
      graphics.drawImage(originalImage, 0, 0, null)
      yoloSegments.foreach {
        case YoloSegment(yoloClass, points) =>
          val color = yoloClass match {
            case YoloBaseLineClass.BaseLine => Color.BLUE
            case YoloBaseLineClass.FinalBaseLine => Color.RED
            case YoloBaseLineClass.WordSeparator => Color.YELLOW
            case YoloBaseLineClass.LetterSeparator => Color.MAGENTA
          }
          graphics.setColor(color)
          val polygon = new Polygon(points.map(point => (point.x * width).toInt).toArray, points.map(point => (point.y * height).toInt).toArray, points.size)
          log.info(f"Color: ${color}. Polygon: ${polygon.xpoints.zip(polygon.ypoints).map{ case (x,y) => f"($x, $y)"}.mkString(", ")}")
          graphics.fillPolygon(polygon)
      }
      graphics.dispose()
      ImageIO.write(image, extension, imageFile)
    }

    val yoloFileName = f"${baseName}.txt"
    val yoloFile = new File(parentDir, yoloFileName)

    Using(new OutputStreamWriter(new FileOutputStream(yoloFile), StandardCharsets.UTF_8)) { writer =>
      yoloSegments.foreach{ segment =>
        val segmentString = segment.points
          .map(point => f"${df(point.x)} ${df(point.y)}")
          .mkString(" ")
        writer.write(f"${pad(YoloBaseLineClass.indexOf(segment.yoloClass))} ${segmentString}\n")
        writer.flush()
      }
    }
  }

  override def cleanUp(): Unit = {
    // Nothing to clean up
  }
}

object YoloLineAnnotatorForSegmentation {
  sealed trait YoloBaseLineClass extends EnumEntry

  object YoloBaseLineClass extends Enum[YoloBaseLineClass] {
    val values = findValues

    case object BaseLine extends YoloBaseLineClass
    case object FinalBaseLine extends YoloBaseLineClass
    case object WordSeparator extends YoloBaseLineClass
    case object LetterSeparator extends YoloBaseLineClass
  }

  case class YoloPoint(x: Double, y: Double)
  case class YoloSegment(yoloClass: YoloBaseLineClass, points: Seq[YoloPoint])

  class YoloLineAnnotatorForSegmentationCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val corpusDir: ScallopOption[String] = opt[String](required = true, descr = "The directory containing original images and labels in Alto4 format")
    val outDir: ScallopOption[String] = opt[String](required = true, descr = "The directory where the processed images will be placed")
    val debugDir: ScallopOption[String] = opt[String](required = false, descr = "A directory where to write debug images")
    val keepStructure: ScallopOption[Boolean] = opt[Boolean](descr = "If present, keep the sub-directory structure within the out-dir")
    val maxFiles = opt[Int](descr = "If present, only transform this many files at most")
    val extension: ScallopOption[String] = choice(Seq("png", "jpg"), default = Some("png"))
    val fileList: ScallopOption[String] = opt[String](required = false, descr = "If present, limit the files to this list only")
    val yamlFile: ScallopOption[String] = opt[String](required = false, descr = "If present, the file to which we write the YOLO YAML configuration")
    verify()
  }

  def execute(args: Array[String], altoFinder: AltoFinder = AltoFinder.default): Unit = {
    val options = new YoloLineAnnotatorForSegmentationCLI(args.toIndexedSeq)

    val corpusDir = new File(options.corpusDir())
    val corpusPath = corpusDir.toPath
    val outDir = new File(options.outDir())
    outDir.mkdirs()
    val outPath = outDir.toPath
    val debugDir = options.debugDir.toOption.map{
      debugDir =>
        val debugDirFile = new File(debugDir)
        debugDirFile.mkdirs()
        debugDirFile.toPath
    }

    val fileList = options.fileList.toOption.map(FileUtils.readFile(_).toSet)
    val extension = options.extension()
    val yamlFile = options.yamlFile.toOption.map{ yamlFilePath =>
      val yamlFile = new File(yamlFilePath)
      yamlFile.getParentFile.mkdirs()
      yamlFile.toPath
    }

    val yoloBaseLineAnnotator = YoloLineAnnotatorForSegmentation(corpusPath, outPath, debugDir, options.keepStructure(), options.maxFiles.toOption, extension, fileList,
      yamlFile,
      altoFinder)
    yoloBaseLineAnnotator.annotate()
  }

  def main(args: Array[String]): Unit = {
    execute(args)
  }
}