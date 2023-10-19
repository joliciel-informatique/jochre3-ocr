package com.joliciel.jochre.ocr.core.corpus

import com.joliciel.jochre.ocr.core.model.Page
import com.joliciel.jochre.ocr.core.utils.FileUtils
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.opencv_core.Mat
import enumeratum._
import org.rogach.scallop.{ScallopConf, ScallopOption}

import java.io.{File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.util.Using

case class YoloLineAnnotatorForObjectDetection(
  corpusDir: Path,
  outDir: Path,
  debugDir: Option[Path] = None,
  keepStructure: Boolean = false,
  maxFiles: Option[Int] = None,
  extension: String = "png",
  fileList: Option[Set[String]] = None,
  yamlFile: Option[Path] = None,
  altoFinder: AltoFinder = AltoFinder.default) extends CorpusAnnotator {

  import YoloLineAnnotatorForObjectDetection._

  private val config = ConfigFactory.load().getConfig("jochre.ocr.yolo")

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
      YoloLineClass.values.foreach{ yoloClass =>
        writer.write(f"  ${YoloLineClass.indexOf(yoloClass)}: ${yoloClass.entryName}\n")
      }
      writer.flush()
    }
  }

  override def annotateOneFile(mat: Mat, alto: Page, parentDir: File, baseName: String): Unit = {
    // YOLO format:
    // Labels for this format should be exported to YOLO format with one *.txt file per image.
    // If there are no objects in an image, no *.txt file is required.
    // The *.txt file should be formatted with one row per object in class x_center y_center width height format.
    // Box coordinates must be in normalized xywh format (from 0 to 1).
    // If your boxes are in pixels, you should divide x_center and width by image width, and y_center and height by image height.
    // Class numbers should be zero-indexed (start with 0).
    val width = alto.width.toDouble
    val height = alto.height.toDouble

    val yoloBoxes = alto.allTextBoxes.flatMap{ textBlock =>
      textBlock.textLinesWithRectangles.zipWithIndex.flatMap{ case ((textLine, textLineRectangle), i) =>
        val yoloClass = if (i==textBlock.textLines.length-1) { YoloLineClass.FinalBaseLine } else { YoloLineClass.BaseLine }

        val baseLineY = ((textLine.baseLine.y1 + textLine.baseLine.y2).toDouble / 2.0)
        val baseLineBox = YoloBox(yoloClass,
          xCenter = ((textLine.baseLine.x1 + textLine.baseLine.x2).toDouble / 2.0) / width,
          yCenter = baseLineY / height,
          width = textLine.baseLine.width.toDouble / width,
          height = baseLineHeightInPixels / height)

        val wordSeparatorBoxes = textLine.spaces.map{ space =>
          YoloBox(YoloLineClass.WordSeparator,
            xCenter = (space.rectangle.left.toDouble + (space.rectangle.width.toDouble / 2.0)) / width,
            yCenter = ((textLineRectangle.top.toDouble + baseLineY) / 2.0) / height,
            width = space.rectangle.width.toDouble / width,
            height = ((baseLineY - textLineRectangle.top.toDouble) * 0.85) / height)
        }
        Seq(baseLineBox) ++ wordSeparatorBoxes
      }
    }

    val imageFileName = f"${baseName}.${extension}"
    val imageFile = new File(parentDir, imageFileName)
    saveImage(mat, imageFile.getPath)

    val yoloFileName = f"${baseName}.txt"
    val yoloFile = new File(parentDir, yoloFileName)

    Using(new OutputStreamWriter(new FileOutputStream(yoloFile), StandardCharsets.UTF_8)) { writer =>
      yoloBoxes.foreach{ box =>
        writer.write(f"${pad(YoloLineClass.indexOf(box.yoloClass))} ${df(box.xCenter)} ${df(box.yCenter)} ${df(box.width)} ${df(box.height)}\n")
        writer.flush()
      }
    }
  }

  override def cleanUp(): Unit = {
    // Nothing to clean up
  }
}

object YoloLineAnnotatorForObjectDetection {
  sealed trait YoloLineClass extends EnumEntry

  object YoloLineClass extends Enum[YoloLineClass] {
    val values = findValues

    case object BaseLine extends YoloLineClass
    case object FinalBaseLine extends YoloLineClass
    case object WordSeparator extends YoloLineClass
  }

  case class YoloBox(yoloClass: YoloLineClass, xCenter: Double, yCenter: Double, width: Double, height: Double)

  class YoloLineAnnotatorForObjectDetectionCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
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
    val options = new YoloLineAnnotatorForObjectDetectionCLI(args.toIndexedSeq)

    val corpusDir = new File(options.corpusDir())
    val corpusPath = corpusDir.toPath
    val outDir = new File(options.outDir())
    outDir.mkdirs()
    val outPath = outDir.toPath
    val debugDir = options.debugDir.toOption.map(debugDir => (new File(debugDir)).toPath)
    val fileList = options.fileList.toOption.map(FileUtils.readFile(_).toSet)
    val extension = options.extension()
    val yamlFile = options.yamlFile.toOption.map{ yamlFilePath =>
      val yamlFile = new File(yamlFilePath)
      yamlFile.getParentFile.mkdirs()
      yamlFile.toPath
    }

    val yoloBaseLineAnnotator = YoloLineAnnotatorForObjectDetection(corpusPath, outPath, debugDir, options.keepStructure(), options.maxFiles.toOption, extension, fileList,
      yamlFile,
      altoFinder)
    yoloBaseLineAnnotator.annotate()
  }

  def main(args: Array[String]): Unit = {
    execute(args)
  }
}