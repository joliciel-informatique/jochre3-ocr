package com.joliciel.jochre.ocr.core.corpus

import com.joliciel.jochre.ocr.core.model.Alto
import com.joliciel.jochre.ocr.core.utils.FileUtils
import org.bytedeco.opencv.opencv_core.Mat
import org.rogach.scallop.{ScallopConf, ScallopOption, stringConverter, intConverter}

import java.awt.geom.AffineTransform
import java.awt.image.{AffineTransformOp, BufferedImage}
import java.io.{File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.text.NumberFormat
import java.util.Locale
import javax.imageio.ImageIO
import scala.util.Using

case class RotationAnnotator(
    corpusDir: Path,
    outDir: Path,
    maxFiles: Option[Int] = None,
    extension: String = "png",
    fileList: Option[Set[String]] = None,
    validationOneEvery: Option[Int] = None,
    imageSize: Int = 1280,
    altoFinder: AltoFinder = AltoFinder.default
) extends CorpusAnnotator {
  override val initialTransforms: Seq[AnnotatedImageTransformer[_]] = Seq.empty

  override def annotateOneFile(mat: Mat, alto: Alto, parentDir: File, baseName: String, index: Int): Unit = {
    val page = alto.pages.head
    val rotation = page.rotation

    val height = mat.rows()
    val width = mat.cols()

    val image = new BufferedImage(
      width,
      height,
      BufferedImage.TYPE_INT_RGB
    )

    val graphics = image.createGraphics

    val originalImage = toBufferedImage(mat)
    graphics.drawImage(originalImage, 0, 0, null)
    graphics.dispose()

    val imageSizeDouble = imageSize.toDouble

    val scaledWidth = if (height > width) {
      (width.toDouble / height.toDouble) * imageSizeDouble
    } else {
      imageSizeDouble
    }
    val scaledHeight = if (height > width) {
      imageSizeDouble
    } else {
      (height.toDouble / width.toDouble) * imageSizeDouble
    }

    val after = new BufferedImage(scaledWidth.toInt, scaledHeight.toInt, BufferedImage.TYPE_INT_ARGB)
    val at = new AffineTransform
    at.scale(scaledWidth / width.toDouble, scaledHeight / height.toDouble)
    val scaleOp = new AffineTransformOp(at, AffineTransformOp.TYPE_BILINEAR)
    val scaledImage = scaleOp.filter(originalImage, after)

    val trainOrVal = validationOneEvery
      .map { validationOneEvery =>
        if ((index + 1) % validationOneEvery == 0) {
          "val"
        } else {
          "train"
        }
      }
      .getOrElse("train")

    val imageFileName = f"$baseName.$extension"
    val imageFile = new File(parentDir, f"$trainOrVal/$imageFileName")
    imageFile.getParentFile.mkdirs()
    ImageIO.write(scaledImage, extension, imageFile)

    val labelFileName = f"$baseName.txt"
    val labelFile = new File(parentDir, f"$trainOrVal/$labelFileName")
    labelFile.getParentFile.mkdirs()
    val fmtLocale = Locale.US

    val formatter = NumberFormat.getInstance(fmtLocale)
    formatter.setMaximumFractionDigits(2)
    formatter.setMinimumFractionDigits(2)

    Using(
      new OutputStreamWriter(
        new FileOutputStream(labelFile),
        StandardCharsets.UTF_8
      )
    ) { writer =>
      writer.write(formatter.format(rotation))
      writer.flush()
    }
  }
}

object RotationAnnotator {
  private class CLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val corpusDir: ScallopOption[String] = opt[String](
      required = true,
      descr = "The directory containing original images and labels in Alto4 format"
    )
    val outDir: ScallopOption[String] = opt[String](
      required = true,
      descr = "The directory where the processed images will be placed"
    )
    val maxFiles: ScallopOption[Int] =
      opt[Int](descr = "If present, only transform this many files at most")
    val extension: ScallopOption[String] =
      choice(Seq("png", "jpg"), default = Some("png"))
    val fileList: ScallopOption[String] = opt[String](
      required = false,
      descr = "If present, limit the files to this list only"
    )
    val validationOneEvery: ScallopOption[Int] = opt[Int](
      required = false,
      descr = "If present, add train/val sub-directories and mark one out of every n files for validation"
    )
    val imageSize: ScallopOption[Int] = opt[Int](
      default = Some(1280),
      descr = "The image size, must be a multiple of 32. Default is 40*32=1280."
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

    val fileList = options.fileList.toOption.map(FileUtils.readFile(_).toSet)
    val extension = options.extension()

    val validationOneEvery = options.validationOneEvery.toOption
    val imageSize = options.imageSize()

    val annotator = RotationAnnotator(
      corpusPath,
      outPath,
      options.maxFiles.toOption,
      extension,
      fileList,
      validationOneEvery,
      imageSize,
      altoFinder
    )
    annotator.annotate()
  }

  def main(args: Array[String]): Unit = {
    execute(args)
  }
}
