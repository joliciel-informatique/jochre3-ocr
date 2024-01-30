package com.joliciel.jochre.ocr.core.corpus

import com.joliciel.jochre.ocr.core.model.Page
import com.joliciel.jochre.ocr.core.utils.{FileUtils, ImageUtils}
import org.bytedeco.opencv.opencv_core.Mat
import org.rogach.scallop.{ScallopConf, ScallopOption}
import org.slf4j.LoggerFactory

import java.io.{File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Path

case class TextLineExtractor(
  corpusDir: Path,
  outDir: Path,
  debugDir: Option[Path] = None,
  maxFiles: Option[Int] = None,
  extension: String = "png",
  fileList: Option[Set[String]] = None,
  validationOneEvery: Option[Int] = None,
  textSimplifier: TextSimplifier = TextSimplifier.default,
  altoFinder: AltoFinder = AltoFinder.default
) extends CorpusAnnotator with ImageUtils {
  private val log = LoggerFactory.getLogger(getClass)

  debugDir.foreach(_.toFile.mkdirs())

  def annotateOneFile(mat: Mat, alto: Page, parentDir: File, baseName: String, index: Int): Unit = {
    debugDir.foreach(debugDir => saveImage(mat, new File(debugDir.toFile, f"${baseName}_rotated.png").getPath))

    alto.textLinesWithRectangles.zipWithIndex.map { case ((textLine, rectangle), i) =>
      log.debug(f"Next textLine: $rectangle")

      val trainOrVal = validationOneEvery.map { validationOneEvery =>
        if ((i + 1) % validationOneEvery == 0) {
          "val"
        } else {
          "train"
        }
      }.getOrElse("train")

      val cropped = crop(mat, rectangle)
      val content = textSimplifier.simplify(textLine.content)

      val fileNameBase = f"${baseName}_${"%03d".format(i)}"
      val imageFileName = f"${fileNameBase}.${extension}"

      val labelDir = new File(parentDir, f"labels/${trainOrVal}")
      labelDir.mkdirs()
      val textFile = new File(labelDir, "line-to-text.txt")
      val writer = new OutputStreamWriter(new FileOutputStream(textFile, true), StandardCharsets.UTF_8)
      try {
        writer.write(f"${imageFileName}\t${content}\n")
        writer.flush()
      } finally {
        writer.close()
      }

      val imageDir = new File(parentDir, f"images/${trainOrVal}")
      imageDir.mkdirs()
      val imageFile = new File(imageDir, imageFileName)
      saveImage(cropped, imageFile.getPath)
    }
  }

  def cleanUp(): Unit = {
  }
}

object TextLineExtractor {
  class TextLineExtractorCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val corpusDir: ScallopOption[String] = opt[String](required = true, descr = "The directory containing original images and labels in Alto4 format")
    val outDir: ScallopOption[String] = opt[String](required = true, descr = "The directory where the processed images will be placed")
    val debugDir: ScallopOption[String] = opt[String](required = false, descr = "A directory where to write debug images")
    val maxFiles = opt[Int](descr = "If present, only transform this many files at most")
    val extension: ScallopOption[String] = choice(Seq("png", "jpg"), default = Some("png"))
    val fileList: ScallopOption[String] = opt[String](required = false, descr = "If present, limit the files to this list only")
    val validationOneEvery: ScallopOption[Int] = opt[Int](required = false, descr = "If present, add train/val sub-directories and mark one out of every n files for validation")

    verify()
  }

  def execute(args: Array[String], textSimplifier: TextSimplifier = TextSimplifier.default, altoFinder: AltoFinder = AltoFinder.default): Unit = {
    val options = new TextLineExtractorCLI(args.toIndexedSeq)

    val corpusDir = new File(options.corpusDir())
    val corpusPath = corpusDir.toPath
    val outDir = new File(options.outDir())
    outDir.mkdirs()
    val outPath = outDir.toPath
    val debugDir = options.debugDir.toOption.map(debugDir => (new File(debugDir)).toPath)
    val fileList = options.fileList.toOption.map(FileUtils.readFile(_).toSet)
    val extension = options.extension()
    val validationOneEvery = options.validationOneEvery.toOption

    val textLineExtractor = TextLineExtractor(corpusPath, outPath, debugDir, options.maxFiles.toOption, extension, fileList, validationOneEvery,
      textSimplifier, altoFinder=altoFinder)
    textLineExtractor.annotate()
  }

  def main(args: Array[String]): Unit = {
    execute(args)
  }
}
