package com.joliciel.jochre.ocr.core.corpus

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.model.{Alto, Page}
import com.joliciel.jochre.ocr.core.utils.{FileUtils, ImageUtils, StringUtils}
import org.bytedeco.opencv.opencv_core.Mat
import org.rogach.scallop._
import org.slf4j.LoggerFactory

import java.io.{File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Path

case class GlyphExtractor(
  corpusDir: Path,
  outDir: Path,
  debugDir: Option[Path] = None,
  maxFiles: Option[Int] = None,
  extension: String = "png",
  fileList: Option[Set[String]] = None,
  validationOneEvery: Option[Int] = None,
  textSimplifier: TextSimplifier = TextSimplifier.default,
  altoFinder: AltoFinder = AltoFinder.default
) extends CorpusAnnotator with ImageUtils with StringUtils {
  private val log = LoggerFactory.getLogger(getClass)

  debugDir.foreach(_.toFile.mkdirs())

  var alphabet: Set[String] = Set.empty

  def annotateOneFile(mat: Mat, alto: Alto, parentDir: File, baseName: String, index: Int): Unit = {
    debugDir.foreach(debugDir => saveImage(mat, debugDir.resolve(f"${baseName}_rotated.png")))

    val imageFileName = f"${baseName}.${extension}"

    val imageTrainingDir = new File(parentDir, f"images/train")
    imageTrainingDir.mkdirs()
    val imageTrainingFile = new File(imageTrainingDir, imageFileName)
    saveImage(mat, imageTrainingFile.toPath)

    val imageValDir = new File(parentDir, f"images/val")
    imageValDir.mkdirs()
    val imageValFile = new File(imageValDir, imageFileName)
    saveImage(mat, imageValFile.toPath)

    val page = alto.pages.head
    page.combinedWords.flatMap(_.glyphs).zipWithIndex.map { case (glyph, i) =>
      log.debug(f"Next glyph: $glyph")

      val trainOrVal = validationOneEvery.map { validationOneEvery =>
        if ((i + 1) % validationOneEvery == 0) {
          "val"
        } else {
          "train"
        }
      }.getOrElse("train")

      val height = (glyph.rectangle.height * 1.1).toInt
      val width = (glyph.rectangle.width * 1.1).toInt
      val adjustedWidth = if (width < height) { height } else { width }
      val leftMargin = (adjustedWidth - glyph.rectangle.width) / 2
      val topMargin = (height - glyph.rectangle.height) / 2
      val cropRectangle = Rectangle(glyph.rectangle.left - leftMargin, glyph.rectangle.top - topMargin, adjustedWidth, height)

      val rect = cropRectangle.intersection(page.rectangle).get

      val content = textSimplifier.simplify(glyph.content)

      val contentChars = stringToChars(content).toSet
      alphabet = alphabet.union(contentChars)

      val labelDir = new File(parentDir, f"labels/${trainOrVal}")
      labelDir.mkdirs()
      val textFile = new File(labelDir, "glyph-to-text.txt")
      val writer = new OutputStreamWriter(new FileOutputStream(textFile, true), StandardCharsets.UTF_8)
      try {
        writer.write(f"${imageFileName}\t${content}\t${rect.left},${rect.top},${rect.width},${rect.height}\n")
        writer.flush()
      } finally {
        writer.close()
      }


    }
  }
}

object GlyphExtractor {
  private val log = LoggerFactory.getLogger(getClass)

  class GlyphExtractorCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
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
    val options = new GlyphExtractorCLI(args.toIndexedSeq)

    val corpusDir = new File(options.corpusDir())
    val corpusPath = corpusDir.toPath
    val outDir = new File(options.outDir())
    outDir.mkdirs()
    val outPath = outDir.toPath
    val debugDir = options.debugDir.toOption.map(debugDir => (new File(debugDir)).toPath)
    val fileList = options.fileList.toOption.map(FileUtils.readFile(_).toSet)
    val extension = options.extension()
    val validationOneEvery = options.validationOneEvery.toOption

    val extractor = GlyphExtractor(corpusPath, outPath, debugDir, options.maxFiles.toOption, extension, fileList, validationOneEvery,
      textSimplifier, altoFinder=altoFinder)
    extractor.annotate()

    log.info(f"Alphabet: ${extractor.alphabet.toSeq.sorted.mkString("")}")
  }

  def main(args: Array[String]): Unit = {
    execute(args)
  }
}
