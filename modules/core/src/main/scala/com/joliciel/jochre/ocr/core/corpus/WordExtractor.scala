package com.joliciel.jochre.ocr.core.corpus

import com.joliciel.jochre.ocr.core.model.Page
import com.joliciel.jochre.ocr.core.utils.{FileUtils, OpenCvUtils}
import org.bytedeco.opencv.opencv_core.Mat
import org.rogach.scallop.{ScallopConf, ScallopOption}
import org.slf4j.LoggerFactory

import java.io.{File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Path

case class WordExtractor(
  corpusDir: Path,
  outDir: Path,
  debugDir: Option[Path] = None,
  keepStructure: Boolean = false,
  maxFiles: Option[Int] = None,
  extension: String = "png",
  fileList: Option[Set[String]] = None,
  textSimplifier: TextSimplifier = TextSimplifier.default,
  altoFinder: AltoFinder = AltoFinder.default
) extends CorpusAnnotator with OpenCvUtils {
  private val log = LoggerFactory.getLogger(getClass)

  val textFile = new File(outDir.toFile, "word-to-text.txt")
  val writer = new OutputStreamWriter(new FileOutputStream(textFile), StandardCharsets.UTF_8)
  debugDir.foreach(_.toFile.mkdirs())

  def annotateOneFile(mat: Mat, alto: Page, parentDir: File, baseName: String, index: Int): Unit = {
    debugDir.foreach(debugDir => saveImage(mat, new File(debugDir.toFile, f"${baseName}_rotated.png").getPath))

    alto.combinedWords.zipWithIndex.map { case (word, i) =>
      log.debug(f"Next word: $word")
      val cropped = crop(mat, word.rectangle)
      val content = textSimplifier.simplify(word.content)

      val fileNameBase = f"${baseName}_${"%04d".format(i)}"
      val imageFileName = f"${fileNameBase}.${extension}"

      writer.write(f"${imageFileName}\t${content}\n")
      val imageFile = new File(parentDir, imageFileName)
      saveImage(cropped, imageFile.getPath)
    }
    writer.flush()
  }

  def cleanUp(): Unit = {
    writer.close()
  }
}

object WordExtractor {
  class WordExtractorCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val corpusDir: ScallopOption[String] = opt[String](required = true, descr = "The directory containing original images and labels in Alto4 format")
    val outDir: ScallopOption[String] = opt[String](required = true, descr = "The directory where the processed images will be placed")
    val debugDir: ScallopOption[String] = opt[String](required = false, descr = "A directory where to write debug images")
    val keepStructure: ScallopOption[Boolean] = opt[Boolean](descr = "If present, keep the sub-directory structure within the out-dir")
    val maxFiles = opt[Int](descr = "If present, only transform this many files at most")
    val extension: ScallopOption[String] = choice(Seq("png", "jpg"), default = Some("png"))
    val fileList: ScallopOption[String] = opt[String](required = false, descr = "If present, limit the files to this list only")
    verify()
  }

  def execute(args: Array[String], textSimplifier: TextSimplifier = TextSimplifier.default, altoFinder: AltoFinder = AltoFinder.default): Unit = {
    val options = new WordExtractorCLI(args.toIndexedSeq)

    val corpusDir = new File(options.corpusDir())
    val corpusPath = corpusDir.toPath
    val outDir = new File(options.outDir())
    outDir.mkdirs()
    val outPath = outDir.toPath
    val debugDir = options.debugDir.toOption.map(debugDir => (new File(debugDir)).toPath)
    val fileList = options.fileList.toOption.map(FileUtils.readFile(_).toSet)
    val extension = options.extension()

    val extractor = WordExtractor(corpusPath, outPath, debugDir, options.keepStructure(), options.maxFiles.toOption, extension, fileList,
      textSimplifier, altoFinder=altoFinder)
    extractor.annotate()
  }

  def main(args: Array[String]): Unit = {
    execute(args)
  }
}
