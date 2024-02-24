package com.joliciel.jochre.ocr.yiddish.corpus

import com.joliciel.jochre.ocr.core.corpus.{AltoFinder, AnnotatedImageTransformer, CorpusAnnotator}
import com.joliciel.jochre.ocr.core.model.{Alto, Glyph, Word}
import com.joliciel.jochre.ocr.core.output.OutputFormat
import com.joliciel.jochre.ocr.core.utils.{ImageUtils, OutputLocation}
import org.bytedeco.opencv.opencv_core.Mat
import org.rogach.scallop.{ScallopConf, ScallopOption}

import java.io.File
import java.nio.file.Path

case class YiddishCorpusTransformer(
  corpusDir: Path,
  outDir: Path,
  override val keepStructure: Boolean = false,
  maxFiles: Option[Int] = None,
  extension: String = ".png",
  fileList: Option[Set[String]] = None,
  altoFinder: AltoFinder = AltoFinder.default
) extends CorpusAnnotator with ImageUtils {
  override val initialTransforms: Seq[AnnotatedImageTransformer[_]] = Seq.empty

  override def annotateOneFile(mat: Mat, alto: Alto, parentDir: File, baseName: String, index: Int): Unit = {
    val transformed = alto.transform{
      case word: Word =>
        val (newGlyphs, somethingChanged) = word.glyphs.foldLeft(Seq.empty[Glyph] -> false){ case ((newGlyphs, somethingChanged), glyph) => newGlyphs match {
          case Nil => (newGlyphs :+ glyph) -> somethingChanged
          case _ =>
            if (glyph.content=="יַ" && newGlyphs.last.content=="י") {
              val combinedGlyph = Glyph(glyph.rectangle.union(newGlyphs.last.rectangle).copy(label = "ײַ"), confidence = 1.0)
              (newGlyphs.init :+ combinedGlyph) -> true
            } else {
              (newGlyphs :+ glyph) -> somethingChanged
            }
        }}
        if (somethingChanged) {
          val newContent = newGlyphs.map(_.content).mkString
          word.copy(glyphs = newGlyphs, rectangle = word.rectangle.copy(label = newContent))
        } else {
          word
        }
    }

    val outputFormat = OutputFormat.Alto4
    val transformedAlto = outputFormat(transformed)
    val outputLocation = OutputLocation(outDir, baseName)
    val altoFile = outputLocation.resolve(outputFormat.suffix)
    val altoFileParent = altoFile.getParent
    altoFileParent.toFile.mkdirs()
    writeFile(altoFile, transformedAlto)

    val imageFile = outputLocation.resolve(extension)
    saveImage(mat, imageFile)
  }

  override def cleanUp(): Unit = {}
}

object YiddishCorpusTransformer {
  private class CLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val corpusDir: ScallopOption[String] = opt[String](required = true, descr = "The directory containing original images and labels in Alto4 format")
    val outputDir: ScallopOption[String] = opt[String](required = true, descr = "The directory where the processed images will be placed")
    val keepStructure: ScallopOption[Boolean] = opt[Boolean]()
    verify()
  }

  def main(args: Array[String]): Unit = {
    val options = new CLI(args.toIndexedSeq)

    val corpusDir = new File(options.corpusDir())
    val corpusPath = corpusDir.toPath
    val outDir = new File(options.outputDir())
    outDir.mkdirs()
    val outPath = outDir.toPath

    val keepStructure = options.keepStructure()

    val transformer = YiddishCorpusTransformer(corpusPath, outPath, keepStructure = keepStructure)
    transformer.annotate()
  }
}