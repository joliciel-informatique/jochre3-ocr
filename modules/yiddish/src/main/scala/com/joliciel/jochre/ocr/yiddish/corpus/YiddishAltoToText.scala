package com.joliciel.jochre.ocr.yiddish.corpus

import com.joliciel.jochre.ocr.core.corpus.TextSimplifier
import com.joliciel.jochre.ocr.core.model.Alto
import com.joliciel.jochre.ocr.core.utils.FileUtils
import com.joliciel.jochre.ocr.yiddish.YiddishTextSimpifier
import org.rogach.scallop.{ScallopConf, ScallopOption, stringConverter, intConverter, flagConverter}

import java.io.File
import java.nio.file.Path
import scala.xml.XML

case class YiddishAltoToText(
    corpusDir: Path,
    outDir: Path,
    keepStructure: Boolean = false,
    textSimplifier: Option[TextSimplifier] = Some(
      YiddishTextSimpifier(replaceNonHebrewAlphabets = false)
    )
) extends FileUtils {
  def extract(): Unit = {
    val altoFiles = listFiles(corpusDir, ".*\\.xml".r, recursive = true)
    altoFiles.foreach { altoFile =>
      val xml = XML.loadFile(altoFile)
      val alto = Alto.fromXML(xml)
      val content = alto.content

      val filePath = altoFile.toPath
      val fileName = if (keepStructure) {
        corpusDir.relativize(filePath).toString
      } else {
        filePath.toFile.getName
      }

      val baseName = FileUtils.removeFileExtension(fileName)

      val textFile = outDir.resolve(f"$baseName.txt")

      textFile.getParent.toFile.mkdirs()

      writeFile(textFile, content)

      textSimplifier.foreach { textSimplifier =>
        val textFileSimplified = outDir.resolve(f"${baseName}_simplified.txt")
        val simplifiedContent = textSimplifier.simplify(content)
        writeFile(textFileSimplified, simplifiedContent)
      }
    }
  }
}

object YiddishAltoToText {
  private class CLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val corpusDir: ScallopOption[String] = opt[String](
      required = true,
      descr = "The directory containing XML in alto format"
    )
    val outputDir: ScallopOption[String] = opt[String](
      required = true,
      descr = "The directory where the text will be placed"
    )
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

    val extractor = YiddishAltoToText(
      corpusPath,
      outPath,
      keepStructure
    )
    extractor.extract()
  }
}
