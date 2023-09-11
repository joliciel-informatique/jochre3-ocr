package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.Jochre
import com.joliciel.jochre.ocr.core.analysis.TextAnalyzer
import org.rogach.scallop.{ScallopConf, ScallopOption}

import java.nio.file.Path

case class JochreYiddish(override val outputDir: Option[Path] = None) extends Jochre {
  override val textAnalyzer: TextAnalyzer = Jochre2Analyzer
}

object JochreYiddish {
  class JochreYiddishCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val inputDir: ScallopOption[String] = opt[String](required = true)
    val outputDir: ScallopOption[String] = opt[String](required = true)
    val maxImages: ScallopOption[Int] = opt[Int](default = Some(0))
    verify()
  }

  def main(args: Array[String]): Unit = {
    val options = new JochreYiddishCLI(args.toIndexedSeq)

    val inputDir = Path.of(options.inputDir())
    val outDir = Path.of(options.outputDir())
    outDir.toFile.mkdirs()

    val maxImages = Option.when(options.maxImages() > 0)(options.maxImages())

    val jochre: JochreYiddish = JochreYiddish(Some(outDir))
    jochre.process(inputDir, maxImages)
  }
}