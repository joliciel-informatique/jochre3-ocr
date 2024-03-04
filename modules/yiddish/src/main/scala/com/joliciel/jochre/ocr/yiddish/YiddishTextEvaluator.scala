package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.evaluation.{CharacterCount, CharacterErrorRate, TextEvaluator}
import org.rogach.scallop.{ScallopConf, ScallopOption}

import java.io.{File, FileWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Path

object YiddishTextEvaluator {

  class YiddishTextEvaluatorCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val inputDir: ScallopOption[String] = opt[String](required = true)
    val goldDir: ScallopOption[String] = opt[String](required = true)
    val evalDir: ScallopOption[String] = opt[String](required = true)
    verify()
  }

  def main(args: Array[String]): Unit = {
    val cli = new YiddishTextEvaluatorCLI(args)
    val inputDir = Path.of(cli.inputDir())
    val goldDir = Path.of(cli.goldDir())
    val evalDir = Path.of(cli.evalDir())
    evalDir.toFile.mkdirs()

    val evaluator = TextEvaluator(Seq(CharacterErrorRate, CharacterCount), evalDir, Some(YiddishTextSimpifier(replaceNonHebrewAlphabets = false)))
    val evalWriter = new FileWriter(new File(evalDir.toFile, "eval.tsv"), StandardCharsets.UTF_8)
    try {
      val results = evaluator.evaluate(inputDir, goldDir)
      evaluator.writeResults(evalWriter, results)
    } finally {
      evalWriter.close()
    }
  }
}
