package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.evaluation.{CharacterCount, CharacterErrorRate, TextEvaluator}
import org.rogach.scallop._

import java.io.{File, FileWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.collection.immutable.ArraySeq

object YiddishTextEvaluator {

  private class YiddishTextEvaluatorCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val inputDir: ScallopOption[String] = opt[String](required = true)
    val goldDir: ScallopOption[String] = opt[String](required = true)
    val evalDir: ScallopOption[String] = opt[String](required = true)
    val ignoreParagraphs: ScallopOption[Boolean] = {
      opt[Boolean](
        default = Some(false),
        descr = "If true, ignore paragraph marks when evaluating, only evaluating the text inside the paragraphs."
      )
    }
    verify()
  }

  def main(args: Array[String]): Unit = {
    val cli = new YiddishTextEvaluatorCLI(ArraySeq.unsafeWrapArray(args))
    val inputDir = Path.of(cli.inputDir())
    val goldDir = Path.of(cli.goldDir())
    val evalDir = Path.of(cli.evalDir())
    evalDir.toFile.mkdirs()

    val ignoreParagraphs = cli.ignoreParagraphs()

    val evaluator = TextEvaluator(
      Seq(CharacterErrorRate, CharacterCount),
      evalDir,
      Some(YiddishTextSimpifier(replaceNonHebrewAlphabets = false)),
      ignoreParagraphs
    )
    val evalWriter = new FileWriter(
      new File(evalDir.toFile, "eval.tsv"),
      StandardCharsets.UTF_8
    )
    try {
      val results = evaluator.evaluate(inputDir, goldDir)
      evaluator.writeResults(evalWriter, results)
    } finally {
      evalWriter.close()
    }
  }
}
