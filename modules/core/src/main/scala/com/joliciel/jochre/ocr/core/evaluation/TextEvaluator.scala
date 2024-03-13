package com.joliciel.jochre.ocr.core.evaluation

import com.joliciel.jochre.ocr.core.corpus.TextSimplifier
import com.joliciel.jochre.ocr.core.utils.FileUtils
import org.slf4j.LoggerFactory

import java.nio.file.Path

case class TextEvaluator(
    metrics: Seq[TextEvaluationMetric],
    evalDir: Path,
    textSimplifier: Option[TextSimplifier] = None
) extends EvaluatorBase
    with FileUtils {
  private val log = LoggerFactory.getLogger(getClass)

  def evaluate(inputDir: Path, goldDir: Path): Seq[EvaluationResult] = {
    val files = listFiles(inputDir, raw".*\.txt".r)
    val results = files.zipWithIndex.map { case (file, i) =>
      log.info(f"Evaluating file $i: ${file.getPath}")
      val predictedText = readFile(file).mkString("\n")
      val filename = file.getName
      val expectedFile = goldDir.resolve(filename)
      val expectedText = readFile(expectedFile.toFile).mkString("\n")
      val expected =
        textSimplifier.map(_.simplify(expectedText)).getOrElse(expectedText)
      val predicted =
        textSimplifier.map(_.simplify(predictedText)).getOrElse(predictedText)
      val results = metrics.map { metric =>
        metric.name -> metric.evaluate(predicted, expected)
      }.toMap
      EvaluationResult(file, results)
    }
    results
  }

  override def metricNames: Seq[String] = metrics.map(_.name)
}
