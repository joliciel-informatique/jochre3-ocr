package com.joliciel.jochre.ocr.core.evaluation

import java.io.Writer

trait EvaluatorBase {
  def metricNames: Seq[String]

  def writeResults(writer: Writer, results: Seq[EvaluationResult]): Unit = {
    val headerLine = metricNames.mkString("\t")

    writer.write(f"File\t$headerLine\n")
    writer.flush()

    results.foreach { result =>
      val resultsPerMetric = metricNames.map(metric => result.results(metric)).map(result => f"$result%.2f").mkString("\t")
      writer.write(f"${result.file.getName}\t$resultsPerMetric\n")
      writer.flush()
    }

    val meanPerMetric = metricNames.map { metric =>
      val mean = results.map(_.results(metric)).sum / results.size.toDouble
      metric -> mean
    }.toMap

    val meanLine = metricNames.map(metric => meanPerMetric(metric)).map(result => f"$result%.2f").mkString("\t")
    writer.write(f"Mean\t$meanLine\n")
    writer.flush()
  }
}
