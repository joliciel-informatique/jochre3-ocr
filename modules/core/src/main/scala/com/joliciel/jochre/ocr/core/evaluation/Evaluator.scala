package com.joliciel.jochre.ocr.core.evaluation

import com.joliciel.jochre.ocr.core.Jochre
import com.joliciel.jochre.ocr.core.corpus.{AltoFinder, TextSimplifier}
import com.joliciel.jochre.ocr.core.model.Page
import com.joliciel.jochre.ocr.core.utils.FileUtils
import org.slf4j.LoggerFactory
import zio.{Task, ZIO}

import java.io.{File, Writer}
import java.nio.file.{Path, Paths}
import java.time.Instant
import java.time.temporal.ChronoUnit

case class EvaluationResult(file: File, results: Map[String, Double])

case class Evaluator(
  jochre: Jochre,
  metrics: Seq[TextEvaluationMetric],
  evalDir: Path,
  textSimplifier: Option[TextSimplifier] = None,
  altoFinder: AltoFinder = AltoFinder.default
) extends FileUtils {

  private val log = LoggerFactory.getLogger(getClass)
  private val analysisTimeName = "AnalysisTime"

  def evaluate(inputDir: Path, outputDir: Option[Path], debugDir: Option[Path], maxImages: Option[Int]): Task[Seq[EvaluationResult]] = {
    val files = jochre.getImageFilesFromDir(inputDir, maxImages)
    ZIO.foreach(files.zipWithIndex){
      case ((file, mat), i) =>
        log.info(f"Evaluating file $i: ${file.getPath}")
        val expected = altoFinder.getAltoPage(file.toPath)
        val startTime = Instant.now()
        for {
          predicted <- jochre.processImage(mat, outputDir, debugDir, file.getName)
          endTime = Instant.now()
          results <- ZIO.foreach(metrics){ metric =>
            ZIO.attempt{
              val predictedText = Page.fromXML(predicted).content
              val expectedText = textSimplifier.map(_.simplify(expected.content)).getOrElse(expected.content)

              writeFile(evalDir.resolve(f"${file.getName}_predicted.txt"), predictedText)
              writeFile(evalDir.resolve(f"${file.getName}_expected.txt"), expectedText)

              metric.name -> metric.evaluate(predictedText, expectedText)
            }
          }.map(_.toMap + (analysisTimeName -> ChronoUnit.MILLIS.between(startTime, endTime).toDouble / 1000))
        } yield {
          EvaluationResult(file, results)
        }
    }
  }

  def writeResults(writer: Writer, results: Seq[EvaluationResult]): Unit = {
    val metricNames = analysisTimeName +: metrics.map(_.name)
    val headerLine = metricNames.mkString("\t")

    writer.write(f"File\t$headerLine\n")
    writer.flush()

    results.foreach{ result =>
      val resultsPerMetric = metricNames.map(metric => result.results(metric)).map(result => f"$result%.2f").mkString("\t")
      writer.write(f"${result.file.getName}\t$resultsPerMetric\n")
      writer.flush()
    }

    val meanPerMetric = metricNames.map{ metric =>
      val mean = results.map(_.results(metric)).sum / results.size.toDouble
      metric -> mean
    }.toMap

    val meanLine = metricNames.map(metric => meanPerMetric(metric)).map(result => f"$result%.2f").mkString("\t")
    writer.write(f"Mean\t$meanLine\n")
    writer.flush()
  }
}