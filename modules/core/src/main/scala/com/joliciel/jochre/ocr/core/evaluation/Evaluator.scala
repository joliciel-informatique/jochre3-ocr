package com.joliciel.jochre.ocr.core.evaluation

import com.joliciel.jochre.ocr.core.Jochre
import com.joliciel.jochre.ocr.core.corpus.{AltoFinder, TextSimplifier}
import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.output.OutputFormat
import com.joliciel.jochre.ocr.core.utils.FileUtils
import org.slf4j.LoggerFactory
import zio.{Task, ZIO}

import java.nio.file.Path
import java.time.Instant
import java.time.temporal.ChronoUnit

case class Evaluator(
    jochre: Jochre,
    metrics: Seq[TextEvaluationMetric],
    evalDir: Path,
    textSimplifier: Option[TextSimplifier] = None,
    altoFinder: AltoFinder = AltoFinder.default,
    ignoreParagraphs: Boolean = false
) extends EvaluatorBase
    with FileUtils {

  private val log = LoggerFactory.getLogger(getClass)
  private val analysisTimeName = "AnalysisTime"

  def evaluate(
      inputDir: Path,
      outputFormats: Seq[OutputFormat] = Seq(OutputFormat.Alto4),
      outputDir: Option[Path],
      debugDir: Option[Path],
      maxImages: Option[Int],
      testRectangle: Option[Rectangle] = None
  ): Task[Seq[EvaluationResult]] = {
    val files = getImageFilesFromDir(inputDir, maxImages)
    ZIO.foreach(files.zipWithIndex) { case ((file, mat), i) =>
      log.info(f"Evaluating file $i: ${file.getPath}")
      val expected = altoFinder.getAlto(file.toPath)
      val startTime = Instant.now()
      for {
        predicted <- jochre.processMat(
          mat,
          file.getName,
          outputFormats,
          outputDir,
          debugDir,
          testRectangle
        )
        endTime = Instant.now()
        results <- ZIO
          .foreach(metrics) { metric =>
            ZIO.attempt {
              val predictedText = predicted.content

              val predictedForEvaluation = if (ignoreParagraphs) {
                predictedText.replaceAll("\n\n+", "\n")
              } else {
                predictedText
              }

              val expectedText = textSimplifier
                .map(_.simplify(expected.content))
                .getOrElse(expected.content)

              val expectedForEvaluation = if (ignoreParagraphs) {
                expectedText.replaceAll("\n\n+", "\n")
              } else {
                expectedText
              }

              writeFile(
                evalDir.resolve(f"${file.getName}_predicted.txt"),
                predictedText
              )
              writeFile(
                evalDir.resolve(f"${file.getName}_expected.txt"),
                expectedText
              )

              metric.name -> metric.evaluate(predictedForEvaluation, expectedForEvaluation)
            }
          }
          .map(
            _.toMap + (analysisTimeName -> ChronoUnit.MILLIS
              .between(startTime, endTime)
              .toDouble / 1000)
          )
      } yield {
        EvaluationResult(file, results)
      }
    }
  }

  val metricNames: Seq[String] = analysisTimeName +: metrics.map(_.name)
}
