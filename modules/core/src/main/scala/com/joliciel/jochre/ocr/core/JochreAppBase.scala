package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.corpus.TextSimplifier
import com.joliciel.jochre.ocr.core.evaluation.{CharacterCount, CharacterErrorRate, Evaluator}
import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.output.OutputFormat
import zio._

import java.io.{File, FileWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Path

trait JochreAppBase {
  def textSimplifier: Option[TextSimplifier]

  def app(options: JochreCLI): ZIO[Jochre, Throwable, ExitCode] = {
    val input = Path.of(options.input())
    val outDir = Path.of(options.outputDir())
    val evalDir = options.evalDir.toOption.map(Path.of(_))
    val debugDir = options.debugDir.toOption.map(Path.of(_))
    val maxImages = Option.when(options.maxImages() > 0)(options.maxImages())
    val startPage = options.startPage.toOption
    val endPage = options.endPage.toOption
    val dpi = options.dpi.toOption
    val outputFormats = {
      options.outputFormats().split(",").map(OutputFormat.withName).toSeq
    }
    val writeImages = options.writeImages()
    val ignoreParagraphs = options.evalIgnoreParagraphs()

    for {
      testRectangle <- ZIO.attempt {
        options.testRectangle.toOption.map { rectString =>
          val ltwh = rectString.split(",").map(_.toInt)
          Rectangle(ltwh(0), ltwh(1), ltwh(2), ltwh(3))
        }
      }
      _ <- ZIO.attempt {
        evalDir.foreach(_.toFile.mkdirs())
        debugDir.foreach(_.toFile.mkdirs())
        outDir.toFile.mkdirs()
      }
      _ <- ZIO.serviceWithZIO[Jochre] { jochre =>
        evalDir
          .map { evalDir =>
            if (input.toFile.isFile) {
              throw new Exception(
                f"For evaluation, input must be a directory, got: ${input.toFile.getPath}"
              )
            }
            val evaluator = Evaluator(
              jochre,
              Seq(CharacterErrorRate, CharacterCount),
              evalDir,
              textSimplifier,
              ignoreParagraphs = ignoreParagraphs
            )
            val evalWriter = new FileWriter(
              new File(evalDir.toFile, "eval.tsv"),
              StandardCharsets.UTF_8
            )
            for {
              results <- evaluator.evaluate(
                input,
                outputFormats,
                Some(outDir),
                debugDir,
                maxImages,
                testRectangle
              )
              _ <- ZIO.attempt {
                evaluator.writeResults(evalWriter, results)
              }
            } yield {
              results
            }
          }
          .getOrElse {
            if (input.toFile.isDirectory) {
              jochre.processDirectory(
                input,
                outputFormats,
                Some(outDir),
                debugDir,
                maxImages,
                testRectangle
              )
            } else if (input.toFile.getName.endsWith(".pdf")) {
              jochre.processPdf(
                input,
                None,
                outputFormats,
                Some(outDir),
                debugDir,
                startPage,
                endPage,
                dpi,
                testRectangle,
                writeImages
              )
            } else {
              // Assume image file
              jochre.processImageFile(
                input,
                None,
                outputFormats,
                Some(outDir),
                debugDir,
                testRectangle
              )
            }
          }
      }
    } yield ExitCode.success
  }
}
