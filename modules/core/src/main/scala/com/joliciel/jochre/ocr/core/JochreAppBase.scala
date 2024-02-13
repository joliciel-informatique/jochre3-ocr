package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.corpus.TextSimplifier
import com.joliciel.jochre.ocr.core.evaluation.{CharacterCount, CharacterErrorRate, Evaluator}
import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.output.OutputFormat
import org.rogach.scallop.{ScallopConf, ScallopOption}
import zio._

import java.io.{File, FileWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Path


trait JochreAppBase {
  def textSimplifier: Option[TextSimplifier]

  class JochreCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val input: ScallopOption[String] = opt[String](required = true)
    val outputDir: ScallopOption[String] = opt[String](required = true)
    val debugDir: ScallopOption[String] = opt[String]()
    val maxImages: ScallopOption[Int] = opt[Int](default = Some(0), descr = "For directories, the max files to process. 0 means all files.")
    val startPage: ScallopOption[Int] = opt[Int](descr = "For PDF files, the start page, starting at 1.")
    val endPage: ScallopOption[Int] = opt[Int](descr = "For PDF files, the end page, starting at 1. 0 means all pages.")
    val dpi: ScallopOption[Int] = opt[Int](descr = "For PDF files, the DPI at which to export the file before analyzing. Default 300.")
    val outputFormats: ScallopOption[String] = opt[String](default = Some(OutputFormat.Alto4.entryName), descr = f"Comma-separated list of output formats among: ${OutputFormat.values.map(_.entryName).mkString(", ")}")
    val evalDir: ScallopOption[String] = opt[String]()
    val testRectangle: ScallopOption[String] = opt[String]()
    verify()
  }

  def app(args: Chunk[String]) =
    for {
      options <- ZIO.attempt(new JochreCLI(args))
      input = Path.of(options.input())
      outDir = Path.of(options.outputDir())
      evalDir = options.evalDir.toOption.map(Path.of(_))
      debugDir = options.debugDir.toOption.map(Path.of(_))
      maxImages = Option.when(options.maxImages() > 0)(options.maxImages())
      startPage = options.startPage.toOption
      endPage = options.endPage.toOption
      dpi = options.dpi.toOption
      outputFormats = options.outputFormats().split(",").map(OutputFormat.withName(_)).toSeq
      testRectangle <- ZIO.attempt {
        options.testRectangle.toOption.map { rectString =>
          val ltwh = rectString.split(",").map(_.toInt)
          Rectangle("", ltwh(0), ltwh(1), ltwh(2), ltwh(3))
        }
      }
      _ <- ZIO.attempt {
        evalDir.foreach(_.toFile.mkdirs())
        debugDir.foreach(_.toFile.mkdirs())
        outDir.toFile.mkdirs()
      }
      _ <- ZIO.serviceWithZIO[Jochre] { jochre =>
        evalDir.map { evalDir =>
          if (input.toFile.isFile) {
            throw new Exception(f"For evaluation, input must be a directory, got: ${input.toFile.getPath}")
          }
          val evaluator = Evaluator(jochre, Seq(CharacterErrorRate, CharacterCount), evalDir, textSimplifier)
          val evalWriter = new FileWriter(new File(evalDir.toFile, "eval.tsv"), StandardCharsets.UTF_8)
          for {
            results <- evaluator.evaluate(input, outputFormats, Some(outDir), debugDir, maxImages, testRectangle)
            _ <- ZIO.attempt {
              evaluator.writeResults(evalWriter, results)
            }
          } yield {
            results
          }
        }.getOrElse {
          if (input.toFile.isDirectory) {
            jochre.processDirectory(input, outputFormats, Some(outDir), debugDir, maxImages, testRectangle)
          } else if (input.toFile.getName.endsWith(".pdf")) {
            jochre.processPdf(input, None, outputFormats, Some(outDir), debugDir, startPage, endPage, dpi, testRectangle)
          } else {
            // Assume image file
            jochre.processImageFile(input, None, outputFormats, Some(outDir), debugDir, testRectangle)
          }
        }
      }
    } yield ExitCode.success
}
