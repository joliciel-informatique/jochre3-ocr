package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.alto.{AltoTransformer, ImageToAltoConverter}
import com.joliciel.jochre.ocr.core.evaluation.{CharacterCount, CharacterErrorRate, Evaluator}
import com.joliciel.jochre.ocr.core.segmentation.{NonSegmenterService, SegmenterService}
import com.joliciel.jochre.ocr.core.text.{FullPageTextGuesserService, TextGuesserService}
import com.joliciel.jochre.ocr.core.{AbstractJochre, Jochre}
import org.rogach.scallop.{ScallopConf, ScallopOption}
import zio._

import java.io.{File, FileWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Path

object JochreYiddishWithoutYolo extends ZIOAppDefault {
  private case class JochreYiddishImpl(segmenterService: SegmenterService, textGuesserService: TextGuesserService) extends AbstractJochre {
    override val altoTransformer: AltoTransformer = YiddishAltoTransformer()
  }

  private val segmenterService: ZLayer[Any, Nothing, SegmenterService] = NonSegmenterService.live
  private val textAnalyzerService = Jochre2Analyzer.live
  private val textGuesserService: ZLayer[ImageToAltoConverter, Nothing, TextGuesserService] = FullPageTextGuesserService.live

  private val jochreYiddishLayerInternal: ZLayer[SegmenterService with TextGuesserService, Throwable, Jochre] = ZLayer {
    for {
      segmenterService <- ZIO.service[SegmenterService]
      textGuesserService <- ZIO.service[TextGuesserService]
    } yield (JochreYiddishImpl(segmenterService, textGuesserService))
  }

  private val noDepTextGuesserService = textAnalyzerService >>> textGuesserService
  val jochreYiddishLayer: ZLayer[Any, Throwable, Jochre] = (segmenterService ++ noDepTextGuesserService) >>> jochreYiddishLayerInternal

  class JochreYiddishCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val inputDir: ScallopOption[String] = opt[String](required = true)
    val outputDir: ScallopOption[String] = opt[String](required = true)
    val debugDir: ScallopOption[String] = opt[String]()
    val maxImages: ScallopOption[Int] = opt[Int](default = Some(0))
    val evalDir: ScallopOption[String] = opt[String]()
    verify()
  }

  def app(args: Chunk[String]) =
    for {
      options <- ZIO.attempt(new JochreYiddishCLI(args))
      inputDir = Path.of(options.inputDir())
      outDir = Path.of(options.outputDir())
      debugDir = options.debugDir.toOption.map(Path.of(_))
      evalDir = options.evalDir.toOption.map(Path.of(_))
      maxImages = Option.when(options.maxImages() > 0)(options.maxImages())
      _ <- ZIO.attempt {
        evalDir.foreach(_.toFile.mkdirs())
        debugDir.foreach(_.toFile.mkdirs())
        outDir.toFile.mkdirs()
      }
      _ <- ZIO.serviceWithZIO[Jochre] { jochre =>
        evalDir.map { evalDir =>
          val evaluator = Evaluator(jochre, Seq(CharacterErrorRate, CharacterCount), evalDir, textSimplifier = Some(YiddishTextSimpifier))
          val evalWriter = new FileWriter(new File(evalDir.toFile, "eval.tsv"), StandardCharsets.UTF_8)
          for {
            results <- evaluator.evaluate(inputDir, Some(outDir), debugDir, maxImages)
            _ <- ZIO.attempt {
              evaluator.writeResults(evalWriter, results)
            }
          } yield {
            results
          }
        }.getOrElse {
          jochre.process(inputDir, Some(outDir), debugDir, maxImages)
        }
      }
    } yield ExitCode.success

  override def run = {
    for {
      args <- getArgs
      result <- app(args).provide(
        segmenterService,
        textAnalyzerService,
        textGuesserService,
        jochreYiddishLayerInternal
      )
    } yield result
  }
}
