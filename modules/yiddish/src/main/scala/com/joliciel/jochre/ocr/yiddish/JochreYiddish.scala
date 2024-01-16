package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.alto.AltoTransformer
import com.joliciel.jochre.ocr.core.evaluation.{CharacterCount, CharacterErrorRate, Evaluator}
import com.joliciel.jochre.ocr.core.segmentation.{FullYoloSegmenterService, SegmenterService, YoloPredictorService}
import com.joliciel.jochre.ocr.core.text.TextGuesserService
import com.joliciel.jochre.ocr.core.{AbstractJochre, Jochre}
import org.rogach.scallop.{ScallopConf, ScallopOption}
import sttp.client3.httpclient.zio.{HttpClientZioBackend, SttpClient}
import zio._

import java.io.{File, FileWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Path

object JochreYiddish extends ZIOAppDefault {
  private case class JochreYiddishImpl(segmenterService: SegmenterService, textGuesserService: TextGuesserService) extends AbstractJochre {
    override val altoTransformer: AltoTransformer = YiddishAltoTransformer()
  }

  private val sttpClient: ZLayer[Any, Throwable, SttpClient] = HttpClientZioBackend.layer()
  private val yoloPredictorService: ZLayer[SttpClient, Nothing, YoloPredictorService] = YoloPredictorService.live
  private val segmenterService: ZLayer[YoloPredictorService, Nothing, SegmenterService] = FullYoloSegmenterService.live
  private val textGuesserService: ZLayer[Any, Nothing, TextGuesserService] = Jochre2LetterGuesserService.live

  private val jochreYiddishLayerInternal: ZLayer[SegmenterService with TextGuesserService, Throwable, Jochre] = ZLayer {
    for {
      segmenterService <- ZIO.service[SegmenterService]
      textGuesserService <- ZIO.service[TextGuesserService]
    } yield (JochreYiddishImpl(segmenterService, textGuesserService))
  }

  private val noDepSegmenterService: ZLayer[Any, Throwable, SegmenterService] = sttpClient >>> yoloPredictorService >>> segmenterService
  val jochreYiddishLayer: ZLayer[Any, Throwable, Jochre] = (noDepSegmenterService ++ textGuesserService) >>> jochreYiddishLayerInternal

  class JochreYiddishCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val inputDir: ScallopOption[String] = opt[String](required = true)
    val outputDir: ScallopOption[String] = opt[String](required = true)
    val maxImages: ScallopOption[Int] = opt[Int](default = Some(0))
    val evalDir: ScallopOption[String] = opt[String]()
    verify()
  }

  def app(args: Chunk[String]) =
    for {
      options <- ZIO.attempt(new JochreYiddishCLI(args))
      inputDir = Path.of(options.inputDir())
      outDir = Path.of(options.outputDir())
      evalDir = options.evalDir.toOption.map(Path.of(_))
      maxImages = Option.when(options.maxImages() > 0) (options.maxImages())
      _ <- ZIO.attempt{
        evalDir.foreach(_.toFile.mkdirs())
        outDir.toFile.mkdirs()
      }
      _ <- ZIO.serviceWithZIO[Jochre]{ jochre =>
        evalDir.map{ evalDir =>
          val evaluator = Evaluator(jochre, Seq(CharacterErrorRate, CharacterCount), evalDir, textSimplifier = Some(YiddishTextSimpifier))
          val evalWriter = new FileWriter(new File(evalDir.toFile, "eval.tsv"), StandardCharsets.UTF_8)
          for {
            results <- evaluator.evaluate(inputDir, Some(outDir), maxImages)
            _ <- ZIO.attempt{evaluator.writeResults(evalWriter, results)}
          } yield {
            results
          }
        }.getOrElse {
          jochre.process(inputDir, Some(outDir), maxImages)
        }
      }
    } yield ExitCode.success

  override def run = {
    for {
      args <- getArgs
      result <- app(args).provide(
        sttpClient,
        yoloPredictorService,
        segmenterService,
        textGuesserService,
        jochreYiddishLayerInternal
      )
    } yield result
  }
}
