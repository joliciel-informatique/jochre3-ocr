package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.{AbstractJochre, Jochre}
import com.joliciel.jochre.ocr.core.analysis.{AltoTransformer, TextAnalyzer}
import com.joliciel.jochre.ocr.core.segmentation.{BlockOnlySegmenterService, YoloPredictorService, SegmenterService}
import com.joliciel.jochre.ocr.core.text.{BlockTextGuesserService, TextGuesserService}
import org.rogach.scallop.{ScallopConf, ScallopOption}
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio._

import java.nio.file.Path

object JochreYiddish extends ZIOAppDefault {
  private case class JochreYiddishImpl(segmenterService: SegmenterService, textGuesserService: TextGuesserService) extends AbstractJochre {
    override val altoTransformer: AltoTransformer = YiddishAltoTransformer()
  }

  val jochreYiddishLayer: ZLayer[SegmenterService with TextGuesserService, Throwable, Jochre] = ZLayer {
    for {
      segmenterService <- ZIO.service[SegmenterService]
      textGuesserService <- ZIO.service[TextGuesserService]
    } yield (JochreYiddishImpl(segmenterService, textGuesserService))
  }

  class JochreYiddishCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val inputDir: ScallopOption[String] = opt[String](required = true)
    val outputDir: ScallopOption[String] = opt[String](required = true)
    val maxImages: ScallopOption[Int] = opt[Int](default = Some(0))
    verify()
  }

  def app(args: Chunk[String]) =
    for {
      options <- ZIO.attempt(new JochreYiddishCLI(args))
      inputDir = Path.of(options.inputDir())
      outDir = Path.of(options.outputDir())
      maxImages = Option.when(options.maxImages() > 0) (options.maxImages())
      _ <- ZIO.attempt(outDir.toFile.mkdirs())
      _ <- ZIO.serviceWithZIO[Jochre](_.process(inputDir, Some(outDir), maxImages))
    } yield ExitCode.success

  override def run = {
    for {
      args <- getArgs
      result <- app(args).provide(
        HttpClientZioBackend.layer(),
        Jochre2Analyzer.live,
        YoloPredictorService.live,
        BlockOnlySegmenterService.live,
        BlockTextGuesserService.live,
        jochreYiddishLayer
      )
    } yield result
  }
}
