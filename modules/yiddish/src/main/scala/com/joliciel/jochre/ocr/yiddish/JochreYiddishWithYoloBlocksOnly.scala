package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.alto.{AltoTransformer, ImageToAltoConverter}
import com.joliciel.jochre.ocr.core.segmentation.{BlockOnlySegmenterService, SegmenterService, YoloPredictorService}
import com.joliciel.jochre.ocr.core.text.{BlockTextGuesserService, TextGuesserService}
import com.joliciel.jochre.ocr.core.{AbstractJochre, Jochre, JochreAppBase}
import sttp.client3.httpclient.zio.{HttpClientZioBackend, SttpClient}
import zio._

object JochreYiddishWithYoloBlocksOnly extends ZIOAppDefault with JochreAppBase {
  override val textSimplifier = Some(YiddishTextSimpifier(replaceNotYiddishAlphabets = false))

  private case class JochreYiddishImpl(segmenterService: SegmenterService, textGuesserService: TextGuesserService) extends AbstractJochre {
    override val altoTransformer: AltoTransformer = YiddishAltoTransformer()
  }

  private val sttpClient: ZLayer[Any, Throwable, SttpClient] = HttpClientZioBackend.layer()
  private val yoloPredictorService: ZLayer[SttpClient, Nothing, YoloPredictorService] = YoloPredictorService.live
  private val segmenterService: ZLayer[YoloPredictorService, Nothing, SegmenterService] = BlockOnlySegmenterService.live
  private val textAnalyzerService = Jochre2Analyzer.live
  private val textGuesserService: ZLayer[ImageToAltoConverter, Nothing, TextGuesserService] = BlockTextGuesserService.live

  private val jochreYiddishLayerInternal: ZLayer[SegmenterService with TextGuesserService, Throwable, Jochre] = ZLayer {
    for {
      segmenterService <- ZIO.service[SegmenterService]
      textGuesserService <- ZIO.service[TextGuesserService]
    } yield (JochreYiddishImpl(segmenterService, textGuesserService))
  }

  private val noDepSegmenterService: ZLayer[Any, Throwable, SegmenterService] = sttpClient >>> yoloPredictorService >>> segmenterService
  private val noDepTextGuesserService = textAnalyzerService >>> textGuesserService
  val jochreYiddishLayer: ZLayer[Any, Throwable, Jochre] = (noDepSegmenterService ++ noDepTextGuesserService) >>> jochreYiddishLayerInternal

  override def run = {
    for {
      args <- getArgs
      result <- app(args).provide(
        sttpClient,
        yoloPredictorService,
        segmenterService,
        textAnalyzerService,
        textGuesserService,
        jochreYiddishLayerInternal
      )
    } yield result
  }
}
