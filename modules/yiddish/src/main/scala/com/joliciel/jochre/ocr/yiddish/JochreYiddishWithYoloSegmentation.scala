package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.alto.AltoTransformer
import com.joliciel.jochre.ocr.core.segmentation.{FullYoloSegmenterService, SegmenterService, YoloPredictorService}
import com.joliciel.jochre.ocr.core.text.TextGuesserService
import com.joliciel.jochre.ocr.core.{AbstractJochre, Jochre, JochreAppBase, JochreCLI}
import sttp.client3.httpclient.zio.{HttpClientZioBackend, SttpClient}
import zio._

object JochreYiddishWithYoloSegmentation extends ZIOAppDefault with JochreAppBase {
  override val textSimplifier = Some(YiddishTextSimpifier(replaceNonHebrewAlphabets = false))
  val yiddishConfig = YiddishConfig.fromConfig

  private case class JochreYiddishImpl(segmenterService: SegmenterService, textGuesserService: TextGuesserService) extends AbstractJochre {
    override val altoTransformer: AltoTransformer = YiddishAltoTransformer(yiddishConfig)
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


  override def run = {
    for {
      args <- getArgs
      jochreCLI <- ZIO.attempt(new JochreCLI(args))
      result <- app(jochreCLI).provide(
        sttpClient,
        yoloPredictorService,
        segmenterService,
        textGuesserService,
        jochreYiddishLayerInternal
      )
    } yield result
  }
}
