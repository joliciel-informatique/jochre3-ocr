package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.alto.{AltoTransformer, ImageToAltoConverter}
import com.joliciel.jochre.ocr.core.segmentation.{NonSegmenterService, SegmenterService}
import com.joliciel.jochre.ocr.core.text.{TextGuesserService, UnsegmentedPageTextGuesserService}
import com.joliciel.jochre.ocr.core.{AbstractJochre, Jochre, JochreAppBase, JochreCLI}
import zio._

object JochreYiddishWithoutYolo extends ZIOAppDefault with JochreAppBase {
  override val textSimplifier = Some(YiddishTextSimpifier(replaceNonHebrewAlphabets = false))
  val yiddishConfig = YiddishConfig.fromConfig

  private case class JochreYiddishImpl(segmenterService: SegmenterService, textGuesserService: TextGuesserService) extends AbstractJochre {
    override val altoTransformer: AltoTransformer = YiddishAltoTransformer(yiddishConfig)
  }

  private val segmenterService: ZLayer[Any, Nothing, SegmenterService] = NonSegmenterService.live
  private val textAnalyzerService = Jochre2Analyzer.live
  private val textGuesserService: ZLayer[ImageToAltoConverter, Nothing, TextGuesserService] = UnsegmentedPageTextGuesserService.live

  private val jochreYiddishLayerInternal: ZLayer[SegmenterService with TextGuesserService, Throwable, Jochre] = ZLayer {
    for {
      segmenterService <- ZIO.service[SegmenterService]
      textGuesserService <- ZIO.service[TextGuesserService]
    } yield (JochreYiddishImpl(segmenterService, textGuesserService))
  }

  private val noDepTextGuesserService = textAnalyzerService >>> textGuesserService
  val jochreYiddishLayer: ZLayer[Any, Throwable, Jochre] = (segmenterService ++ noDepTextGuesserService) >>> jochreYiddishLayerInternal

  override def run = {
    for {
      args <- getArgs
      jochreCLI <- ZIO.attempt(new JochreCLI(args))
      result <- app(jochreCLI).provide(
        segmenterService,
        textAnalyzerService,
        textGuesserService,
        jochreYiddishLayerInternal
      )
    } yield result
  }
}
