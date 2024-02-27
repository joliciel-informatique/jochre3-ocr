package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.alto.AltoTransformer
import com.joliciel.jochre.ocr.core.corpus.TextSimplifier
import com.joliciel.jochre.ocr.core.learning.GlyphGuesser
import com.joliciel.jochre.ocr.core.lexicon.Lexicon
import com.joliciel.jochre.ocr.core.segmentation.{FullYoloSegmenterService, SegmenterService, YoloPredictorService}
import com.joliciel.jochre.ocr.core.text.{FullSegmentationGuesserConfig, FullSegmentationGuesserService, TextGuesserService}
import com.joliciel.jochre.ocr.core.{AbstractJochre, Jochre, JochreAppBase, JochreCLI}
import com.joliciel.jochre.ocr.yiddish.learning.YiddishGlyphGuesser
import com.joliciel.jochre.ocr.yiddish.lexicon.{YivoLexicon, YivoLexiconService}
import sttp.client3.httpclient.zio.{HttpClientZioBackend, SttpClient}
import zio._

object JochreYiddishFull extends ZIOAppDefault with JochreAppBase {
  override val textSimplifier = Some(YiddishTextSimpifier(replaceNotYiddishAlphabets = false))

  private case class JochreYiddishImpl(segmenterService: SegmenterService, textGuesserService: TextGuesserService, yiddishConfig: YiddishConfig, yivoLexicon: YivoLexicon) extends AbstractJochre {
    override val altoTransformer: AltoTransformer = YiddishAltoTransformer(yiddishConfig, yivoLexicon)
  }

  private val sttpClient: ZLayer[Any, Throwable, SttpClient] = HttpClientZioBackend.layer()
  private val yoloPredictorService: ZLayer[SttpClient, Nothing, YoloPredictorService] = YoloPredictorService.live
  private val segmenterService: ZLayer[YoloPredictorService, Nothing, SegmenterService] = FullYoloSegmenterService.live
  private val glyphGuesserLayer: ZLayer[Any, Throwable, GlyphGuesser] = YiddishGlyphGuesser.live
  private val yivoLexiconService: ZLayer[YiddishConfig, Throwable, YivoLexiconService] = YivoLexiconService.live

  private val lexiconService: ZLayer[YivoLexiconService, Throwable, YivoLexicon] = ZLayer{
    for {
      yivoLexiconService <- ZIO.service[YivoLexiconService]
      yivoLexicon <- yivoLexiconService.getYivoLexicon
    } yield yivoLexicon
  }

  private val fullSegmentationConfigLayer = ZLayer.fromZIO(ZIO.attempt(FullSegmentationGuesserConfig.fromConfig))

  private val textSimplifierLayer: ZLayer[Any, Nothing, TextSimplifier] = ZLayer.succeed(textSimplifier.value)
  private val textGuesserService: ZLayer[GlyphGuesser with Lexicon with TextSimplifier with FullSegmentationGuesserConfig, Throwable, TextGuesserService] =
    FullSegmentationGuesserService.live

  private val jochreYiddishLayerInternal: ZLayer[SegmenterService with TextGuesserService with YiddishConfig with YivoLexicon, Throwable, Jochre] = ZLayer {
    for {
      yiddishConfig <- ZIO.service[YiddishConfig]
      yivoLexicon <- ZIO.service[YivoLexicon]
      segmenterService <- ZIO.service[SegmenterService]
      textGuesserService <- ZIO.service[TextGuesserService]
    } yield (JochreYiddishImpl(segmenterService, textGuesserService, yiddishConfig, yivoLexicon))
  }

  private val noDepSegmenterService: ZLayer[Any, Throwable, SegmenterService] = sttpClient >>> yoloPredictorService >>> segmenterService

  val jochreYiddishLayer: ZLayer[Any, Throwable, Jochre] = (YiddishConfig.configLayer >>> (yivoLexiconService ++ ZLayer.service[YiddishConfig]) >>>
    (noDepSegmenterService ++ ZLayer.service[YivoLexiconService] ++ ZLayer.service[YiddishConfig] ++
      (glyphGuesserLayer ++ textSimplifierLayer ++ lexiconService ++ fullSegmentationConfigLayer >>> textGuesserService ++ ZLayer.service[YivoLexicon]))) >>>
    jochreYiddishLayerInternal

  override def run = {
    for {
      args <- getArgs
      jochreCLI <- ZIO.attempt(new JochreCLI(args))
      result <- app(jochreCLI).provideSome[ZIOAppArgs](
        sttpClient,
        yoloPredictorService,
        segmenterService,
        glyphGuesserLayer,
        textSimplifierLayer,
        YiddishConfig.appArgsLayer,
        yivoLexiconService,
        lexiconService,
        FullSegmentationGuesserConfig.appArgsLayer,
        textGuesserService,
        jochreYiddishLayerInternal
      )
    } yield result
  }
}
