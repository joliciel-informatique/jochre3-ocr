package com.joliciel.jochre.ocr.yiddish.learning

import com.joliciel.jochre.ocr.core.learning.GlyphGuesser
import com.joliciel.jochre.ocr.yiddish.learning.YiddishGlyphTrainer.ModelType
import com.typesafe.config.ConfigFactory
import zio.ZLayer

import java.nio.file.Path

object YiddishGlyphGuesser {
  private val yiddishConfig = ConfigFactory.load().getConfig("jochre.ocr.yiddish.glyph-guesser")

  private val modelDir = Path.of(yiddishConfig.getString("model-path"))
  private val modelName = yiddishConfig.getString("model-name")
  private val modelType = ModelType.withName(yiddishConfig.getString("model-type")).glyphTrainerModelType
  private val glyphGuesser = GlyphGuesser(modelDir, modelName, modelType)

  val live: ZLayer[Any, Throwable, GlyphGuesser] = ZLayer.succeed(glyphGuesser)
}
