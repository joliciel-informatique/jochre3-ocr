package com.joliciel.jochre.ocr.yiddish.learning

import com.joliciel.jochre.ocr.core.learning.{GlyphGuesser, GlyphGuesserForAnotherAlphabet, GlyphGuessersForOtherAlphabets}
import com.joliciel.jochre.ocr.yiddish.learning.YiddishGlyphTrainer.ModelType
import com.typesafe.config.ConfigFactory
import zio.ZLayer

import java.nio.file.Path
import scala.jdk.CollectionConverters._


object YiddishGlyphGuessersForOtherAlphabets {
  private val configs = ConfigFactory.load().getConfigList("jochre.ocr.yiddish.glyph-guesser-for-other-alphabets").asScala

  private val modelDir = Path.of(ClassLoader.getSystemResource("yiddish/models").toURI())

  private val guessers = configs.map{config =>
    val language = config.getString("language")
    val regex = config.getString("regex").r
    val modelName = config.getString("model-name")
    val modelType = ModelType.withName(config.getString("model-type")).glyphTrainerModelType
    val glyphGuesser = GlyphGuesser(modelDir, modelName, modelType)
    GlyphGuesserForAnotherAlphabet(language, regex, glyphGuesser)
  }.toSeq

  private val glyphGuessersForOtherAlphabets = GlyphGuessersForOtherAlphabets(guessers)


  val live: ZLayer[Any, Throwable, GlyphGuessersForOtherAlphabets] = ZLayer.succeed(glyphGuessersForOtherAlphabets)
}
