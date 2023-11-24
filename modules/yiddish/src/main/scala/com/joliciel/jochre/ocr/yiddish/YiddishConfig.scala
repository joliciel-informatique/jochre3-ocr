package com.joliciel.jochre.ocr.yiddish

import com.typesafe.config.ConfigFactory
import zio.config._
import zio.config.magnolia._
import zio.{Config, ZIO, ZLayer}

case class YiddishConfig(
  lexiconPath: String,
  letterModelPath: String,
  addHyphenElement: Boolean,
)

object YiddishConfig {
  private val config = ConfigFactory.load().getConfig("jochre.ocr.yiddish")
  val fromConfig: YiddishConfig = YiddishConfig(
    config.getString("lexicon-path"),
    config.getString("letter-model-path"),
    config.getBoolean("add-hyphen-element"),
  )

  val yiddishConfig: Config[YiddishConfig] = deriveConfig[YiddishConfig].mapKey(toKebabCase).nested("yiddish")

  val live: ZLayer[Any, Config.Error, YiddishConfig] = ZLayer.fromZIO(ZIO.config(yiddishConfig))
}