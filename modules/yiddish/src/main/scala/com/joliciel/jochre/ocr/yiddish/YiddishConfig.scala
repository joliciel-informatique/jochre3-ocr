package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.JochreCLI
import com.typesafe.config.ConfigFactory
import zio.config._
import zio.config.magnolia._
import zio.{Config, ZIO, ZIOAppArgs, ZLayer}

case class YiddishConfig(
  lexiconPath: Option[String],
  addHyphenElement: Boolean,
)

object YiddishConfig {
  private val config = ConfigFactory.load().getConfig("jochre.ocr.yiddish")
  val fromConfig: YiddishConfig = YiddishConfig(
    Some(config.getString("lexicon-path")),
    config.getBoolean("add-hyphen-element"),
  )

  val configLayer: ZLayer[Any, Throwable, YiddishConfig] = ZLayer.fromZIO {
    ZIO.attempt {
      fromConfig
    }
  }

  val appArgsLayer: ZLayer[ZIOAppArgs, Throwable, YiddishConfig] = ZLayer.fromZIO {
    for {
      args <- ZIOAppArgs.getArgs
      jochreCLI <- ZIO.attempt {
        new JochreCLI(args)
      }
    } yield {
      fromConfig.copy(
        lexiconPath = jochreCLI.lexiconDir.toOption,
      )
    }
  }
}