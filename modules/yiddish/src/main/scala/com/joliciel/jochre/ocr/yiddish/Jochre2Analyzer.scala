package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.analysis.TextAnalyzer
import com.joliciel.jochre.yiddish.JochreYiddish
import com.typesafe.config.ConfigFactory

import java.awt.image.BufferedImage
import java.io.StringWriter
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.xml.{Elem, XML}

object Jochre2Analyzer extends TextAnalyzer {
  private val yiddishConfig = ConfigFactory.load().getConfig("jochre.ocr.yiddish")
  private val lexiconPath = yiddishConfig.getString("lexicon-path")
  private val letterModelPath = yiddishConfig.getString("letter-model-path")

  private val jochreConfig = ConfigFactory.load()
  private val args = mutable.Map(
    "lexicon" -> lexiconPath,
    "letterModel" -> letterModelPath,
    "isCleanSegment" -> "true"
  )
  private val jochreYiddish: JochreYiddish = new JochreYiddish(jochreConfig, args.asJava)

  override def analyze(image: BufferedImage): Elem = {
    val writer = new StringWriter()
    try {
      jochreYiddish.imageToAlto4(image, "segment", writer)
      val altoString = writer.toString
      XML.loadString(altoString)
    } finally {
      writer.close()
    }
  }
}
