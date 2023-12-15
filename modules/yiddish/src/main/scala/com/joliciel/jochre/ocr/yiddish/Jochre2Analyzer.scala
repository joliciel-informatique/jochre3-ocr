package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.graphics.SegmentationException
import com.joliciel.jochre.ocr.core.analysis.TextAnalyzer
import com.joliciel.jochre.ocr.yiddish.YiddishConfig.yiddishConfig
import com.joliciel.jochre.yiddish.JochreYiddish
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import zio.{Config, ZIO, ZLayer}

import java.awt.image.BufferedImage
import java.io.StringWriter
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.xml.{Elem, XML}

object Jochre2Analyzer extends TextAnalyzer {
  private val log = LoggerFactory.getLogger(getClass)

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

  override def analyze(image: BufferedImage): Option[Elem] = {
    val writer = new StringWriter()
    try {
      jochreYiddish.imageToAlto4(image, "segment", writer)
      val altoString = writer.toString
      Some(XML.loadString(altoString))
    } catch {
      case se: SegmentationException =>
        log.error("Segmentation exception while analyzing OCR", se)
        None
      case e: Exception if e.getCause.isInstanceOf[SegmentationException] =>
        log.error("Wrapped segmentation exception while analyzing OCR", e.getCause)
        None
    } finally {
      writer.close()
    }
  }

  val live: ZLayer[Any, Config.Error, TextAnalyzer] = ZLayer.succeed(Jochre2Analyzer)
}
