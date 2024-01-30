package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.graphics.SegmentationException
import com.joliciel.jochre.ocr.core.alto.ImageToAltoConverter
import com.joliciel.jochre.ocr.core.text.AnalysisExceptionToIgnore
import com.joliciel.jochre.yiddish.JochreYiddish
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import zio.{Config, Task, ZIO, ZLayer}

import java.awt.image.BufferedImage
import java.io.StringWriter
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.xml.{Elem, XML}

object Jochre2Analyzer extends ImageToAltoConverter {
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

  override def analyze(image: BufferedImage): Task[Elem] = ZIO.attempt{
    val writer = new StringWriter()
    try {
      jochreYiddish.imageToAlto4(image, "segment", writer)
      val altoString = writer.toString
      XML.loadString(altoString)
    } catch {
      case se: SegmentationException =>
        log.error("Segmentation exception while analyzing OCR", se)
        throw new AnalysisExceptionToIgnore(se)
      case e: Exception if e.getCause.isInstanceOf[SegmentationException] =>
        log.error("Wrapped segmentation exception while analyzing OCR", e.getCause)
        throw new AnalysisExceptionToIgnore(e.getCause)
    } finally {
      writer.close()
    }
  }

  val live: ZLayer[Any, Config.Error, ImageToAltoConverter] = ZLayer.succeed(Jochre2Analyzer)
}
