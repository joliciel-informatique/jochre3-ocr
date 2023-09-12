package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.analysis.TextAnalyzer
import com.joliciel.jochre.yiddish.JochreYiddish
import com.typesafe.config.ConfigFactory

import java.awt.image.BufferedImage
import java.io.{PipedInputStream, PipedOutputStream, StringWriter}
import javax.imageio.ImageIO
import scala.collection.mutable
import scala.concurrent.Future
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

  import scala.concurrent.ExecutionContext.Implicits.global

  override def analyze(image: BufferedImage): Elem = {
    val out = new PipedOutputStream()
    val in = new PipedInputStream(out)

    Future {
      try {
        ImageIO.write(image, "png", out)
        out.flush()
      } finally {
        out.close()
      }
    }

    val writer = new StringWriter()
    try {
      jochreYiddish.imageInputStreamToAlto4(in, "segment", writer)
      val altoString = writer.toString
      XML.loadString(altoString)
    } finally {
      in.close()
      writer.close()
    }
  }
}
