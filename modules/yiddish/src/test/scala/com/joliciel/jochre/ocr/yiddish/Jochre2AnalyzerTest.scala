package com.joliciel.jochre.ocr.yiddish

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import javax.imageio.ImageIO

class Jochre2AnalyzerTest extends AnyFlatSpec with Matchers {
  "A Jochre2Analyzer" should "analyze an image" in {
    val inputStream = getClass.getClassLoader.getResourceAsStream("yiddish_sample.jpg")
    val image = ImageIO.read(inputStream)
    val alto = Jochre2Analyzer.analyze(image)
    val content = (alto \\ "String").map(node => node \@ "CONTENT").mkString(" ")

    content shouldEqual "מאַמע-לשון"
  }
}
