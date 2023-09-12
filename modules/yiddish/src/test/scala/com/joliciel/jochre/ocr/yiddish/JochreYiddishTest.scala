package com.joliciel.jochre.ocr.yiddish

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import javax.imageio.ImageIO

class JochreYiddishTest extends AnyFlatSpec with Matchers {
  "A JochreYiddish" should "analyze an image" in {
    val inputStream = getClass.getClassLoader.getResourceAsStream("yiddish_sample.jpg")
    val image = ImageIO.read(inputStream)
    val jochreYiddish = JochreYiddish()
    val alto = jochreYiddish.processImage(image, "yiddish_sample.jpg")
    val content = (alto \\ "String").map(node => node \@ "CONTENT").mkString(" ")

    content shouldEqual "מאַמע-לשון"
  }
}
