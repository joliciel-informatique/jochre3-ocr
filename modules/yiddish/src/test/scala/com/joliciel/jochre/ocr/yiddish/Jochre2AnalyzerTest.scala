package com.joliciel.jochre.ocr.yiddish

import zio._
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestEnvironment, assertTrue}

import javax.imageio.ImageIO

object Jochre2AnalyzerTest extends JUnitRunnableSpec {

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("JochreYiddishTest")(
    test("analyze an image") {
      val inputStream = getClass.getClassLoader.getResourceAsStream("yiddish_sample.jpg")
      val image = ImageIO.read(inputStream)
      for {
        alto <- Jochre2Analyzer.analyze(image)
      } yield {
        val content = (alto \\ "String").map(node => node \@ "CONTENT").mkString(" ")
        assertTrue(content == "מאַמע-לשון")
      }
   })
}
