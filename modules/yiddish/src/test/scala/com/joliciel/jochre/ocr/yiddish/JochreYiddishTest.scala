package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.Jochre
import zio._
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}

import javax.imageio.ImageIO

object JochreYiddishTest extends JUnitRunnableSpec {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("JochreYiddishTest")(
    test("analyze an image") {
      val inputStream = getClass.getClassLoader.getResourceAsStream("yiddish_sample.jpg")
      val image = ImageIO.read(inputStream)
      for {
        jochreYiddish <- ZIO.service[Jochre]
        alto <- jochreYiddish.processImage(image, None, "yiddish_sample.jpg")
      } yield {
        val content = (alto \\ "String").map(node => node \@ "CONTENT").mkString(" ")
        assertTrue(content == "מאַמע - לשון")
      }
    }
  ).provideLayer(JochreYiddish.jochreYiddishLayer) @@ TestAspect.sequential
}
