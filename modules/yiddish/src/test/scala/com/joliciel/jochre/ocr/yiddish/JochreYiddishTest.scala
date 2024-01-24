package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.Jochre
import org.slf4j.LoggerFactory
import zio._
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}

import javax.imageio.ImageIO
import scala.xml.{PrettyPrinter, Text}

object JochreYiddishTest extends JUnitRunnableSpec {
  private val log = LoggerFactory.getLogger(getClass)

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("JochreYiddishTest")(
    test("analyze an image") {
      val inputStream = getClass.getClassLoader.getResourceAsStream("yiddish_sample.jpg")
      val image = ImageIO.read(inputStream)
      for {
        jochreYiddish <- ZIO.service[Jochre]
        alto <- jochreYiddish.processImage(image, None, None, "yiddish_sample.jpg", testRectangle = None)
      } yield {
        val altoFileName = (alto \\ "fileName").head.child.collect{case text: Text => text}.map(_.text).head
        assertTrue(altoFileName == "yiddish_sample.jpg")

        val content = (alto \\ "String").map(node => node \@ "CONTENT").mkString(" ")
        assertTrue(content == "מאַמע - לשון")

        val prettyPrinter = new PrettyPrinter(80, 2)
        log.debug(prettyPrinter.format(alto))

        val wcs = (alto \\ "String").map(node => node \@ "WC").map(_.toDouble)
        assertTrue(wcs.forall(_>0.0))
      }
    }
  ).provide(
      JochreYiddishWithYoloBlocksOnly.jochreYiddishLayer) @@ TestAspect.sequential
}
