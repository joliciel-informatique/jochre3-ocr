package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.Jochre
import com.joliciel.jochre.ocr.core.utils.XmlImplicits
import org.slf4j.LoggerFactory
import zio._
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}

import javax.imageio.ImageIO
import scala.xml.{PrettyPrinter, Text}

object JochreYiddishWithYoloBlocksOnlyTest extends JUnitRunnableSpec with XmlImplicits {
  private val log = LoggerFactory.getLogger(getClass)

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("JochreYiddishWithYoloBlocksOnlyTest")(
    test("analyze an image") {
      val inputStream = getClass.getClassLoader.getResourceAsStream("yiddish_sample.jpg")
      val image = ImageIO.read(inputStream)
      for {
        jochreYiddish <- ZIO.service[Jochre]
        page <- jochreYiddish.processImage(image, "yiddish_sample.jpg")
      } yield {
        if (log.isDebugEnabled) {
          val prettyPrinter = new PrettyPrinter(80, 2)
          log.debug(prettyPrinter.format(page.toXml()))
        }

        assertTrue(page.content == "מאַמע - לשון")

        val wcs = page.allWords.map(_.confidence)
        assertTrue(wcs.forall(_>0.0))
      }
    }
  ).provide(
      JochreYiddishWithYoloBlocksOnly.jochreYiddishLayer) @@ TestAspect.sequential
}
