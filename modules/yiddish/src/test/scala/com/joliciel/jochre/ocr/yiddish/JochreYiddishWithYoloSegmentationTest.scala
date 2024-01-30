package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.Jochre
import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.model.Page
import org.slf4j.LoggerFactory
import zio._
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}

import javax.imageio.ImageIO
import scala.xml.{PrettyPrinter, Text}

object JochreYiddishWithYoloSegmentationTest extends JUnitRunnableSpec {
  private val log = LoggerFactory.getLogger(getClass)

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("JochreYiddishWithYoloSegmentationTest")(
    test("analyze an image") {
      val inputStream = getClass.getClassLoader.getResourceAsStream("nybc200089_0011.png")
      val image = ImageIO.read(inputStream)
      for {
        jochreYiddish <- ZIO.service[Jochre]
        alto <- jochreYiddish.processImage(image, None, None, "nybc200089_0011.png", testRectangle = None)
      } yield {
        val altoFileName = (alto \\ "fileName").head.child.collect{case text: Text => text}.map(_.text).head
        assertTrue(altoFileName == "nybc200089_0011.png")

        val page = Page.fromXML(alto)
        val testRectangle = Rectangle("", 732, 1638, 2319, 240)
        val myTextBlock = page.textBlocks.filter(_.rectangle.percentageIntersection(testRectangle) > 0.75).headOption

        assertTrue(myTextBlock.isDefined)

        val content = myTextBlock.get.content
        assertTrue(content == "הײנט איז יום־טוב מע טאָר נישט װײנען !")
      }
    }
  ).provide(
      JochreYiddishWithYoloSegmentation.jochreYiddishLayer) @@ TestAspect.sequential
}
