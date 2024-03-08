package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.Jochre
import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.model.Page
import com.joliciel.jochre.ocr.core.utils.XmlImplicits
import org.slf4j.LoggerFactory
import zio._
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}

import javax.imageio.ImageIO
import scala.xml.{Atom, PrettyPrinter, Text}

object JochreYiddishFullTest extends JUnitRunnableSpec with XmlImplicits {
  private val log = LoggerFactory.getLogger(getClass)

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("JochreYiddishFull")(
    test("analyze an image") {
      val inputStream = getClass.getClassLoader.getResourceAsStream("nybc200089_0011.png")
      val image = ImageIO.read(inputStream)
      for {
        jochreYiddish <- ZIO.service[Jochre]
        alto <- jochreYiddish.processImage(image, "nybc200089_0011.png")
      } yield {
        if (log.isDebugEnabled) {
          val prettyPrinter = new PrettyPrinter(80, 2)
          log.debug(prettyPrinter.format(alto.toXml))
        }
        val testRectangle = Rectangle(732, 1638, 2319, 240)
        val page = alto.pages.head
        val myTextBlock = page.textBlocks.find(_.rectangle.percentageIntersection(testRectangle) > 0.75)

        assertTrue(myTextBlock.isDefined)

        val content = myTextBlock.get.content
        assertTrue(content == "הײנט איז יום־טוב — מע טאָר נישט װײנען !")
      }
    }
  ).provide(JochreYiddishFull.jochreYiddishLayer) @@ TestAspect.sequential
}
