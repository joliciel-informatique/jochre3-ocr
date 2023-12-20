package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.Jochre
import com.joliciel.jochre.ocr.core.segmentation.{BlockOnlySegmenterService, YoloPredictorService}
import com.joliciel.jochre.ocr.core.text.BlockTextGuesserService
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio._
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}

import javax.imageio.ImageIO
import scala.xml.Text

object JochreYiddishTest extends JUnitRunnableSpec {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("JochreYiddishTest")(
    test("analyze an image") {
      val inputStream = getClass.getClassLoader.getResourceAsStream("yiddish_sample.jpg")
      val image = ImageIO.read(inputStream)
      for {
        jochreYiddish <- ZIO.service[Jochre]
        alto <- jochreYiddish.processImage(image, None, "yiddish_sample.jpg")
      } yield {
        val altoFileName = (alto \\ "fileName").head.child.collect{case text: Text => text}.map(_.text).head
        assertTrue(altoFileName == "yiddish_sample.jpg")

        val content = (alto \\ "String").map(node => node \@ "CONTENT").mkString(" ")
        assertTrue(content == "מאַמע - לשון")

        val wcs = (alto \\ "String").map(node => node \@ "WC").map(_.toDouble)
        assertTrue(wcs.forall(_>0.0))
      }
    }
  ).provide(HttpClientZioBackend.layer(),
      Jochre2Analyzer.live,
      YoloPredictorService.live,
      BlockOnlySegmenterService.live,
      BlockTextGuesserService.live,
      JochreYiddish.jochreYiddishLayer) @@ TestAspect.sequential
}
