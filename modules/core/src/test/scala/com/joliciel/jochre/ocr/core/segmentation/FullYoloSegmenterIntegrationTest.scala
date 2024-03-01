package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.model.{Illustration, TextBlock}
import com.joliciel.jochre.ocr.core.utils.{ImageUtils, OutputLocation}
import com.typesafe.config.ConfigFactory
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio._
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestEnvironment, assertTrue}

import java.nio.file.Path
import javax.imageio.ImageIO

object FullYoloSegmenterIntegrationTest extends JUnitRunnableSpec with ImageUtils  {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("BlockPredictor")(
    test("transform blocks into page") {
      val image = ImageIO.read(getClass.getResourceAsStream("/images/nybc200089_0011_deskewered.jpg"))
      val mat = fromBufferedImage(image)
      val fileName = "nybc200089_0011_deskewered.jpg"
      val config = ConfigFactory.load()
      val basePath = Path.of(config.getString("jochre.ocr.directory.core"))
      val outputPath = basePath.resolve("temp/output")
      outputPath.toFile.mkdirs()
      val outputLocation = Some(OutputLocation(outputPath, "nybc200089_0011"))
      for {
        yoloPredictorService <- ZIO.service[YoloPredictorService]
        fullYoloSegmenter = new FullYoloSegmenter(yoloPredictorService)
        result <- fullYoloSegmenter.segment(mat, fileName, outputLocation)
      } yield {
        val expectedBlock = Rectangle(636, 2272, 2450, 622)
        val foundBlock = result.textBlocks.find(_.rectangle.percentageIntersection(expectedBlock) > 0.9)
        assertTrue(foundBlock.isDefined
          && foundBlock.get.textLines.size==5
          && foundBlock.get.textLines(0).words.size==11
          && foundBlock.get.textLines(0).words(5).glyphs.size==6
        )
      }
    }
  ).provide(
    HttpClientZioBackend.layer() >>> YoloPredictorService.live
  )
}
