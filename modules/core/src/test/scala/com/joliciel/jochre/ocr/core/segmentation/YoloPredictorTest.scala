package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.utils.{ImageUtils, OutputLocation}
import com.typesafe.config.ConfigFactory
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio._
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestEnvironment, assertTrue}

import java.nio.file.Path
import javax.imageio.ImageIO

object YoloPredictorTest extends JUnitRunnableSpec with ImageUtils {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("BlockPredictor")(
    test("predict blocks") {
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
        blockPredictor <- yoloPredictorService.getYoloPredictor(YoloPredictionType.Blocks, mat, fileName, outputLocation)
        result <- blockPredictor.predict()
      } yield {
        val expected = Seq(
          Rectangle("Paragraph", 600, 2254, 2477, 605),
          Rectangle("Paragraph", 608, 2880, 2468, 1281),
          Rectangle("TextBox", 671, 1610, 2370, 214),
          Rectangle("TextBox", 1765, 2040, 154, 117),
          Rectangle("TextBox", 1784, 4176, 121, 93)
        )
        assertTrue(result.forall(rect => expected.find(other => rect.rectangle.iou(other) > 0.9).isDefined))
      }
    }
  ).provide(
    HttpClientZioBackend.layer() >>> YoloPredictorService.live
  )
}
