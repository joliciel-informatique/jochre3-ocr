package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.utils.{OpenCvUtils, OutputLocation}
import com.typesafe.config.ConfigFactory
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio._
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestEnvironment, assertTrue}

import java.nio.file.Path
import javax.imageio.ImageIO

object BlockPredictorTest extends JUnitRunnableSpec with OpenCvUtils {
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
        blockPredictorService <- ZIO.service[BlockPredictorService]
        blockPredictor <- blockPredictorService.getBlockPredictor(mat, fileName, outputLocation)
        result <- blockPredictor.predict()
      } yield {
        val expected = Seq(
          Rectangle("paragraph", 600, 2254, 2477, 605),
          Rectangle("paragraph", 608, 2880, 2468, 1281),
          Rectangle("textbox", 671, 1610, 2370, 214),
          Rectangle("textbox", 1765, 2040, 154, 117),
          Rectangle("textbox", 1784, 4176, 121, 93)
        )
        assertTrue(result.forall(rect => expected.find(other => rect.iou(other) > 0.9).isDefined))
      }
    }
  ).provide(
    HttpClientZioBackend.layer() >>> BlockPredictorService.live
  )
}
