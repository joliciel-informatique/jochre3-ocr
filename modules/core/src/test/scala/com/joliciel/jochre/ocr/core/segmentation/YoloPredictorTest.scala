package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.utils.{ImageUtils, OutputLocation}
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio._
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestEnvironment, assertTrue}

import java.nio.file.Path
import javax.imageio.ImageIO

object YoloPredictorTest extends JUnitRunnableSpec with ImageUtils {
  private val log = LoggerFactory.getLogger(getClass)

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("YoloPredictor")(
    test("predict blocks") {
      val image =
        ImageIO.read(getClass.getResourceAsStream("/images/nybc200089_0011_deskewered.jpg"))
      val mat = fromBufferedImage(image)
      val fileName = "nybc200089_0011_deskewered.jpg"
      val config = ConfigFactory.load()
      val basePath = Path.of(config.getString("jochre.ocr.directory.core"))
      val outputPath = basePath.resolve("temp/output")
      outputPath.toFile.mkdirs()
      val outputLocation = Some(OutputLocation(outputPath, "nybc200089_0011"))
      for {
        yoloPredictorService <- ZIO.service[YoloPredictorService]
        blockPredictor <- yoloPredictorService.getYoloPredictor
        result <- blockPredictor.predict(
          YoloPredictionType.Blocks,
          mat,
          fileName,
          outputLocation,
          minConfidence = Some(0.7)
        )
      } yield {
        log.info(f"Result: ${result.map(_.rectangle).sortBy(_.top).mkString(", ")}")
        val expected = Seq(
          Rectangle(1780, 1468, 126, 109),
          Rectangle(675, 1653, 2269, 126),
          Rectangle(1771, 2058, 135, 84),
          Rectangle(615, 2295, 2438, 1839),
          Rectangle(1797, 4185, 101, 75)
        )
        assertTrue(
          result.forall(rect => expected.exists(other => rect.rectangle.iou(other) > 0.8))
        )
      }
    }
  ).provide(
    HttpClientZioBackend.layer() >>> YoloPredictorService.live
  )
}
