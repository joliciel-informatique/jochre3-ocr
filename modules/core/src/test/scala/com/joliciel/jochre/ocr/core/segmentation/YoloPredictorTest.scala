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
        blockPredictor <- yoloPredictorService.getYoloPredictor(
          YoloPredictionType.Blocks,
          mat,
          fileName,
          outputLocation,
          minConfidence = Some(0.7)
        )
        result <- blockPredictor.predict()
      } yield {
        log.info(f"Result: ${result.map(_.rectangle).mkString(", ")}")
        val expected = Seq(
          Rectangle(1792, 1442, 101, 151),
          Rectangle(708, 1620, 2295, 185),
          Rectangle(1771, 2037, 130, 139),
          Rectangle(615, 2240, 2430, 1919),
          Rectangle(1809, 4163, 84, 122)
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
