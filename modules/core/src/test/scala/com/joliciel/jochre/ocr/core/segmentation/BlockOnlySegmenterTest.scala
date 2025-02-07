package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.graphics.{PredictedRectangle, Rectangle}
import com.joliciel.jochre.ocr.core.model.{Illustration, TextBlock}
import com.joliciel.jochre.ocr.core.utils.{ImageUtils, OutputLocation}
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.opencv_core.Mat
import zio._
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestEnvironment, assertTrue}

import java.nio.file.Path
import javax.imageio.ImageIO

object BlockOnlySegmenterTest extends JUnitRunnableSpec with ImageUtils {
  object MockYoloPredictorService extends YoloPredictorService {
    override def getYoloPredictor: Task[YoloPredictor] = ZIO.attempt {
      new YoloPredictor {
        override def predict(
            predictionType: YoloPredictionType,
            mat: Mat,
            fileName: String,
            outputLocation: Option[OutputLocation],
            minConfidence: Option[Double],
            tileNumber: Option[Int]
        ): Task[Seq[PredictedRectangle]] = ZIO.attempt(
          Seq(
            PredictedRectangle(
              BlockType.TopLevelTextBlock.entryName,
              Rectangle(10, 10, 50, 50),
              0.9
            ),
            PredictedRectangle(
              BlockType.TopLevelTextBlock.entryName,
              Rectangle(60, 10, 100, 100),
              0.8
            ),
            PredictedRectangle(BlockType.Illustration.entryName, Rectangle(20, 120, 50, 50), 0.9)
          )
        )
      }
    }
  }

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("BlockPredictor")(
    test("transform blocks into page") {
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
        yoloPredictorService <- ZIO.succeed(MockYoloPredictorService)
        blockOnlySegmenter = new BlockOnlySegmenter(yoloPredictorService)
        result <- blockOnlySegmenter.segment(mat, fileName, outputLocation)
      } yield {
        val blocks = result.blocks.map {
          case textBlock: TextBlock       => textBlock.copy(id = "")
          case illustration: Illustration => illustration.copy(id = "")
        }
        val expected = Seq(
          TextBlock(Rectangle(60, 10, 100, 100), Seq.empty, id = "", defaultLanguage = Some("yi")),
          TextBlock(Rectangle(10, 10, 50, 50), Seq.empty, id = "", defaultLanguage = Some("yi")),
          Illustration(Rectangle(20, 120, 50, 50), id = "")
        )
        assertTrue(blocks == expected)
      }
    }
  )
}
