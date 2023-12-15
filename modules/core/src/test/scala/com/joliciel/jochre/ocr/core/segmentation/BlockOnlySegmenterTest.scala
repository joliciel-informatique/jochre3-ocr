package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.model.ImageLabel.{PredictedRectangle, Rectangle}
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
  object MockBlockPredictorService extends BlockPredictorService {
    override def getBlockPredictor(mat: Mat, fileName: String, outputLocation: Option[OutputLocation]): Task[SegmentationPredictor[PredictedRectangle]] = ZIO.attempt {
      new SegmentationPredictor[PredictedRectangle] {
        override def predict(): Task[Seq[PredictedRectangle]] = ZIO.attempt(Seq(
          PredictedRectangle(Rectangle(BlockType.TextBox.entryName, 10, 10, 50, 50), 0.9),
          PredictedRectangle(Rectangle(BlockType.Paragraph.entryName, 60, 10, 100, 100), 0.8),
          PredictedRectangle(Rectangle(BlockType.Image.entryName, 20, 120, 50, 50), 0.9),
        ))
      }
    }
  }

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
        blockPredictorService <- ZIO.succeed(MockBlockPredictorService)
        blockOnlySegmenter = new BlockOnlySegmenter(blockPredictorService)
        result <- blockOnlySegmenter.segment(mat, fileName, outputLocation)
      } yield {
        val expected = Seq(
          TextBlock(Rectangle(BlockType.TextBox.entryName, 10, 10, 50, 50), Seq.empty),
          TextBlock(Rectangle(BlockType.Paragraph.entryName, 60, 10, 100, 100), Seq.empty),
          Illustration(Rectangle(BlockType.Image.entryName, 20, 120, 50, 50)),
        )
        assertTrue(result.blocks==expected)
      }
    }
  )
}
