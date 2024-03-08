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

object FullYoloSegmenterIntegrationTest extends JUnitRunnableSpec with ImageUtils {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("FullYoloSegmenter")(
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
        yoloPredictorService <- ZIO.service[YoloPredictorService]
        fullYoloSegmenter = new FullYoloSegmenter(yoloPredictorService)
        result <- fullYoloSegmenter.segment(mat, fileName, outputLocation)
      } yield {
        val expectedBlock = Rectangle(624, 2248, 2434, 1911)
        val foundBlock =
          result.textBlocks.find(_.rectangle.percentageIntersection(expectedBlock) > 0.9)
        val textLines = foundBlock.map(_.textLines)
        val firstLineWords = textLines.map(_.head.words)
        val fifthWordGlyphs = firstLineWords.map(_(5).glyphs)
        val textLineCount = textLines.map(_.size).getOrElse(0)
        val wordCount = firstLineWords.map(_.size).getOrElse(0)
        val glyphCount = fifthWordGlyphs.map(_.size).getOrElse(0)

        assertTrue(
          foundBlock.isDefined
            && textLineCount == 16
            && wordCount == 11
            && glyphCount == 6
        )
      }
    }
  ).provide(
    HttpClientZioBackend.layer() >>> YoloPredictorService.live
  )
}
