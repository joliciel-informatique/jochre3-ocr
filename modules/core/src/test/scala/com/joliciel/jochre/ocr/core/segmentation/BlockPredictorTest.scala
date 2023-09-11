package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.utils.{OpenCvUtils, OutputLocation}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path
import javax.imageio.ImageIO

class BlockPredictorTest extends AnyFlatSpec with Matchers with OpenCvUtils {
  "A BlockPredictor" should "predict blocks" in {
    val image = ImageIO.read(getClass.getResourceAsStream("/images/nybc200089_0011_deskewered.jpg"))
    val mat = fromBufferedImage(image)
    val fileName = "nybc200089_0011_deskewered.jpg"
    val outputPath = Path.of("temp/output")
    outputPath.toFile.mkdirs()
    val blockPredictor = BlockPredictor(mat, fileName, Some(OutputLocation(outputPath, "nybc200089_0011")))
    val result = blockPredictor.predict()

    val expected = Seq(
      Rectangle("paragraph", 600, 2254, 2477, 605),
      Rectangle("paragraph", 608, 2880, 2468, 1281),
      Rectangle("textbox", 671, 1610, 2370, 214),
      Rectangle("textbox", 1765, 2040, 154, 117),
      Rectangle("textbox", 1784, 4176, 121, 93)
    )

    result.map { rect =>
      val matched = expected.find(other => rect.iou(other) > 0.9)
      matched.isDefined shouldEqual true
    }
  }
}
