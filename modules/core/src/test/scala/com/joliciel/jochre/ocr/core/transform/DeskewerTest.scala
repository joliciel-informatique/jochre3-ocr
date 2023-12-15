package com.joliciel.jochre.ocr.core.transform

import com.joliciel.jochre.ocr.core.utils.ImageUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import javax.imageio.ImageIO

class DeskewerTest extends AnyFlatSpec with Matchers with ImageUtils {
  "The Deskewer" should "deskew" in {
    val image = ImageIO.read(getClass.getResourceAsStream("/images/nybc200089_0011.png"))
    val mat = fromBufferedImage(image)
    val deskewer = Deskewer()
    val skewAngle = deskewer.getSkewAngle(mat).get
    skewAngle shouldBe (-1.09 +- 0.1)
  }
}
