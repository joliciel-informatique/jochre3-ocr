package com.joliciel.jochre.ocr.core.text

import com.joliciel.jochre.ocr.core.model.{Block, Illustration, TextBlock, TextContainer}
import com.joliciel.jochre.ocr.core.graphics.{PredictedRectangle, Rectangle}
import com.joliciel.jochre.ocr.core.segmentation.BlockType
import com.joliciel.jochre.ocr.core.utils.{ImageUtils, OutputLocation}
import org.bytedeco.opencv.opencv_core.Mat

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}
import javax.imageio.ImageIO

private[text] sealed trait ImageSegment {
  def block: Rectangle
}
private[text] case class TextSegment(block: Rectangle, subImage: BufferedImage) extends ImageSegment
private[text] case class IllustrationSegment(block: Rectangle) extends ImageSegment

private[text] case class ImageSegmentExtractor(image: Mat, blocks: Seq[Block], outputLocation: Option[OutputLocation] = None) extends ImageUtils {
  val segments: Seq[ImageSegment] = {
    val withoutIllustrations = toBufferedImage(image);
    val graph: Graphics2D = withoutIllustrations.createGraphics
    graph.setColor(Color.WHITE)

    blocks.collect{
      case illustration: Illustration => illustration
    }.foreach{ illustration =>
      graph.fill(illustration.rectangle.toAWT)
    }

    graph.dispose()

    outputLocation.foreach(outputLocation => ImageIO.write(withoutIllustrations, "png", outputLocation.resolve("_no_illustraions.png").toFile))

    blocks.flatMap{
      case textBlock: TextContainer =>
        val rectangle = textBlock.rectangle
        val left = if (rectangle.left<0) { 0 } else {rectangle.left}
        val width = if (left + rectangle.width > withoutIllustrations.getWidth) { withoutIllustrations.getWidth - left}  else { rectangle.width }
        val top = if (rectangle.top<0) { 0 } else {rectangle.top}
        val height = if (top + rectangle.height> withoutIllustrations.getHeight) { withoutIllustrations.getHeight - top } else { rectangle.height }
        val subImage = withoutIllustrations.getSubimage(left, top, width, height)
        Some(TextSegment(rectangle, subImage))
      case illustration: Illustration =>
        Some(IllustrationSegment(illustration.rectangle))
      case _ =>
        None
    }
  }
}
