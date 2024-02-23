package com.joliciel.jochre.ocr.core.text

import com.joliciel.jochre.ocr.core.model.ImageLabel
import com.joliciel.jochre.ocr.core.segmentation.BlockType
import com.joliciel.jochre.ocr.core.utils.{ImageUtils, OutputLocation}
import org.bytedeco.opencv.opencv_core.Mat

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}
import javax.imageio.ImageIO

private[text] sealed trait ImageSegment {
  def block: ImageLabel.Rectangle
}
private[text] case class TextSegment(block: ImageLabel.Rectangle, subImage: BufferedImage) extends ImageSegment
private[text] case class IllustrationSegment(block: ImageLabel.Rectangle) extends ImageSegment

private[text] case class ImageSegmentExtractor(image: Mat, blocks: Seq[ImageLabel.Rectangle], outputLocation: Option[OutputLocation] = None) extends ImageUtils {
  val segments: Seq[ImageSegment] = {
    val withoutIllustrations = toBufferedImage(image);
    val graph: Graphics2D = withoutIllustrations.createGraphics
    graph.setColor(Color.WHITE)

    blocks.filter(_.label==BlockType.Image.entryName)
      .map{ illustration =>
        graph.fill(illustration.toAWT)
      }

    graph.dispose()

    outputLocation.foreach(outputLocation => ImageIO.write(withoutIllustrations, "png", outputLocation.resolve("_no_illustraions.png").toFile))

    blocks.flatMap{
      case textBlock: ImageLabel.Rectangle if BlockType.withName(textBlock.label).isText =>
        val left = if (textBlock.left<0) { 0 } else {textBlock.left}
        val width = if (left + textBlock.width > withoutIllustrations.getWidth) { withoutIllustrations.getWidth - left}  else { textBlock.width }
        val top = if (textBlock.top<0) { 0 } else {textBlock.top}
        val height = if (top + textBlock.height> withoutIllustrations.getHeight) { withoutIllustrations.getHeight - top } else { textBlock.height }
        val subImage = withoutIllustrations.getSubimage(left, top, width, height)
        Some(TextSegment(textBlock, subImage))
      case block: ImageLabel.Rectangle if block.label == BlockType.Image.entryName =>
        Some(IllustrationSegment(block))
      case _ =>
        None
    }
  }
}
