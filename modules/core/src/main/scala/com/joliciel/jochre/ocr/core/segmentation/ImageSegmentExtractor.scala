package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.model.ImageLabel
import com.joliciel.jochre.ocr.core.utils.{OpenCvUtils, OutputLocation}
import org.bytedeco.opencv.opencv_core.Mat

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}
import javax.imageio.ImageIO

sealed trait ImageSegment {
  def block: ImageLabel.Rectangle
}
case class TextSegment(block: ImageLabel.Rectangle, subImage: BufferedImage) extends ImageSegment
case class IllustrationSegment(block: ImageLabel.Rectangle) extends ImageSegment

case class ImageSegmentExtractor(image: Mat, blocks: Seq[ImageLabel.Rectangle], outputLocation: Option[OutputLocation] = None) extends OpenCvUtils {
  val segments: Seq[ImageSegment] = {
    val withoutIllustrations = toBufferedImage(image);
    val graph: Graphics2D = withoutIllustrations.createGraphics
    graph.setColor(Color.WHITE)

    blocks.filter(_.label==BlockType.Illustration.entryName)
      .map{ illustration =>
        graph.fill(illustration.toAWT())
      }

    graph.dispose()

    outputLocation.foreach(outputLocation => ImageIO.write(withoutIllustrations, "png", outputLocation.resolve("_no_illustraions.png").toFile))

    blocks.flatMap{
      case textBlock: ImageLabel.Rectangle if BlockType.withName(textBlock.label).isText =>
        val subImage = withoutIllustrations.getSubimage(textBlock.left, textBlock.top, textBlock.width, textBlock.height)
        Some(TextSegment(textBlock, subImage))
      case block: ImageLabel.Rectangle if block.label == BlockType.Illustration.entryName =>
        Some(IllustrationSegment(block))
      case _ =>
        None
    }
  }
}
