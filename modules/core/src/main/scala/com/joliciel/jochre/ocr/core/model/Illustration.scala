package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.segmentation.BlockType
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.global.opencv_imgproc.LINE_8
import org.bytedeco.opencv.opencv_core.{AbstractScalar, Mat, Point}

import scala.xml.{Elem, Node}

case class Illustration(rectangle: Rectangle) extends Block {
  override def translate(xDiff: Int, yDiff: Int): Illustration =
    Illustration(rectangle.translate(xDiff, yDiff))

  override def rotate(imageInfo: ImageInfo): Illustration =
    Illustration(rectangle.rotate(imageInfo))

  override def rescale(scale: Double): Illustration = this.copy(
    rectangle = this.rectangle.rescale(scale)
  )

  override def toXml(id: String): Elem =
    <Illustration ID={id} HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString}>
    </Illustration>

  override def draw(mat: Mat): Unit = {
    opencv_imgproc.rectangle(mat, new Point(rectangle.left - 2, rectangle.top - 2), new Point(rectangle.left + rectangle.width + 4, rectangle.top + rectangle.height + 4), AbstractScalar.MAGENTA,
      2, LINE_8, 0)
  }

  override def content: String = ""

  override def transform(partialFunction: PartialFunction[AltoElement, AltoElement]): Illustration = {
    val transformed = if (partialFunction.isDefinedAt(this)) { partialFunction(this).asInstanceOf[Illustration] } else { this }
    transformed
  }
}

object Illustration {
  def fromXML(node: Node): Illustration = {
    Illustration(Rectangle.fromXML(BlockType.Image.entryName, node))
  }
}