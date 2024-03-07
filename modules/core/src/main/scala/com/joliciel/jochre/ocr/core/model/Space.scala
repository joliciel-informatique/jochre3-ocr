package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.graphics.{ImageInfo, Rectangle}
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.global.opencv_imgproc.LINE_8
import org.bytedeco.opencv.opencv_core.{AbstractScalar, Mat, Point}

import scala.xml.{Elem, Node}

case class Space(rectangle: Rectangle) extends WordOrSpace {
  override def translate(xDiff: Int, yDiff: Int): Space = Space(
    rectangle.translate(xDiff, yDiff)
  )

  override def rotate(imageInfo: ImageInfo): Space = Space(
    rectangle.rotate(imageInfo)
  )

  override def rescale(scale: Double): Space = this.copy(
    rectangle = this.rectangle.rescale(scale)
  )

  override def toXml: Elem =
    <SP HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={
      rectangle.width.toString
    } HEIGHT={rectangle.height.toString}></SP>

  override def draw(mat: Mat): Unit = {
    opencv_imgproc.rectangle(
      mat,
      new Point(rectangle.left, rectangle.top),
      new Point(
        rectangle.left + rectangle.width,
        rectangle.top + rectangle.height
      ),
      AbstractScalar.YELLOW,
      1,
      LINE_8,
      0
    )
  }

  override val content: String = " "

  override def transform(
      partialFunction: PartialFunction[AltoElement, AltoElement]
  ): Space = {
    val transformed = if (partialFunction.isDefinedAt(this)) {
      partialFunction(this).asInstanceOf[Space]
    } else { this }
    transformed
  }
}

object Space {
  def fromXML(node: Node): Space = {
    Space(Rectangle.fromXML(node))
  }
}
