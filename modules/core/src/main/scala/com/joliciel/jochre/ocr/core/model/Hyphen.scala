package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.graphics.{ImageInfo, Rectangle}
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.global.opencv_imgproc.LINE_8
import org.bytedeco.opencv.opencv_core.{AbstractScalar, Mat, Point}

import scala.xml.{Elem, Node}

case class Hyphen(content: String, rectangle: Rectangle) extends WordOrSpace {
  override def translate(xDiff: Int, yDiff: Int): Hyphen =
    this.copy(rectangle = rectangle.translate(xDiff, yDiff))

  override def rotate(imageInfo: ImageInfo): Hyphen =
    this.copy(rectangle = rectangle.rotate(imageInfo))

  override def rescale(scale: Double): Hyphen =
    this.copy(rectangle = this.rectangle.rescale(scale))

  override def toXml: Elem =
    <HYP HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={
      rectangle.width.toString
    } HEIGHT={rectangle.height.toString} CONTENT={content}></HYP>

  def toWord: Word =
    Word(
      this.content,
      this.rectangle,
      Seq(Glyph(this.content, this.rectangle, 0.5)),
      Seq.empty,
      0.5
    )

  override def draw(mat: Mat): Unit = {
    opencv_imgproc.rectangle(
      mat,
      new Point(rectangle.left, rectangle.top),
      new Point(
        rectangle.left + rectangle.width,
        rectangle.top + rectangle.height
      ),
      AbstractScalar.MAGENTA,
      1,
      LINE_8,
      0
    )
  }

  override def transform(
      partialFunction: PartialFunction[AltoElement, AltoElement]
  ): Hyphen = {
    val transformed = if (partialFunction.isDefinedAt(this)) {
      partialFunction(this).asInstanceOf[Hyphen]
    } else { this }
    transformed
  }
}

object Hyphen {
  def fromXML(node: Node): Hyphen = {
    val content = node \@ "CONTENT"
    Hyphen(content = content, rectangle = Rectangle.fromXML(node))
  }
}
