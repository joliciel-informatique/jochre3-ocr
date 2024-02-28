package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.utils.MathUtils.MathImplicits._
import org.bytedeco.opencv.opencv_core.Mat

import scala.xml.{Elem, Node}

case class Glyph(content: String, rectangle: Rectangle, confidence: Double) extends PageElement with WithRectangle {
  override def translate(xDiff: Int, yDiff: Int): Glyph =
    this.copy(rectangle = rectangle.translate(xDiff, yDiff))

  override def rotate(imageInfo: ImageInfo): Glyph =
    this.copy(rectangle = rectangle.rotate(imageInfo))

  override def rescale(scale: Double): Glyph =
    this.copy(rectangle = this.rectangle.rescale(scale))

  override def toXml: Elem =
    <Glyph HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString}
           CONTENT={content} GC={confidence.roundTo(2).toString}></Glyph>

  override def draw(mat: Mat): Unit = {}

  override def transform(partialFunction: PartialFunction[AltoElement, AltoElement]): Glyph = {
    val transformed = if (partialFunction.isDefinedAt(this)) { partialFunction(this).asInstanceOf[Glyph] } else { this }
    transformed
  }
}

object Glyph {
  def fromXML(node: Node): Glyph = {
    val content =node \@ "CONTENT"
    Glyph(content = content, rectangle = Rectangle.fromXML(node), confidence = (node \@ "GC").toDoubleOption.getOrElse(0.0))
  }
}
