package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.utils.MathUtils.MathImplicits._
import org.bytedeco.opencv.opencv_core.Mat

import scala.xml.{Elem, Node}

case class Glyph(rectangle: Rectangle, confidence: Double) extends PageElement with Ordered[Glyph] {
  override val content = rectangle.label
  override def translate(xDiff: Int, yDiff: Int): Glyph =
    Glyph(rectangle.translate(xDiff, yDiff), confidence)

  override def rotate(imageInfo: ImageInfo): Glyph =
    Glyph(rectangle.rotate(imageInfo), confidence)

  override def rescale(scale: Double): Glyph = this.copy(
    rectangle = this.rectangle.rescale(scale)
  )

  override def toXml(id: String): Elem =
    <Glyph HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString}
           CONTENT={rectangle.label} GC={confidence.roundTo(2).toString}>
    </Glyph>

  override def compare(that: Glyph): Int = this.rectangle.horizontalCompare(that.rectangle)

  override def draw(mat: Mat): Unit = {}

  override def transform(partialFunction: PartialFunction[AltoElement, AltoElement]): Glyph = {
    val transformed = if (partialFunction.isDefinedAt(this)) { partialFunction(this).asInstanceOf[Glyph] } else { this }
    transformed
  }
}

object Glyph {
  def fromXML(node: Node): Glyph =
    Glyph(Rectangle.fromXML(node \@ "CONTENT", node), (node \@ "GC").toDoubleOption.getOrElse(0.0))
}
