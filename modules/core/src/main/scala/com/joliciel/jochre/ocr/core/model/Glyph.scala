package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.utils.MathImplicits._

import scala.xml.{Elem, Node}

case class Glyph(rectangle: Rectangle, confidence: Double) extends PageElement {
  override def translate(xDiff: Int, yDiff: Int): Glyph =
    Glyph(rectangle.translate(xDiff, yDiff), confidence)

  override def rotate(imageInfo: ImageInfo): Glyph =
    Glyph(rectangle.rotate(imageInfo), confidence)

  override def toXml(id: String): Elem =
    <Glyph HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString}
           CONTENT={rectangle.label} GC={confidence.roundTo(2).toString}>
    </Glyph>
}

object Glyph {
  def fromXML(node: Node): Glyph =
    Glyph(Rectangle.fromXML(node \@ "CONTENT", node), (node \@ "GC").toDoubleOption.getOrElse(0.0))
}
