package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle

import scala.xml.{Elem, Node}

case class Glyph(rectangle: Rectangle) extends PageElement {
  override def translate(xDiff: Int, yDiff: Int): Glyph =
    Glyph(rectangle.translate(xDiff, yDiff))

  override def rotate(imageInfo: ImageInfo): Glyph =
    Glyph(rectangle.rotate(imageInfo))

  override def toXml(id: String): Elem =
    <Glyph HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString} CONTENT={rectangle.label}>
    </Glyph>
}

object Glyph {
  def fromXML(node: Node): Glyph =
    Glyph(Rectangle.fromXML(node \@ "CONTENT", node))
}
