package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle

import scala.xml.{Elem, Node}

case class Word(glyphs: Seq[Glyph], rectangle: Rectangle) extends PageElement {
  override def translate(xDiff: Int, yDiff: Int): Word =
    Word(glyphs.map(_.translate(xDiff, yDiff)), rectangle.translate(xDiff, yDiff))

  override def rotate(imageInfo: ImageInfo): Word =
    Word(glyphs.map(_.rotate(imageInfo)), rectangle.rotate(imageInfo))

  override def toXml(id: String): Elem =
    <String HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString} CONTENT={rectangle.label}>
      {glyphs.map(_.toXml())}
    </String>
}

object Word {
  def fromXML(node: Node): Word = {
    val glyphs = node.child.collect{
      case elem: Elem if elem.label=="Glyph" => Glyph.fromXML(elem)
    }.toSeq
    val content = node \@ "CONTENT"
    Word(glyphs, Rectangle.fromXML(content, node))
  }
}
