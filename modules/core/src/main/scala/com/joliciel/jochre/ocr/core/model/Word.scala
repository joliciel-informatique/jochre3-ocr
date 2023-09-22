package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.utils.MathImplicits._

import scala.xml.{Elem, Node}

case class Word(rectangle: Rectangle, glyphs: Seq[Glyph], confidence: Double) extends WordOrSpace {
  override def translate(xDiff: Int, yDiff: Int): Word =
    Word(rectangle.translate(xDiff, yDiff), glyphs.map(_.translate(xDiff, yDiff)), confidence)

  override def rotate(imageInfo: ImageInfo): Word =
    Word(rectangle.rotate(imageInfo), glyphs.map(_.rotate(imageInfo)), confidence)

  override def toXml(id: String): Elem =
    <String HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString}
            CONTENT={rectangle.label} WC={confidence.roundTo(2).toString}>
      {glyphs.map(_.toXml())}
    </String>
}

object Word {
  def fromXML(node: Node): Word = {
    val glyphs = node.child.collect{
      case elem: Elem if elem.label=="Glyph" => Glyph.fromXML(elem)
    }.toSeq
    val content = node \@ "CONTENT"
    val confidence = (node \@ "WC").toDoubleOption.getOrElse(0.0)
    Word(Rectangle.fromXML(content, node), glyphs, confidence)
  }
}
