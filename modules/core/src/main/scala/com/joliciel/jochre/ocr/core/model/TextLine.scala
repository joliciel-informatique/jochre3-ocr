package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Line

import scala.xml.{Elem, Node}

case class TextLine(words: Seq[Word], baseLine: Line) extends PageElement {
  override def translate(xDiff: Int, yDiff: Int): TextLine =
    TextLine(words.map(_.translate(xDiff, yDiff)), baseLine.translate(xDiff, yDiff))

  override def rotate(imageInfo: ImageInfo): TextLine =
    TextLine(words.map(_.rotate(imageInfo)), baseLine.rotate(imageInfo))

  override def toXml(id: String): Elem =
    <TextLine HPOS={baseLine.x1.toString} VPOS={baseLine.y1.toString} WIDTH={baseLine.width.toString} HEIGHT={baseLine.height.toString} BASELINE={baseLine.y1.toString}>
      {words.map(_.toXml())}
    </TextLine>
}

object TextLine {
  def fromXML(imageInfo: ImageInfo, node: Node): TextLine = {
    val words = node.child.collect {
      case elem: Elem if elem.label == "String" => Word.fromXML(elem)
    }.toSeq
    TextLine(words, Line.fromXML(imageInfo, node))
  }
}