package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Line

import scala.xml.{Elem, Node}

case class TextLine(baseLine: Line, wordsAndSpaces: Seq[WordOrSpace]) extends PageElement {
  lazy val content: String = wordsAndSpaces.map{
    case Word(rectangle, _, _) => rectangle.label
    case Space(_) => " "
  }.mkString

  lazy val words: Seq[Word] = wordsAndSpaces.collect{ case w: Word => w }

  override def translate(xDiff: Int, yDiff: Int): TextLine =
    TextLine(baseLine.translate(xDiff, yDiff), wordsAndSpaces.map(_.translate(xDiff, yDiff)).collect{ case wordOrSpace: WordOrSpace => wordOrSpace })

  override def rotate(imageInfo: ImageInfo): TextLine =
    TextLine(baseLine.rotate(imageInfo), wordsAndSpaces.map(_.rotate(imageInfo)).collect{ case wordOrSpace: WordOrSpace => wordOrSpace })

  override def toXml(id: String): Elem =
    <TextLine HPOS={baseLine.x1.toString} VPOS={baseLine.y1.toString} WIDTH={baseLine.width.toString} HEIGHT={baseLine.height.toString}
              BASELINE={f"${baseLine.x1},${baseLine.y1} ${baseLine.x2},${baseLine.y2}"}>
      {wordsAndSpaces.map(_.toXml())}
    </TextLine>
}

object TextLine {
  def fromXML(imageInfo: ImageInfo, node: Node): TextLine = {
    val wordsAndSpaces = node.child.collect {
      case elem: Elem if elem.label == "String" => Word.fromXML(elem)
      case elem: Elem if elem.label == "SP" => Space.fromXML(elem)
    }.toSeq
    TextLine(Line.fromXML(imageInfo, node), wordsAndSpaces)
  }
}