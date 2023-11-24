package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Line

import scala.xml.{Elem, Node}

case class TextLine(baseLine: Line, wordsAndSpaces: Seq[WordOrSpace]) extends PageElement with Ordered[TextLine] {
  lazy val content: String = wordsAndSpaces.map{
    case word@Word(_, _, _) => word.content
    case Space(_) => " "
    case hyphen@Hyphen(_) => hyphen.content
  }.mkString

  lazy val words: Seq[Word] = wordsAndSpaces.collect{ case w: Word => w }
  lazy val spaces: Seq[Space] = wordsAndSpaces.collect{ case s: Space => s }
  lazy val hyphen: Option[Hyphen] = wordsAndSpaces.collectFirst{ case h: Hyphen => h }

  lazy val wordsWithHyphenIncluded: Seq[Word] = {
    hyphen.map{ hyphen =>
      val lastWord = words.last
      val newRectangle = lastWord.rectangle.union(hyphen.rectangle).copy(label = f"${lastWord.content}${hyphen.content}")
      val newGlyphs = lastWord.glyphs :+ Glyph(hyphen.rectangle, 0.5)
      val lastWordWithHyphen = Word(newRectangle, newGlyphs, lastWord.confidence)
      words.init :+ lastWordWithHyphen
    }.getOrElse(words)
  }

  override def translate(xDiff: Int, yDiff: Int): TextLine =
    TextLine(baseLine.translate(xDiff, yDiff), wordsAndSpaces.map(_.translate(xDiff, yDiff)).collect{ case wordOrSpace: WordOrSpace => wordOrSpace })

  override def rotate(imageInfo: ImageInfo): TextLine =
    TextLine(baseLine.rotate(imageInfo), wordsAndSpaces.map(_.rotate(imageInfo)).collect{ case wordOrSpace: WordOrSpace => wordOrSpace })

  override def toXml(id: String): Elem =
    <TextLine HPOS={baseLine.x1.toString} VPOS={baseLine.y1.toString} WIDTH={baseLine.width.toString} HEIGHT={baseLine.height.toString}
              BASELINE={f"${baseLine.x1},${baseLine.y1} ${baseLine.x2},${baseLine.y2}"}>
      {wordsAndSpaces.map(_.toXml())}
    </TextLine>

  override def compare(that: TextLine): Int =
    this.baseLine.compare(that.baseLine)
}

object TextLine {
  def fromXML(imageInfo: ImageInfo, node: Node): TextLine = {
    val wordsAndSpaces = node.child.collect {
      case elem: Elem if elem.label == "String" => Word.fromXML(elem)
      case elem: Elem if elem.label == "SP" => Space.fromXML(elem)
      case elem: Elem if elem.label == "HYP" => Hyphen.fromXML(elem)
    }.toSeq
    TextLine(Line.fromXML(imageInfo, node), wordsAndSpaces)
  }
}