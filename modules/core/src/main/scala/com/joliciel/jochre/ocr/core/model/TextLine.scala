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

  lazy val wordsWithHyphenIncluded: Seq[Word] =
    hyphen.map{ hyphen =>
      val lastWordWithHyphen = words.last.combineWith(hyphen)
      words.init :+ lastWordWithHyphen
    }.getOrElse(words)

  /** A sequence of words in which any words not separated by white space have been combined into a single word */
  lazy val combinedWords: Seq[Word] = wordsAndSpaces.foldLeft(Seq.empty[Word] -> true){ case ((combinedWords, newWord), wordOrSpace) =>
    (newWord, wordOrSpace) match {
      case (true, word:Word) => (combinedWords :+ word) -> false
      case (false, word:Word) => (combinedWords.init :+ combinedWords.last.combineWith(word)) -> false
      case (true, hyphen: Hyphen) => (combinedWords :+ hyphen.toWord) -> false
      case (false, hyphen: Hyphen) => (combinedWords.init :+ combinedWords.last.combineWith(hyphen)) -> false
      case (_, _: Space) => combinedWords -> true
      case (_, other) => throw new Exception(f"Should never happen, what is $other")
    }
  }._1

  override def translate(xDiff: Int, yDiff: Int): TextLine =
    TextLine(baseLine.translate(xDiff, yDiff), wordsAndSpaces.map(_.translate(xDiff, yDiff)).collect{ case wordOrSpace: WordOrSpace => wordOrSpace })

  override def rotate(imageInfo: ImageInfo): TextLine =
    TextLine(baseLine.rotate(imageInfo), wordsAndSpaces.map(_.rotate(imageInfo)).collect{ case wordOrSpace: WordOrSpace => wordOrSpace })

  override def rescale(scale: Double): TextLine = this.copy(
    baseLine = this.baseLine.rescale(scale),
    wordsAndSpaces = this.wordsAndSpaces.map(_.rescale(scale)).collect { case wos: WordOrSpace => wos }
  )

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