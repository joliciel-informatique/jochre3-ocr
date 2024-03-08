package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.graphics.{ImageInfo, Line, WithRectangle}
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.global.opencv_imgproc.LINE_8
import org.bytedeco.opencv.opencv_core.{AbstractScalar, Mat, Point}

import scala.xml.{Elem, Node}

case class TextLine(
    baseLine: Line,
    wordsAndSpaces: Seq[WordOrSpace],
    language: Option[String] = None,
    styleRefs: Option[String] = None,
    tagRefs: Option[String] = None,
    defaultLanguage: Option[String] = None
) extends PageElement
    with WithLanguage
    with Ordered[TextLine] {
  override lazy val content: String = wordsAndSpaces.map(_.content).mkString

  lazy val words: Seq[Word] = wordsAndSpaces.collect { case w: Word => w }
  lazy val spaces: Seq[Space] = wordsAndSpaces.collect { case s: Space => s }
  lazy val hyphen: Option[Hyphen] = wordsAndSpaces.collectFirst { case h: Hyphen =>
    h
  }

  lazy val wordsWithHyphenIncluded: Seq[Word] =
    hyphen
      .map { hyphen =>
        val lastWordWithHyphen = words.last.combineWith(hyphen)
        words.init :+ lastWordWithHyphen
      }
      .getOrElse(words)

  /** A sequence of words in which any words not separated by white space have been combined into a
    * single word
    */
  lazy val combinedWords: Seq[Word] = wordsAndSpaces
    .foldLeft(Seq.empty[Word] -> true) { case ((combinedWords, newWord), wordOrSpace) =>
      (newWord, wordOrSpace) match {
        case (true, word: Word) => (combinedWords :+ word) -> false
        case (false, word: Word) =>
          (combinedWords.init :+ combinedWords.last.combineWith(
            word
          )) -> false
        case (true, hyphen: Hyphen) =>
          (combinedWords :+ hyphen.toWord) -> false
        case (false, hyphen: Hyphen) =>
          (combinedWords.init :+ combinedWords.last.combineWith(
            hyphen
          )) -> false
        case (_, _: Space) => combinedWords -> true
        case (_, other) =>
          throw new Exception(f"Should never happen, what is $other")
      }
    }
    ._1

  override def translate(xDiff: Int, yDiff: Int): TextLine =
    TextLine(
      baseLine.translate(xDiff, yDiff),
      wordsAndSpaces.map(_.translate(xDiff, yDiff)).collect { case wordOrSpace: WordOrSpace =>
        wordOrSpace
      }
    )

  override def rotate(imageInfo: ImageInfo): TextLine =
    TextLine(
      baseLine.rotate(imageInfo),
      wordsAndSpaces.map(_.rotate(imageInfo)).collect { case wordOrSpace: WordOrSpace =>
        wordOrSpace
      }
    )

  override def rescale(scale: Double): TextLine = this.copy(
    baseLine = this.baseLine.rescale(scale),
    wordsAndSpaces = this.wordsAndSpaces.map(_.rescale(scale)).collect { case wos: WordOrSpace =>
      wos
    }
  )

  override def toXml: Elem =
    <TextLine HPOS={baseLine.x1.toString} VPOS={baseLine.y1.toString}
              WIDTH={baseLine.width.toString} HEIGHT={baseLine.height.toString}
              BASELINE={f"${baseLine.x1},${baseLine.y1} ${baseLine.x2},${baseLine.y2}"}
              STYLEREFS={styleRefs.orNull} TAGREFS={tagRefs.orNull}
    >{wordsAndSpaces.map(_.toXml)}</TextLine>

  override def compare(that: TextLine): Int =
    this.baseLine.compare(that.baseLine)

  override def draw(mat: Mat): Unit = {
    opencv_imgproc.line(
      mat,
      new Point(baseLine.x1, baseLine.y1),
      new Point(baseLine.x2, baseLine.y2),
      AbstractScalar.BLUE,
      3,
      LINE_8,
      0
    )
    this.wordsAndSpaces.foreach(_.draw(mat))
  }

  override def transform(
      partialFunction: PartialFunction[AltoElement, AltoElement]
  ): TextLine = {
    val transformed = if (partialFunction.isDefinedAt(this)) {
      partialFunction(this).asInstanceOf[TextLine]
    } else { this }
    val newWordsAndSpaces =
      transformed.wordsAndSpaces.map(_.transform(partialFunction)).collect {
        case wordOrSpace: WordOrSpace => wordOrSpace
      }
    transformed.copy(wordsAndSpaces = newWordsAndSpaces)
  }

  def withDefaultLanguage(defaultLanguage: String): TextLine = {
    val currentLanguage = this.languageOrDefault
    val newLanguage =
      Option.when(currentLanguage != defaultLanguage)(currentLanguage)

    val withLanguageSet = this.copy(
      language = newLanguage,
      defaultLanguage = Some(defaultLanguage),
      wordsAndSpaces = this.wordsAndSpaces.map {
        case word: Word =>
          word.withDefaultLanguage(
            this.getEffectiveLanguage(newLanguage, Some(defaultLanguage))
          )
        case other => other
      }
    )
    val leftToRight = withLanguageSet.isLeftToRight
    val newWordsAndSpaces = if (leftToRight != this.isLeftToRight) {
      withLanguageSet.wordsAndSpaces.sorted(
        WithRectangle.HorizontalOrdering(leftToRight)
      )
    } else {
      withLanguageSet.wordsAndSpaces
    }
    withLanguageSet.copy(wordsAndSpaces = newWordsAndSpaces)
  }
}

object TextLine {
  def fromXML(imageInfo: ImageInfo, node: Node): TextLine = {
    val wordsAndSpaces = node.child.collect {
      case elem: Elem if elem.label == "String" => Word.fromXML(elem)
      case elem: Elem if elem.label == "SP"     => Space.fromXML(elem)
      case elem: Elem if elem.label == "HYP"    => Hyphen.fromXML(elem)
    }.toSeq

    val tagRefs = node \@ "TAGREFS"
    val tagRefsOption = Option.when(tagRefs.nonEmpty)(tagRefs)
    val styleRefs = node \@ "STYLEREFS"
    val styleRefsOption = Option.when(styleRefs.nonEmpty)(styleRefs)
    val languageAttribute = node \@ "LANG"
    val languageOption =
      Option.when(languageAttribute.nonEmpty)(languageAttribute)

    TextLine(
      Line.fromXML(imageInfo, node),
      wordsAndSpaces,
      language = languageOption,
      styleRefs = styleRefsOption,
      tagRefs = tagRefsOption
    )
  }
}
