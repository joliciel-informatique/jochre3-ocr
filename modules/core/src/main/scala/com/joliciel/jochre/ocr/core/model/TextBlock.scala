package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.global.opencv_imgproc.LINE_8
import org.bytedeco.opencv.opencv_core.{AbstractScalar, Mat, Point}

import java.util.UUID
import scala.xml.{Elem, Node}

case class TextBlock(
  rectangle: Rectangle,
  textLines: Seq[TextLine],
  id: String = UUID.randomUUID().toString,
  language: Option[String] = None,
  idNext: Option[String] = None,
  styleRefs: Option[String] = None,
  tagRefs: Option[String] = None,
  defaultLanguage: Option[String] = None,
) extends TextContainer with WithLanguage {
  lazy val textLinesWithRectangles: Seq[(TextLine, Rectangle)] = {
    val optionalTextLinesAfter = textLines.drop(1).map(Some(_)) :+ None

    val surroundedTextLines = textLines.zip(optionalTextLinesAfter)
    val rectangles = surroundedTextLines.foldLeft(Seq.empty[Rectangle]) { case (rectangles, (current, after)) =>
      val lastRectangle = rectangles.lastOption
      val top = lastRectangle.map(_.bottom).getOrElse(rectangle.top)
      val height = after.map { after =>
        val distanceToTop = current.baseLine.y1 - top
        val distanceToNext = after.baseLine.y1 - current.baseLine.y1
        distanceToTop + (distanceToNext * 0.25).toInt
      }.getOrElse(rectangle.bottom - top)
      val myRectangle = Rectangle(current.baseLine.x1, top, current.baseLine.x2 - current.baseLine.x1, height)
      rectangles :+ myRectangle
    }
    textLines.zip(rectangles)
  }

  lazy val combinedWords: Seq[Word] = textLines.flatMap(_.combinedWords)

  override def translate(xDiff: Int, yDiff: Int): TextBlock =
    this.copy(rectangle = rectangle.translate(xDiff, yDiff), textLines = textLines.map(_.translate(xDiff, yDiff)))

  override def rotate(imageInfo: ImageInfo): TextBlock =
    this.copy(rectangle = rectangle.rotate(imageInfo), textLines = textLines.map(_.rotate(imageInfo)))

  override def rescale(scale: Double): TextBlock = this.copy(
    rectangle = this.rectangle.rescale(scale),
    textLines = this.textLines.map(_.rescale(scale)).collect { case tl: TextLine => tl }
  )

  override def toXml: Elem =
    <TextBlock ID={id} HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString}
               STYLEREFS={styleRefs.orNull} TAGREFS={tagRefs.orNull} IDNEXT={idNext.orNull}>{textLines.map(_.toXml)}</TextBlock>

  def allWords: Seq[Word] = textLines.flatMap(_.words)

  override def draw(mat: Mat): Unit = {
    opencv_imgproc.rectangle(mat, new Point(rectangle.left - 2, rectangle.top - 2), new Point(rectangle.left + rectangle.width + 4, rectangle.top + rectangle.height + 4), AbstractScalar.BLACK,
      2, LINE_8, 0)
    this.textLines.foreach(_.draw(mat))
  }

  override lazy val content: String = textLines.map(_.content).mkString("\n")

  override def transform(partialFunction: PartialFunction[AltoElement, AltoElement]): TextBlock = {
    val transformed = if (partialFunction.isDefinedAt(this)) { partialFunction(this).asInstanceOf[TextBlock] } else { this }
    val newLines = transformed.textLines.map(_.transform(partialFunction)).collect{ case textLine: TextLine => textLine }
    transformed.copy(textLines = newLines)
  }

  def withDefaultLanguage(defaultLanguage: String): TextBlock = {
    val currentLanguage = this.languageOrDefault
    val newLanguage = Option.when(currentLanguage != defaultLanguage)(currentLanguage)

    this.copy(language = newLanguage, defaultLanguage = Some(defaultLanguage), textLines = this.textLines.map(_.withDefaultLanguage(this.getEffectiveLanguage(newLanguage, Some(defaultLanguage)))))
  }
}

object TextBlock {
  def fromXML(imageInfo: ImageInfo, node: Node): TextBlock = {
    val textLines = node.child.collect {
      case elem: Elem if elem.label == "TextLine" => TextLine.fromXML(imageInfo, elem)
    }.toSeq

    val id = node \@ "ID"
    val idOption = Option.when(id.nonEmpty)(id).getOrElse(UUID.randomUUID().toString)
    val idNext = node \@ "IDNEXT"
    val idNextOption = Option.when(idNext.nonEmpty)(idNext)
    val tagRefs = node \@ "TAGREFS"
    val tagRefsOption = Option.when(tagRefs.nonEmpty)(tagRefs)
    val styleRefs = node \@ "STYLEREFS"
    val styleRefsOption = Option.when(styleRefs.nonEmpty)(styleRefs)
    val languageAttribute = node \@ "LANG"
    val languageOption = Option.when(languageAttribute.nonEmpty)(languageAttribute)

    TextBlock(Rectangle.fromXML(node), textLines, id = idOption, language = languageOption, idNext = idNextOption, styleRefs = styleRefsOption, tagRefs = tagRefsOption)
  }
}
