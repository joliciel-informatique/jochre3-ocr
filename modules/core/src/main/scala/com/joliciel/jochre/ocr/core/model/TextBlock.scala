package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.segmentation.BlockType
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.global.opencv_imgproc.LINE_8
import org.bytedeco.opencv.opencv_core.{AbstractScalar, Mat, Point}

import scala.xml.{Elem, Node}

case class TextBlock(rectangle: Rectangle, textLines: Seq[TextLine]) extends Block {
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
      val myRectangle = Rectangle(current.content, current.baseLine.x1, top, current.baseLine.x2 - current.baseLine.x1, height)
      rectangles :+ myRectangle
    }
    textLines.zip(rectangles)
  }

  lazy val combinedWords: Seq[Word] = textLines.flatMap(_.combinedWords)

  override def translate(xDiff: Int, yDiff: Int): TextBlock =
    TextBlock(rectangle.translate(xDiff, yDiff), textLines.map(_.translate(xDiff, yDiff)))

  override def rotate(imageInfo: ImageInfo): TextBlock =
    TextBlock(rectangle.rotate(imageInfo), textLines.map(_.rotate(imageInfo)))

  override def rescale(scale: Double): TextBlock = this.copy(
    rectangle = this.rectangle.rescale(scale),
    textLines = this.textLines.map(_.rescale(scale)).collect { case tl: TextLine => tl }
  )

  override def toXml(id: String): Elem =
    <TextBlock ID={id} HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString} >
      {textLines.map(_.toXml())}
    </TextBlock>

  def allWords: Seq[Word] = textLines.flatMap(_.words)

  override def draw(mat: Mat): Unit = {
    opencv_imgproc.rectangle(mat, new Point(rectangle.left - 2, rectangle.top - 2), new Point(rectangle.left + rectangle.width + 4, rectangle.top + rectangle.height + 4), AbstractScalar.BLACK,
      2, LINE_8, 0)
    this.textLines.map(_.draw(mat))
  }

  override lazy val content: String = textLines.map(_.content).mkString("\n")

  override def transform(partialFunction: PartialFunction[AltoElement, AltoElement]): TextBlock = {
    val transformed = if (partialFunction.isDefinedAt(this)) { partialFunction(this).asInstanceOf[TextBlock] } else { this }
    val newLines = transformed.textLines.map(_.transform(partialFunction)).collect{ case textLine: TextLine => textLine }
    transformed.copy(textLines = newLines)
  }
}

object TextBlock {
  def fromXML(imageInfo: ImageInfo, node: Node): TextBlock = {
    val textLines = node.child.collect {
      case elem: Elem if elem.label == "TextLine" => TextLine.fromXML(imageInfo, elem)
    }.toSeq
    TextBlock(Rectangle.fromXML(BlockType.TextBox.entryName, node), textLines)
  }
}
