package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.segmentation.BlockType

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

  override def translate(xDiff: Int, yDiff: Int): TextBlock =
    TextBlock(rectangle.translate(xDiff, yDiff), textLines.map(_.translate(xDiff, yDiff)))

  override def rotate(imageInfo: ImageInfo): TextBlock =
    TextBlock(rectangle.rotate(imageInfo), textLines.map(_.rotate(imageInfo)))

  override def toXml(id: String): Elem =
    <TextBlock ID={id} HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString} >
      {textLines.map(_.toXml())}
    </TextBlock>

  def allWords: Seq[Word] = textLines.flatMap(_.words)
}

object TextBlock {
  def fromXML(imageInfo: ImageInfo, node: Node): TextBlock = {
    val textLines = node.child.collect {
      case elem: Elem if elem.label == "TextLine" => TextLine.fromXML(imageInfo, elem)
    }.toSeq
    TextBlock(Rectangle.fromXML(BlockType.TextBox.entryName, node), textLines)
  }
}
