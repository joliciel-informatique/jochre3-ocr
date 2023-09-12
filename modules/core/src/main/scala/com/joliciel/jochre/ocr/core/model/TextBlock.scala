package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.segmentation.BlockType

import scala.xml.{Elem, Node}

case class TextBlock(textLines: Seq[TextLine], rectangle: Rectangle) extends Block {
  override def translate(xDiff: Int, yDiff: Int): TextBlock =
    TextBlock(textLines.map(_.translate(xDiff, yDiff)), rectangle.translate(xDiff, yDiff))

  override def rotate(imageInfo: ImageInfo): TextBlock =
    TextBlock(textLines.map(_.rotate(imageInfo)), rectangle.rotate(imageInfo))

  override def toXml(id: String): Elem =
    <TextBlock ID={id} HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString} >
      {textLines.map(_.toXml())}
    </TextBlock>
}

object TextBlock {
  def fromXML(imageInfo: ImageInfo, node: Node): TextBlock = {
    val textLines = node.child.collect {
      case elem: Elem if elem.label == "TextLine" => TextLine.fromXML(imageInfo, elem)
    }.toSeq
    TextBlock(textLines, Rectangle.fromXML(BlockType.TextBox.entryName, node))
  }
}
