package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.segmentation.BlockType

import scala.xml.{Elem, Node}

case class ComposedBlock(rectangle: Rectangle, textBlocks: Seq[TextBlock]) extends Block {
  override def translate(xDiff: Int, yDiff: Int): ComposedBlock =
    ComposedBlock(rectangle.translate(xDiff, yDiff), textBlocks.map(_.translate(xDiff, yDiff)))

  override def rotate(imageInfo: ImageInfo): ComposedBlock =
    ComposedBlock(rectangle.rotate(imageInfo), textBlocks.map(_.rotate(imageInfo)))

  override def toXml(id: String): Elem =
    <ComposedBlock ID={id} HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString}>
      {textBlocks.zipWithIndex.map{ case (textBlock, i) => textBlock.toXml(f"${id}_T${i}")}}
    </ComposedBlock>

  lazy val allWords: Seq[Word] = textBlocks.flatMap(_.allWords)
}

object ComposedBlock {
  def fromXML(imageInfo: ImageInfo, node: Node): ComposedBlock = {
    val paragraphs = node.child.collect {
      case elem: Elem if elem.label == "TextBlock" => TextBlock.fromXML(imageInfo, elem)
    }.toSeq
    ComposedBlock(Rectangle.fromXML(BlockType.Paragraph.entryName, node), paragraphs)
  }
}
