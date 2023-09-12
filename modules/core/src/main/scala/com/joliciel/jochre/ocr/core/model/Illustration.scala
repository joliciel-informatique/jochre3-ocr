package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.segmentation.BlockType

import scala.xml.{Elem, Node}

case class Illustration(rectangle: Rectangle) extends Block {
  override def translate(xDiff: Int, yDiff: Int): Illustration =
    Illustration(rectangle.translate(xDiff, yDiff))

  override def rotate(imageInfo: ImageInfo): Illustration =
    Illustration(rectangle.rotate(imageInfo))

  override def toXml(id: String): Elem =
    <Illustration ID={id} HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString}>
    </Illustration>
}

object Illustration {
  def fromXML(node: Node): Illustration = {
    Illustration(Rectangle.fromXML(BlockType.Illustration.entryName, node))
  }
}