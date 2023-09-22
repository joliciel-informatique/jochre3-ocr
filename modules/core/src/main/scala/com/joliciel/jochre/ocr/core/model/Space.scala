package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle

import scala.xml.{Elem, Node}

case class Space(rectangle: Rectangle) extends WordOrSpace {
  override def translate(xDiff: Int, yDiff: Int): PageElement = Space(rectangle.translate(xDiff, yDiff))

  override def rotate(imageInfo: ImageInfo): PageElement = Space(rectangle.rotate(imageInfo))

  override def toXml(id: String): Elem =
    <SP HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString}>
    </SP>
}

object Space {
  def fromXML(node: Node): Space = {
    Space(Rectangle.fromXML("", node))
  }
}