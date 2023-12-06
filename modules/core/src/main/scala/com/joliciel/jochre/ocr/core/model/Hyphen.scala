package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle

import scala.xml.{Elem, Node}

case class Hyphen(rectangle: Rectangle) extends WordOrSpace {
  val content = rectangle.label

  override def translate(xDiff: Int, yDiff: Int): Hyphen = Hyphen(rectangle.translate(xDiff, yDiff))

  override def rotate(imageInfo: ImageInfo): Hyphen = Hyphen(rectangle.rotate(imageInfo))

  override def rescale(scale: Double): Hyphen = this.copy(
    rectangle = this.rectangle.rescale(scale)
  )

  override def toXml(id: String): Elem =
    <HYP HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString} CONTENT={rectangle.label}>
    </HYP>

  override def compare(that: WordOrSpace): Int = this.rectangle.left.compare(that.rectangle.left)

  def toWord: Word =
    Word(this.rectangle, Seq(Glyph(this.rectangle, 0.5)), 0.5)
}

object Hyphen {
  def fromXML(node: Node): Hyphen = {
    val content = node \@ "CONTENT"
    Hyphen(Rectangle.fromXML(content, node))
  }
}