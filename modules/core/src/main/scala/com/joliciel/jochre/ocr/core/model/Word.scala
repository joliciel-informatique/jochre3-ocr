package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.utils.MathImplicits._
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.global.opencv_imgproc.LINE_8
import org.bytedeco.opencv.opencv_core.{AbstractScalar, Mat, Point}

import scala.xml.{Elem, Node}

case class Word(rectangle: Rectangle, glyphs: Seq[Glyph], confidence: Double) extends WordOrSpace {
  override val content = rectangle.label
  override def translate(xDiff: Int, yDiff: Int): Word =
    Word(rectangle.translate(xDiff, yDiff), glyphs.map(_.translate(xDiff, yDiff)), confidence)

  override def rotate(imageInfo: ImageInfo): Word =
    Word(rectangle.rotate(imageInfo), glyphs.map(_.rotate(imageInfo)), confidence)

  override def rescale(scale: Double): Word = this.copy(
    rectangle = this.rectangle.rescale(scale),
    glyphs = this.glyphs.map(_.rescale(scale)).collect { case g: Glyph => g }
  )

  override def toXml(id: String): Elem =
    <String HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString}
            CONTENT={rectangle.label} WC={confidence.roundTo(2).toString}>
      {glyphs.map(_.toXml())}
    </String>

  override def compare(that: WordOrSpace): Int = this.rectangle.horizontalCompare(that.rectangle)

  def combineWith(that: Word): Word = Word(this.rectangle.union(that.rectangle), this.glyphs ++ that.glyphs, Math.sqrt(this.confidence * that.confidence))

  def combineWith(hyphen: Hyphen): Word = {
    val newRectangle = this.rectangle.union(hyphen.rectangle)
    val newGlyphs = this.glyphs :+ Glyph(hyphen.rectangle, 0.5)
    Word(newRectangle, newGlyphs, this.confidence)
  }

  override def draw(mat: Mat): Unit = {
    opencv_imgproc.rectangle(mat, new Point(rectangle.left, rectangle.top + 1), new Point(rectangle.left + rectangle.width, rectangle.top + rectangle.height - 2), AbstractScalar.GREEN,
      3, LINE_8, 0)
    if (glyphs.size > 1) {
      glyphs.init.foreach { glyph =>
        opencv_imgproc.line(mat, new Point(glyph.rectangle.left, glyph.rectangle.top), new Point(glyph.rectangle.left, glyph.rectangle.bottom), AbstractScalar.RED,
          2, LINE_8, 0)
      }
    }
  }
}

object Word {
  def fromXML(node: Node): Word = {
    val glyphs = node.child.collect{
      case elem: Elem if elem.label=="Glyph" => Glyph.fromXML(elem)
    }.toSeq
    val content = node \@ "CONTENT"
    val confidence = (node \@ "WC").toDoubleOption.getOrElse(0.0)
    Word(Rectangle.fromXML(content, node), glyphs, confidence)
  }
}
