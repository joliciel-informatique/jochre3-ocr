package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.utils.MathUtils.MathImplicits._
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.global.opencv_imgproc.LINE_8
import org.bytedeco.opencv.opencv_core.{AbstractScalar, Mat, Point}

import scala.xml.{Elem, Node}

case class Word(content: String, rectangle: Rectangle, glyphs: Seq[Glyph], alternatives: Seq[SpellingAlternative], confidence: Double, styleRefs: Option[String] = None, tagRefs: Option[String] = None) extends WordOrSpace {
  override def translate(xDiff: Int, yDiff: Int): Word =
    this.copy(rectangle = rectangle.translate(xDiff, yDiff), glyphs = glyphs.map(_.translate(xDiff, yDiff)))

  override def rotate(imageInfo: ImageInfo): Word =
    this.copy(rectangle = rectangle.rotate(imageInfo), glyphs = glyphs.map(_.rotate(imageInfo)))

  override def rescale(scale: Double): Word = this.copy(
    rectangle = this.rectangle.rescale(scale),
    glyphs = this.glyphs.map(_.rescale(scale)).collect { case g: Glyph => g }
  )

  override def toXml: Elem =
    <String HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString}
            CONTENT={content} WC={confidence.roundTo(2).toString} STYLEREFS={styleRefs.orNull} TAGREFS={tagRefs.orNull}>
      {alternatives.map(_.toXml)}
      {glyphs.map(_.toXml)}</String>

  override def compare(that: WordOrSpace): Int = this.rectangle.horizontalCompare(that.rectangle)

  def combineWith(that: Word): Word = Word(f"${this.content}${that.content}", this.rectangle.union(that.rectangle), this.glyphs ++ that.glyphs, this.alternatives ++ that.alternatives, Math.sqrt(this.confidence * that.confidence))

  def combineWith(hyphen: Hyphen): Word = {
    val newRectangle = this.rectangle.union(hyphen.rectangle)
    val newGlyphs = this.glyphs :+ Glyph(hyphen.content, hyphen.rectangle, 0.5)
    this.copy(rectangle = newRectangle, glyphs = newGlyphs)
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

  override def transform(partialFunction: PartialFunction[AltoElement, AltoElement]): Word = {
    val transformed = if (partialFunction.isDefinedAt(this)) { partialFunction(this).asInstanceOf[Word] } else { this }
    val newGlyphs = transformed.glyphs.map(_.transform(partialFunction)).collect { case glyph: Glyph => glyph }
    val newAlternatives = transformed.alternatives.map(_.transform(partialFunction)).collect { case alternative: SpellingAlternative => alternative }
    transformed.copy(glyphs = newGlyphs, alternatives = newAlternatives)
  }
}

object Word {
  def fromXML(node: Node): Word = {
    val glyphs = node.child.collect{
      case elem: Elem if elem.label=="Glyph" => Glyph.fromXML(elem)
    }.toSeq
    val alternatives = node.child.collect {
      case elem: Elem if elem.label=="ALTERNATIVE" => SpellingAlternative.fromXML(elem)
    }.toSeq
    val content = node \@ "CONTENT"
    val confidence = (node \@ "WC").toDoubleOption.getOrElse(0.0)
    val tagRefs = node \@ "TAGREFS"
    val tagRefsOption = Option.when(tagRefs.nonEmpty)(tagRefs)
    val styleRefs = node \@ "STYLEREFS"
    val styleRefsOption = Option.when(styleRefs.nonEmpty)(styleRefs)
    Word(content = content, rectangle = Rectangle.fromXML(node), glyphs, alternatives, confidence, styleRefs = styleRefsOption, tagRefs = tagRefsOption)
  }
}
