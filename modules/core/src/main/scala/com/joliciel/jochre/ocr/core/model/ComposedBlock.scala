package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.segmentation.BlockType
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.global.opencv_imgproc.LINE_8
import org.bytedeco.opencv.opencv_core.{AbstractScalar, Mat, Point}

import java.util.UUID
import scala.xml.{Elem, Node}

case class ComposedBlock(
  rectangle: Rectangle,
  textBlocks: Seq[TextBlock],
  id: String = UUID.randomUUID().toString,
  idNext: Option[String] = None,
  styleRefs: Option[String] = None,
  tagRefs: Option[String] = None
) extends Block {
  override def translate(xDiff: Int, yDiff: Int): ComposedBlock =
    ComposedBlock(rectangle.translate(xDiff, yDiff), textBlocks.map(_.translate(xDiff, yDiff)))

  override def rotate(imageInfo: ImageInfo): ComposedBlock =
    ComposedBlock(rectangle.rotate(imageInfo), textBlocks.map(_.rotate(imageInfo)))

  override def rescale(scale: Double): ComposedBlock = this.copy(
    rectangle = this.rectangle.rescale(scale),
    textBlocks = this.textBlocks.map(_.rescale(scale)).collect { case tb: TextBlock => tb }
  )

  override def toXml: Elem =
    <ComposedBlock ID={id} HPOS={rectangle.left.toString} VPOS={rectangle.top.toString} WIDTH={rectangle.width.toString} HEIGHT={rectangle.height.toString} STYLEREFS={styleRefs.orNull} TAGREFS={tagRefs.orNull} IDNEXT={idNext.orNull}>
      {textBlocks.map(_.toXml)}</ComposedBlock>

  lazy val allWords: Seq[Word] = textBlocks.flatMap(_.allWords)
  lazy val combinedWords: Seq[Word] = textBlocks.flatMap(_.combinedWords)
  lazy val textLinesWithRectangles: Seq[(TextLine, Rectangle)] = textBlocks.flatMap(_.textLinesWithRectangles)

  override def draw(mat: Mat): Unit = {
    opencv_imgproc.rectangle(mat, new Point(rectangle.left - 4, rectangle.top - 4), new Point(rectangle.left + rectangle.width + 8, rectangle.top + rectangle.height + 8), AbstractScalar.YELLOW,
      2, LINE_8, 0)
    this.textBlocks.foreach(_.draw(mat))
  }

  override lazy val content: String = textBlocks.map(_.content).mkString("\n")

  override def transform(partialFunction: PartialFunction[AltoElement, AltoElement]): ComposedBlock = {
    val transformed = if (partialFunction.isDefinedAt(this)) { partialFunction(this).asInstanceOf[ComposedBlock] } else { this }
    val newTextBlocks = transformed.textBlocks.map(_.transform(partialFunction)).collect { case block: TextBlock => block }
    transformed.copy(textBlocks = newTextBlocks)
  }
}

object ComposedBlock {
  def fromXML(imageInfo: ImageInfo, node: Node): ComposedBlock = {
    val paragraphs = node.child.collect {
      case elem: Elem if elem.label == "TextBlock" => TextBlock.fromXML(imageInfo, elem)
    }.toSeq

    val id = node \@ "ID"
    val idOption = Option.when(id.nonEmpty)(id).getOrElse(UUID.randomUUID().toString)
    val idNext = node \@ "IDNEXT"
    val idNextOption = Option.when(idNext.nonEmpty)(idNext)
    val tagRefs = node \@ "TAGREFS"
    val tagRefsOption = Option.when(tagRefs.nonEmpty)(tagRefs)
    val styleRefs = node \@ "STYLEREFS"
    val styleRefsOption = Option.when(styleRefs.nonEmpty)(styleRefs)

    ComposedBlock(Rectangle.fromXML(BlockType.Paragraph.entryName, node), paragraphs, id = idOption, idNext = idNextOption, styleRefs = styleRefsOption, tagRefs = tagRefsOption)
  }
}
