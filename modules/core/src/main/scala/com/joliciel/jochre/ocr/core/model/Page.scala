package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.utils.MathUtils.MathImplicits._
import org.bytedeco.opencv.opencv_core.Mat

import scala.xml.{Elem, Node}

case class Page(
  id: String,
  height: Int,
  width: Int,
  physicalPageNumber: Int,
  rotation: Double,
  language: String,
  confidence: Double,
  blocks: Seq[Block]) extends PageElement {
  override def translate(xDiff: Int, yDiff: Int): Page = {
    this.copy(blocks = blocks.map(_.translate(xDiff, yDiff)).collect{
      case block: Block => block
    })
  }

  lazy val textBlocks: Seq[TextBlock] = blocks.collect{
    case textBlock: TextBlock => textBlock
  }

  lazy val composedBlocks: Seq[ComposedBlock] = blocks.collect {
    case composedBlock: ComposedBlock => composedBlock
  }

  lazy val illustrations: Seq[Illustration] = blocks.collect {
    case illustration: Illustration => illustration
  }

  lazy val allTextBoxes: Seq[TextBlock] = BlockSorter.sort(composedBlocks.flatMap(_.textBlocks) ++ textBlocks)
    .collect{
      case t:TextBlock => t
    }

  lazy val allTextLines: Seq[TextLine] = (textBlocks.flatMap(_.textLines) ++ composedBlocks.flatMap(_.textBlocks.flatMap(_.textLines))).sorted

  lazy val allWords: Seq[Word] = allTextLines.flatMap(_.words)

  lazy val allGlyphs: Seq[Glyph] = allWords.flatMap(_.glyphs)

  lazy val combinedWords: Seq[Word] = blocks.flatMap{
    case textBlock: TextBlock => textBlock.combinedWords
    case composedBlock: ComposedBlock => composedBlock.combinedWords
    case _: Illustration => Seq.empty
  }

  lazy val textLinesWithRectangles: Seq[(TextLine, Rectangle)] = blocks.flatMap {
    case textBlock: TextBlock => textBlock.textLinesWithRectangles
    case composedBlock: ComposedBlock => composedBlock.textLinesWithRectangles
    case _: Illustration => Seq.empty
  }

  val rectangle: Rectangle = Rectangle("", 0, 0, width, height)

  lazy val printArea: Rectangle = {
    val minLeft = this.blocks.map(_.rectangle.left).minOption.getOrElse(0)
    val minTop = this.blocks.map(_.rectangle.top).minOption.getOrElse(0)
    val maxRight = this.blocks.map(_.rectangle.right).maxOption.getOrElse(width)
    val maxBottom = this.blocks.map(_.rectangle.bottom).maxOption.getOrElse(height)
    Rectangle("",
      left = minLeft,
      top = minTop,
      width = maxRight - minLeft,
      height = maxBottom - minTop
    )
  }
  
  def croppedPrintArea(cropMargin: Double): Rectangle = {
    val xMargin = (width.toDouble * cropMargin).toInt
    val yMargin = (height.toDouble * cropMargin).toInt

    val newLeft = if (printArea.left - xMargin < 0) { 0 } else { printArea.left - xMargin }
    val newTop = if (printArea.top - yMargin < 0) { 0 } else { printArea.top - yMargin }
    val newWidth = printArea.width + (2 * xMargin)
    val newHeight = printArea.height + (2 * yMargin)

    Rectangle("",
      left = newLeft,
      top = newTop,
      width = if (newLeft + newWidth > width) {
        width - newLeft
      } else {
        newWidth
      },
      height = if (newTop + newHeight > height) {
        height - newTop
      } else {
        newHeight
      },
    )
  }

  def rotate(): Page = {
    this.rotate(ImageInfo(width, height, 0-rotation))
  }

  def unrotate(): Page = {
    this.rotate(ImageInfo(width, height, rotation))
  }

  override def rotate(imageInfo: ImageInfo): Page = {
    this.copy(blocks = blocks.map(_.rotate(imageInfo)).collect {
      case block: Block => block
    })
  }

  override def rescale(scale: Double): Page = {
    this.copy(blocks = blocks.map(_.rescale(scale)).collect {
      case block: Block => block
    })
  }

  def crop(rectangle: Rectangle): Page = {
    this.copy(
      height = rectangle.height,
      width = rectangle.width,
      blocks = this.blocks.map(
        _.translate(0 - rectangle.left, 0 - rectangle.top))
        .collect{ case b: Block => b}
    )
  }

  override def toXml: Elem =
    <Page ID={id} HEIGHT={height.toString} WIDTH={width.toString} PHYSICAL_IMG_NR={physicalPageNumber.toString} ROTATION={rotation.roundTo(2).toString}
          LANG={language} PC={confidence.roundTo(2).toString}>
      <PrintSpace HEIGHT={height.toString} WIDTH={width.toString} HPOS="0" VPOS="0">
        {blocks.map(_.toXml)}
      </PrintSpace>
    </Page>

  override def draw(mat: Mat): Unit = {
    this.blocks.foreach(_.draw(mat))
  }

  override def content: String = this.blocks.map{
    case composedBlock:ComposedBlock => composedBlock.content
    case textBlock:TextBlock => f"${textBlock.content}\n"
    case illustration:Illustration => illustration.content
  }.mkString

  override def transform(partialFunction: PartialFunction[AltoElement, AltoElement]): Page = {
    val transformed = if (partialFunction.isDefinedAt(this)) { partialFunction(this).asInstanceOf[Page] } else { this }
    val newBlocks = transformed.blocks.map(_.transform(partialFunction)).collect{ case block: Block => block }
    transformed.copy(blocks = newBlocks)
  }

  def withCleanIds: Page = {
    val baseId = f"$physicalPageNumber%05d"
    val newBlocks = blocks.zipWithIndex.map{
      case (composedBlock: ComposedBlock, i) =>
        val newTextBlocks = composedBlock.textBlocks.zipWithIndex.map{
          case (textBlock: TextBlock, j) => textBlock.copy(id = f"TB_${baseId}_${i+1}%03d_${j+1}%03d")
        }
        composedBlock.copy(id = f"CB_${baseId}_${i+1}%03d", textBlocks = newTextBlocks)
      case (textBlock: TextBlock, i) =>
        textBlock.copy(id = f"TB_${baseId}_${i+1}%03d_${0}%03d")
      case (illustration: Illustration, i) =>
        illustration.copy(id = f"IL_${baseId}_${i+1}%03d")
    }
    this.copy(blocks = newBlocks)
  }

  /**
   * For simpler comparison when testing.
   */
  def withoutIds: Page = this.transform{
    case textBlock: TextBlock => textBlock.copy(id = "")
    case composedBlock: ComposedBlock => composedBlock.copy(id = "")
    case illustration: Illustration => illustration.copy(id = "")
  }
}

object Page {
  def fromXML(node: Node): Page = {
    val page = (node \\ "Page").headOption.getOrElse(throw new Exception("No Page element found"))
    val id = page \@ "ID"
    val height = (page \@ "HEIGHT").toIntOption.getOrElse(0)
    val width = (page \@ "WIDTH").toIntOption.getOrElse(0)
    val physicalPageNumber = (page \@ "PHYSICAL_IMG_NR").toIntOption.getOrElse(0)
    val rotationStr = page \@ "ROTATION"
    val rotation = if (rotationStr.nonEmpty) {
      rotationStr.toDouble
    } else {
      val blockRotationStr = (page \\ "TextBlock").headOption.map(_ \@ "ROTATION").getOrElse("0")
      if (blockRotationStr.isEmpty) { 0 } else { blockRotationStr.toDouble }
    }
    val imageInfo = ImageInfo(width, height, rotation)
    val printSpace = (page \ "PrintSpace").headOption
    val languageStr = page \@"LANG"
    val language = if (languageStr.isEmpty) {
      "en"
    } else {
      languageStr
    }
    val confidence = (page \@ "PC").toDoubleOption.getOrElse(0.0)

    val blocks = printSpace.map(_.child.collect{
      case elem: Elem if elem.label == "TextBlock" => TextBlock.fromXML(imageInfo, elem)
      case elem: Elem if elem.label == "ComposedBlock" => ComposedBlock.fromXML(imageInfo, elem)
      case elem: Elem if elem.label == "Illustration" => Illustration.fromXML(elem)
    }.toSeq).getOrElse(Seq.empty)

    Page(id, height, width, physicalPageNumber, rotation, language, confidence, blocks)
  }
}