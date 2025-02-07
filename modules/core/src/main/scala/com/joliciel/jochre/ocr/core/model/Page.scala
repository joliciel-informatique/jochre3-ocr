package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.graphics.{BlockSorter, ImageInfo, Rectangle}
import com.joliciel.jochre.ocr.core.utils.MathUtils.MathImplicits._
import com.joliciel.jochre.ocr.core.utils.StringUtils
import com.typesafe.config.ConfigFactory
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
    blocks: Seq[Block]
) extends PageElement {
  val leftToRight = StringUtils.isLeftToRight(language)

  override def translate(xDiff: Int, yDiff: Int): Page = {
    this.copy(blocks = blocks.map(_.translate(xDiff, yDiff)).collect { case block: Block =>
      block
    })
  }

  lazy val textBlocks: Seq[TextBlock] = blocks.collect { case textBlock: TextBlock =>
    textBlock
  }

  lazy val composedBlocks: Seq[ComposedBlock] = blocks.collect { case composedBlock: ComposedBlock =>
    composedBlock
  }

  lazy val illustrations: Seq[Illustration] = blocks.collect { case illustration: Illustration =>
    illustration
  }

  lazy val allTextBlocks: Seq[TextBlock] = BlockSorter
    .sort(composedBlocks ++ textBlocks, leftToRight)
    .collect {
      case c: ComposedBlock => c.textBlocks
      case t: TextBlock     => Seq(t)
    }
    .flatten

  lazy val allTextLines: Seq[TextLine] = (textBlocks.flatMap(
    _.textLines
  ) ++ composedBlocks.flatMap(_.textBlocks.flatMap(_.textLines))).sorted

  lazy val allWords: Seq[Word] = allTextLines.flatMap(_.words)

  lazy val allGlyphs: Seq[Glyph] = allWords.flatMap(_.glyphs)

  lazy val combinedWords: Seq[Word] = blocks.flatMap {
    case textBlock: TextBlock         => textBlock.combinedWords
    case composedBlock: ComposedBlock => composedBlock.combinedWords
    case _: Illustration              => Seq.empty
  }

  lazy val textLinesWithRectangles: Seq[(TextLine, Rectangle)] =
    blocks.flatMap {
      case textBlock: TextBlock         => textBlock.textLinesWithRectangles
      case composedBlock: ComposedBlock => composedBlock.textLinesWithRectangles
      case _: Illustration              => Seq.empty
    }

  val rectangle: Rectangle = Rectangle(0, 0, width, height)

  lazy val printArea: Rectangle = {
    val minLeft = this.blocks.map(_.rectangle.left).minOption.getOrElse(0)
    val minTop = this.blocks.map(_.rectangle.top).minOption.getOrElse(0)
    val maxRight = this.blocks.map(_.rectangle.right).maxOption.getOrElse(width)
    val maxBottom =
      this.blocks.map(_.rectangle.bottom).maxOption.getOrElse(height)
    Rectangle(
      left = minLeft,
      top = minTop,
      width = maxRight - minLeft,
      height = maxBottom - minTop
    )
  }

  def croppedPrintArea(cropMargin: Double): Rectangle = {
    val xMargin = (width.toDouble * cropMargin).toInt
    val yMargin = (height.toDouble * cropMargin).toInt

    val newLeft = if (printArea.left - xMargin < 0) { 0 }
    else { printArea.left - xMargin }
    val newTop = if (printArea.top - yMargin < 0) { 0 }
    else { printArea.top - yMargin }
    val newWidth = printArea.width + (2 * xMargin)
    val newHeight = printArea.height + (2 * yMargin)

    Rectangle(
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
      }
    )
  }

  def rotate(): Page = {
    this.rotate(ImageInfo(width, height, 0 - rotation))
  }

  def unrotate(): Page = {
    this.rotate(ImageInfo(width, height, rotation))
  }

  override def rotate(imageInfo: ImageInfo): Page = {
    this.copy(blocks = blocks.map(_.rotate(imageInfo)).collect { case block: Block =>
      block
    })
  }

  override def rescale(scale: Double): Page = {
    this.copy(blocks = blocks.map(_.rescale(scale)).collect { case block: Block =>
      block
    })
  }

  def crop(rectangle: Rectangle): Page = {
    this.copy(
      height = rectangle.height,
      width = rectangle.width,
      blocks = this.blocks
        .map(_.translate(0 - rectangle.left, 0 - rectangle.top))
        .collect { case b: Block => b }
    )
  }

  override def toXml: Elem =
    <Page ID={id}
          HEIGHT={height.toString} WIDTH={width.toString}
          PHYSICAL_IMG_NR={physicalPageNumber.toString} ROTATION={rotation.roundTo(2).toString}
          LANG={language} PC={confidence.roundTo(2).toString}
    ><PrintSpace HEIGHT={height.toString} WIDTH={width.toString} 
                 HPOS="0" VPOS="0"
    >{blocks.map(_.toXml)}</PrintSpace></Page>

  override def draw(mat: Mat): Unit = {
    this.blocks.foreach(_.draw(mat))
  }

  override lazy val content: String = this.blocks
    .collect { case textContainer: TextContainer =>
      textContainer.content
    }
    .mkString("\n\n")

  lazy val processedContent: String = this.blocks
    .collect { case textContainer: TextContainer =>
      textContainer.processedContent
    }
    .mkString("\n")

  override def transform(
      partialFunction: PartialFunction[AltoElement, AltoElement]
  ): Page = {
    val transformed = if (partialFunction.isDefinedAt(this)) {
      partialFunction(this).asInstanceOf[Page]
    } else { this }
    val newBlocks =
      transformed.blocks.map(_.transform(partialFunction)).collect { case block: Block =>
        block
      }
    transformed.copy(blocks = newBlocks)
  }

  def withCleanIds: Page = {
    val baseId = f"$physicalPageNumber%05d"
    val newBlocks = blocks.zipWithIndex.map {
      case (composedBlock: ComposedBlock, i) =>
        val newTextBlocks = composedBlock.textBlocks.zipWithIndex.map { case (textBlock: TextBlock, j) =>
          textBlock.copy(id = f"TB_${baseId}_${i + 1}%03d_${j + 1}%03d")
        }
        composedBlock.copy(
          id = f"CB_${baseId}_${i + 1}%03d",
          textBlocks = newTextBlocks
        )
      case (textBlock: TextBlock, i) =>
        textBlock.copy(id = f"TB_${baseId}_${i + 1}%03d_${0}%03d")
      case (illustration: Illustration, i) =>
        illustration.copy(id = f"IL_${baseId}_${i + 1}%03d")
    }
    this.copy(blocks = newBlocks)
  }

  /** For simpler comparison when testing.
    */
  def withoutIds: Page = this.transform {
    case textBlock: TextBlock         => textBlock.copy(id = "")
    case composedBlock: ComposedBlock => composedBlock.copy(id = "")
    case illustration: Illustration   => illustration.copy(id = "")
  }

  def withLanguage(newLanguage: String): Page = {
    if (this.language == newLanguage) {
      this
    } else {
      val newBlocks = this.blocks.map {
        case textBlock: TextBlock => textBlock.withDefaultLanguage(newLanguage)
        case composedBlock: ComposedBlock =>
          composedBlock.copy(textBlocks = composedBlock.textBlocks.map(_.withDefaultLanguage(newLanguage)))
        case other => other
      }

      val oldLeftToRight = StringUtils.isLeftToRight(this.language)
      val newLeftToRight = StringUtils.isLeftToRight(newLanguage)
      val sortedBlocks = if (oldLeftToRight != newLeftToRight) {
        BlockSorter.sort(newBlocks, newLeftToRight)
      } else {
        newBlocks
      }
      this.copy(language = newLanguage, blocks = newBlocks)
    }
  }

  def withDefaultLanguage: Page = {
    this.copy(blocks = this.blocks.map {
      case composedBlock: ComposedBlock =>
        composedBlock.withDefaultLanguage(this.language)
      case textBlock: TextBlock => textBlock.withDefaultLanguage(this.language)
      case other                => other
    })
  }
}

object Page {
  private val defaultLanguage =
    ConfigFactory.load().getConfig("jochre.ocr").getString("language")

  def fromXML(node: Node): Page = {
    val id = node \@ "ID"
    val height = (node \@ "HEIGHT").toIntOption.getOrElse(0)
    val width = (node \@ "WIDTH").toIntOption.getOrElse(0)
    val physicalPageNumber =
      (node \@ "PHYSICAL_IMG_NR").toIntOption.getOrElse(0)
    val rotationStr = node \@ "ROTATION"
    val rotation = if (rotationStr.nonEmpty) {
      rotationStr.toDouble
    } else {
      val blockRotationStr =
        (node \\ "TextBlock").headOption.map(_ \@ "ROTATION").getOrElse("0")
      if (blockRotationStr.isEmpty) { 0 }
      else { blockRotationStr.toDouble }
    }
    val imageInfo = ImageInfo(width, height, rotation)
    val printSpace = (node \ "PrintSpace").headOption
    val languageStr = node \@ "LANG"
    val language = if (languageStr.isEmpty) {
      defaultLanguage
    } else {
      languageStr
    }
    val confidence = (node \@ "PC").toDoubleOption.getOrElse(0.0)

    val blocks = printSpace
      .map(_.child.collect {
        case elem: Elem if elem.label == "TextBlock" =>
          TextBlock.fromXML(imageInfo, elem)
        case elem: Elem if elem.label == "ComposedBlock" =>
          ComposedBlock.fromXML(imageInfo, elem)
        case elem: Elem if elem.label == "Illustration" =>
          Illustration.fromXML(elem)
      }.toSeq)
      .getOrElse(Seq.empty)

    Page(
      id,
      height,
      width,
      physicalPageNumber,
      rotation,
      language,
      confidence,
      blocks
    )
  }
}
