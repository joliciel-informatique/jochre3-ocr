package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.model.ImageLabel.{PredictedRectangle, Rectangle}
import enumeratum.{Enum, EnumEntry}
import io.circe.Decoder

package object segmentation {
  private[core] sealed trait BlockType extends EnumEntry {
    def isText: Boolean
    def yoloName: String
  }

  private[core] object BlockType extends Enum[BlockType] {
    val values: IndexedSeq[BlockType] = findValues

    case object Paragraph extends BlockType {
      val isText: Boolean = true
      val yoloName = "paragraph"
    }
    case object TextBox extends BlockType {
      val isText: Boolean = true
      val yoloName = "text_box"
    }
    case object Image extends BlockType {
      val isText: Boolean = false
      val yoloName = "image"
    }
    case object Table extends BlockType {
      val isText: Boolean = false
      val yoloName = "table"
    }

    def withYoloName(yoloName: String): Option[BlockType] =
      values.find(_.yoloName==yoloName)
  }

  private[segmentation] sealed trait YoloPredictionType extends EnumEntry {
    def extension: String
    def endpoint: String
    def getLabel(category: String): String = category
  }

  private[segmentation] object YoloPredictionType extends Enum[YoloPredictionType] {
    val values: IndexedSeq[YoloPredictionType] = findValues

    case object Blocks extends YoloPredictionType {
      val extension: String = "_block_prediction.png"
      val endpoint: String = "analyze-blocks"
      override def getLabel(category: String): String = BlockType.withYoloName(category).map(_.entryName).getOrElse(throw new Exception(f"Unknown BlockType: $category"))
    }
    case object Lines extends YoloPredictionType {
      val extension: String = "_line_prediction.png"
      val endpoint: String = "analyze-lines"
    }
    case object Words extends YoloPredictionType {
      val extension: String = "_word_prediction.png"
      val endpoint: String = "analyze-words"
    }
    case object Glyphs extends YoloPredictionType {
      val extension: String = "_glyph_prediction.png"
      val endpoint: String = "analyze-glyphs"
    }
  }

  private[segmentation] case class YoloResult(box: List[Int], category: String, confidence: Double) {
    def toPredictedRectangle(label: String): PredictedRectangle = PredictedRectangle(Rectangle(label, box(0) - (box(2) / 2), box(1) - (box(3) / 2), box(2), box(3)), confidence)
  }

  private[segmentation] object YoloImplicits {
    implicit val decodeYoloResult: Decoder[YoloResult] =
      Decoder.forProduct3("box", "class", "conf")(YoloResult.apply)
  }
}
