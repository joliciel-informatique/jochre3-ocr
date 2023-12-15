package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.model.ImageLabel.{PredictedRectangle, Rectangle}
import enumeratum.{Enum, EnumEntry}
import io.circe.Decoder

package object segmentation {
  sealed trait BlockType extends EnumEntry {
    def isText: Boolean
    def yoloName: String
  }

  object BlockType extends Enum[BlockType] {
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

  case class YoloResult(box: List[Int], category: String, confidence: Double) {
    def toPredictedRectangle(label: String): PredictedRectangle = PredictedRectangle(Rectangle(label, box(0) - (box(2) / 2), box(1) - (box(3) / 2), box(2), box(3)), confidence)
  }

  object YoloImplicits {
    implicit val decodeYoloResult: Decoder[YoloResult] =
      Decoder.forProduct3("box", "class", "conf")(YoloResult.apply)
  }
}
