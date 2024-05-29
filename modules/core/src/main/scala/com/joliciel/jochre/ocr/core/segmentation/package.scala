package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.graphics.{PredictedRectangle, Rectangle}
import com.typesafe.config.ConfigFactory
import enumeratum.{Enum, EnumEntry}
import io.circe.Decoder

package object segmentation {
  private[core] sealed trait BlockType extends EnumEntry {
    def isText: Boolean
    def yoloName: String
  }

  private[core] object BlockType extends Enum[BlockType] {
    val values: IndexedSeq[BlockType] = findValues

    case object TopLevelTextBlock extends BlockType {
      val isText: Boolean = true
      val yoloName = "TopLevelTextBlock"
    }
    case object Illustration extends BlockType {
      val isText: Boolean = false
      val yoloName = "Illustration"
    }

    def withYoloName(yoloName: String): Option[BlockType] =
      values.find(_.yoloName == yoloName)
  }

  sealed trait YoloPredictionType extends EnumEntry {
    def extension: String
    def endpoint: String
    def getLabel(category: String): String = category
    def maxWidth: Int
    def maxHeight: Int
    def defaultMinConfidence: Double
  }

  object YoloPredictionType extends Enum[YoloPredictionType] {
    private val config = ConfigFactory.load().getConfig("jochre.ocr.yolo")

    val values: IndexedSeq[YoloPredictionType] = findValues

    case object Blocks extends YoloPredictionType {
      val extension: String = "_block_prediction"
      val endpoint: String = "analyze-blocks"
      override def getLabel(category: String): String = BlockType
        .withYoloName(category)
        .map(_.entryName)
        .getOrElse(throw new Exception(f"Unknown BlockType: $category"))

      val maxWidth: Int = config.getInt("image-size-for-blocks")
      val maxHeight: Int = config.getInt("image-size-for-blocks")
      val defaultMinConfidence: Double = config.getDouble("default-min-confidence.blocks")
    }
    case object TextBlocks extends YoloPredictionType {
      val extension: String = "_textblock_prediction"
      val endpoint: String = "analyze-text-blocks"
      val maxWidth: Int = config.getInt("image-size-for-text-blocks")
      val maxHeight: Int = config.getInt("image-size-for-text-blocks")
      val defaultMinConfidence: Double = config.getDouble("default-min-confidence.text-blocks")
    }
    case object Lines extends YoloPredictionType {
      val extension: String = "_line_prediction"
      val endpoint: String = "analyze-lines"
      val maxWidth: Int = config.getInt("image-size-for-lines")
      val maxHeight: Int = config.getInt("image-size-for-lines")
      val defaultMinConfidence: Double = config.getDouble("default-min-confidence.lines")
    }
    case object Words extends YoloPredictionType {
      val extension: String = "_word_prediction"
      val endpoint: String = "analyze-words"
      val maxWidth: Int = config.getInt("image-size-for-words")
      val maxHeight: Int = config.getInt("image-size-for-words")
      val defaultMinConfidence: Double = config.getDouble("default-min-confidence.words")
    }
    case object Glyphs extends YoloPredictionType {
      val extension: String = "_glyph_prediction"
      val endpoint: String = "analyze-glyphs"
      val maxWidth: Int = config.getInt("image-size-for-glyphs")
      val maxHeight: Int = config.getInt("image-size-for-glyphs")
      val defaultMinConfidence: Double = config.getDouble("default-min-confidence.glyphs")
    }
    case object WordToGlyphs extends YoloPredictionType {
      val extension: String = "_word_to_glyph_prediction"
      val endpoint: String = "word-to-glyph"
      val maxWidth: Int = config.getInt("word-width-for-glyphs")
      val maxHeight: Int = config.getInt("word-height-for-glyphs")
      val defaultMinConfidence: Double = config.getDouble("default-min-confidence.word-to-glyph")
    }
  }

  private[segmentation] case class YoloResult(
      box: List[Int],
      category: String,
      confidence: Double
  ) {
    def toPredictedRectangle(label: String): PredictedRectangle =
      PredictedRectangle(
        label,
        Rectangle(box(0) - (box(2) / 2), box(1) - (box(3) / 2), box(2), box(3)),
        confidence
      )
  }

  private[segmentation] object YoloImplicits {
    implicit val decodeYoloResult: Decoder[YoloResult] =
      Decoder.forProduct3("box", "class", "conf")(YoloResult.apply)
  }
}
