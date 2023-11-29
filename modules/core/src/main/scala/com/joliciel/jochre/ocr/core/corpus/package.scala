package com.joliciel.jochre.ocr.core

import enumeratum._

package object corpus {
  sealed trait YoloObjectType extends EnumEntry

  object YoloObjectType extends Enum[YoloObjectType] {
    val values = findValues

    /** A baseline */
    case object BaseLine extends YoloObjectType

    /** A baseline that isn't at the end of a paragraph */
    case object NonFinalBaseLine extends YoloObjectType

    /** The last baseline of a paragraph */
    case object FinalBaseLine extends YoloObjectType

    /** A single word */
    case object Word extends YoloObjectType

    /**
     * A single glyph
     */
    case object Glyph extends YoloObjectType

    /** A separator between two words */
    case object WordSeparator extends YoloObjectType

    /** A separator between two glyphs */
    case object GlyphSeparator extends YoloObjectType
  }

  case class YoloBox(yoloClass: YoloObjectType, xCenter: Double, yCenter: Double, width: Double, height: Double)
}
