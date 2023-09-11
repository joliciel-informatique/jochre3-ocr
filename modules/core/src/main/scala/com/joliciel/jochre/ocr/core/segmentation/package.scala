package com.joliciel.jochre.ocr.core

import enumeratum.{Enum, EnumEntry}

package object segmentation {
  sealed trait Block extends EnumEntry

  object Block extends Enum[Block] {
    val values: IndexedSeq[Block] = findValues

    case object Paragraph extends Block
    case object TextBox extends Block
    case object Illustration extends Block
    case object Table extends Block
  }
}
