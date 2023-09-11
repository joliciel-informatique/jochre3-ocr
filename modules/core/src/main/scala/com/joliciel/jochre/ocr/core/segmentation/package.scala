package com.joliciel.jochre.ocr.core

import enumeratum.{Enum, EnumEntry}

package object segmentation {
  sealed trait BlockType extends EnumEntry {
    def isText: Boolean
  }

  object BlockType extends Enum[BlockType] {
    val values: IndexedSeq[BlockType] = findValues

    case object Paragraph extends BlockType {
      val isText: Boolean = true
    }
    case object TextBox extends BlockType {
      val isText: Boolean = true
    }
    case object Illustration extends BlockType {
      val isText: Boolean = false
    }
    case object Table extends BlockType {
      val isText: Boolean = false
    }
  }
}
