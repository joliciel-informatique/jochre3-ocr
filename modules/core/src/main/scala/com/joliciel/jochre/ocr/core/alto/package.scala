package com.joliciel.jochre.ocr.core

package object alto {
  case class AltoTransformerOptions(removeGlyphs: Boolean = false) {
    def withRemoveGlyphs(removeGlyphs: Boolean): AltoTransformerOptions = this.copy(removeGlyphs = removeGlyphs)

    def withRemoveGlyphs(removeGlyphs: Option[Boolean]): AltoTransformerOptions = removeGlyphs
      .map(removeGlyphs => withRemoveGlyphs(removeGlyphs))
      .getOrElse(this)
  }
}
