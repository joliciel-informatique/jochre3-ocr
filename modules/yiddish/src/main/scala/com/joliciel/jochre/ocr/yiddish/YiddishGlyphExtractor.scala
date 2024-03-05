package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.corpus.GlyphExtractor

object YiddishGlyphExtractor {
  def main(args: Array[String]): Unit = {
    GlyphExtractor.execute(args,
      textSimplifier = YiddishTextSimpifier(replaceNonHebrewAlphabets = true)
    )
  }
}
