package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.corpus.WordExtractor

object YiddishWordExtractor {
  def main(args: Array[String]): Unit = {
    WordExtractor.execute(args,
      textSimplifier = YiddishTextSimpifier(replaceNotYiddishAlphabets = true)
    )
  }
}
