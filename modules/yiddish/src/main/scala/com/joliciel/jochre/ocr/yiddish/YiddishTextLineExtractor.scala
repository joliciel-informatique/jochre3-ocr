package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.corpus.TextLineExtractor

object YiddishTextLineExtractor {
  def main(args: Array[String]): Unit = {
    TextLineExtractor.execute(args, textSimplifier = YiddishTextSimpifier)
  }
}
