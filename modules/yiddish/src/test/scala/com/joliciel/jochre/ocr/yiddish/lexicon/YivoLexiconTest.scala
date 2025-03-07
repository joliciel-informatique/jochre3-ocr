package com.joliciel.jochre.ocr.yiddish.lexicon

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.joliciel.jochre.ocr.yiddish.YiddishConfig

class YivoLexiconTest extends AnyFlatSpec with Matchers {
  private val yiddishConfig: YiddishConfig = YiddishConfig.fromConfig
  private val lexicon = YivoLexicon.fromYiddishConfig(yiddishConfig)

  "getFrequency" should "correctly calculate negative frequency for impossible words" in {
    lexicon.getFrequency("ארץישראל") shouldEqual -1
  }

  it should "correctly calculate positive frequency for known words" in {
    lexicon.getFrequency("דער") shouldEqual 1
  }

  it should "correctly calculate zero frequency for unknown words" in {
    lexicon.getFrequency("האָצנפּלאָץ") shouldEqual 0
  }

  it should "correctly calculate positive frequency for hyphenated words if both parts are known" in {
    lexicon.getFrequency("דער־זי") shouldEqual 1
  }

  it should "correctly calculate zero frequency for hyphenated words if one part is unknown" in {
    lexicon.getFrequency("דער־האָצנפּלאָץ") shouldEqual 0
  }

  it should "correctly calculate negative frequency for hyphenated words if one part is impossible" in {
    lexicon.getFrequency("דער־דץר") shouldEqual -1
  }

  it should "correctly calculate positive frequency for words ignoring punctuation" in {
    lexicon.getFrequency("“דער־זי") shouldEqual 1
  }
}
