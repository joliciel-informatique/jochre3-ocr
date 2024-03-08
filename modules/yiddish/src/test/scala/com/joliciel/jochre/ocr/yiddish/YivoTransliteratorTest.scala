package com.joliciel.jochre.ocr.yiddish

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class YivoTransliteratorTest extends AnyFlatSpec with Matchers {
  private val normal = Seq(
    "ייִדן",
    "אָוונט",
    "איך האָב געגנבֿעט אַ בוך.",
    "דאָס איז ממש אַ מחיה."
  )
  private val romanized = Seq(
    "yidn",
    "ovnt",
    "ikh hob geganvet a bukh.",
    "dos iz mamesh a mekhaye."
  )

  private val hebrewWithoutLoshnKoydesh = Seq(
    "ייִדן",
    "אָװנט",
    "איך האָב געגאַנװעט אַ בוך.",
    "דאָס איז מאַמעש אַ מעכײַע."
  )

  "transliterate" should "correctly transcribe Hebrew alphabet to Romanized" in {
    val replies = normal.map(YivoTransliterator.transliterate(_))
    replies shouldEqual romanized
  }

  "detransliterate" should "correctly transcribe Romanized to Hebrew alphabet" in {
    val replies = romanized.map(YivoTransliterator.detransliterate(_))
    replies shouldEqual normal.map(YivoTransliterator.replaceWithPrecombined)
  }

  "detransliterate" should "correctly transcribe Romanized to Hebrew alphabet without loshn koydesh" in {
    val replies = romanized.map(YivoTransliterator.detransliterate(_, loshnKoydesh = false))
    replies shouldEqual hebrewWithoutLoshnKoydesh.map(YivoTransliterator.replaceWithPrecombined)
  }

}
