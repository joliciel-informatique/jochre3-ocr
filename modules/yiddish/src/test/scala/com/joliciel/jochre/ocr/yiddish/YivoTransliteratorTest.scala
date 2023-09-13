package com.joliciel.jochre.ocr.yiddish

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class YivoTransliteratorTest extends AnyFlatSpec
  with Matchers
{
  val normal = Seq(
    "ייִדן",
    "אָוונט",
    "איך האָב געגנבֿעט אַ בוך.",
    "דאָס איז ממש אַ מחיה."
  )
  val romanized = Seq(
    "yidn",
    "ovnt",
    "ikh hob geganvet a bukh.",
    "dos iz mamesh a mekhaye."
  )

  val hebrewWithoutLoshnKoydesh = Seq(
    "ייִדן",
    "אָװנט",
    "איך האָב געגאַנװעט אַ בוך.",
    "דאָס איז מאַמעש אַ מעכײַע."
  )

  "transliterate" should "correctly transcribe Hebrew alphabet to Romanized" in {
    val replies = normal.map(YivoTransliterator.transliterate(_, true))
    replies shouldEqual romanized
  }

  "detransliterate" should "correctly transcribe Romanized to Hebrew alphabet" in {
    val replies = romanized.map(YivoTransliterator.detransliterate(_, true))
    replies shouldEqual normal.map(YivoTransliterator.replaceWithPrecombined(_))
  }

  "detransliterate" should "correctly transcribe Romanized to Hebrew alphabet without loshn koydesh" in {
    val replies = romanized.map(YivoTransliterator.detransliterate(_, false))
    replies shouldEqual hebrewWithoutLoshnKoydesh.map(YivoTransliterator.replaceWithPrecombined(_))
  }

}
