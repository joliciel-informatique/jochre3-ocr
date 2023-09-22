package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.corpus.TextSimplifier

import scala.language.implicitConversions
import scala.util.matching.Regex

object YiddishTextSimpifier extends TextSimplifier {

  private implicit class StringWithRegex(val s: String) {
    def replaceRegex(regex: Regex, replacement: String): String = regex.replaceAllIn(s, replacement)
  }

  private val invalidKomets = """(?<!א)ָ""".r
  private val pasekhTsveyYudnSeparate = """ייַ""".r
  private val invalidPasekh = """(?<![אײ])ַ""".r
  private val invalidKhirik = """ִ(?!י)""".r
  private val invalidDagesh = """(?<![וכפבת])ּa""".r
  private val invalidRafe = """(?<![בפכ])ֿ""".r
  private val invalidSin = """(?<!ש)ׂ""".r
  private val invalidNiqqud = """[ְֱֲֳֵֶֹֻׁ]""".r

  private val latinAlphabet = """[a-zA-Z]""".r
  private val cyrillicAlphabet = """[А-яЁё]""".r

  override def simplify(text: String): String = {
    text.replaceRegex(invalidKomets, "")
      .replaceRegex(pasekhTsveyYudnSeparate, "ײַ")
      .replaceRegex(invalidPasekh, "")
      .replaceRegex(invalidKhirik, "")
      .replaceRegex(invalidDagesh, "")
      .replaceRegex(invalidRafe, "")
      .replaceRegex(invalidSin, "")
      .replaceRegex(invalidNiqqud, "")
      .replaceRegex(latinAlphabet, "L")
      .replaceRegex(cyrillicAlphabet, "C")
  }
}
