package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.corpus.TextSimplifier

import java.io.{File, PrintWriter}
import java.text.Normalizer
import scala.io.Source
import scala.language.implicitConversions
import scala.util.matching.Regex

case class YiddishTextSimpifier(replaceNonHebrewAlphabets: Boolean = false) extends TextSimplifier {

  private implicit class StringWithRegex(val s: String) {
    def replaceRegex(regex: Regex, replacement: String): String =
      regex.replaceAllIn(s, replacement)
  }

  private val pasekhTsveyYudnSeparate = """ייַ""".r
  private val tsveyYudnNotFollowedByKhirik = """(יי)(?!ִ)""".r
  private val vovYudNotFollowedByKhirik = """(וי)(?!ִ)""".r
  private val tsveyVovnNotFollowedByMelupm = """(וו)(?!ּ)""".r
  private val nonYivoKomets = """(?<!א)ָ""".r
  private val nonYivoPasekh = """(?<![אײ])ַ""".r
  private val nonYivoKhirik = """(?<!י)ִ""".r
  private val nonYivoDagesh = """(?<![וכפבת])ּ""".r
  private val nonYivoRafe = """(?<![בפכ])ֿ""".r
  private val nonYivoSinDot = """(?<!ש)ׂ""".r
  private val nonYivoNikud = """[ְֱֲֳֵֶֹֻׁ]""".r
  private val nonStandardMaqaf = """[-⸗]""".r
  private val nonStandardLongDash = """[𝆙←–—]""".r
  private val nonStandardSingleQuote = """['‛’׳]""".r
  private val nonStandardDoubleQuote = """["“״]|(‛‛)|(’’)|('')""".r
  private val nonStandardLowerDoubleQuote = """(,,)|(‚‚)""".r
  private val verticalBar = """|""".r
  private val otherSymbol = """[▼◦№⁂]""".r

  private val latinAlphabet = """(?U)\p{IsLatin}""".r
  private val cyrillicAlphabet = """(?U)\p{IsCyrillic}""".r
  private val greekAlphabet = """(?U)\p{IsGreek}""".r

  override def simplify(text: String): String = {
    val normalizedText = Normalizer.normalize(text, Normalizer.Form.NFD)

    val simplifiedText = normalizedText
      // Replace non-YIVO nikud first, for cases like a shin with a non-YIVO shva and a YIVO sin-dot
      .replaceRegex(nonYivoNikud, "")
      .replaceRegex(nonYivoKomets, "")
      .replaceRegex(pasekhTsveyYudnSeparate, "ײַ")
      .replaceRegex(tsveyYudnNotFollowedByKhirik, "ײ")
      .replaceRegex(tsveyVovnNotFollowedByMelupm, "װ")
      .replaceRegex(vovYudNotFollowedByKhirik, "ױ")
      .replaceRegex(nonYivoPasekh, "")
      .replaceRegex(nonYivoKhirik, "")
      .replaceRegex(nonYivoDagesh, "")
      .replaceRegex(nonYivoRafe, "")
      .replaceRegex(nonYivoSinDot, "")
      .replaceRegex(nonStandardMaqaf, "־")
      .replaceRegex(nonStandardLongDash, "—")
      .replaceRegex(nonStandardSingleQuote, "’")
      .replaceRegex(nonStandardDoubleQuote, "“")
      .replaceRegex(nonStandardLowerDoubleQuote, "„")
      // Get rid of stray vertical bars left over by Jochre 2
      .replaceRegex(verticalBar, "")
      .replaceRegex(otherSymbol, "•")

    if (replaceNonHebrewAlphabets) {
      simplifiedText
        .replaceRegex(latinAlphabet, "L")
        .replaceRegex(cyrillicAlphabet, "C")
        .replaceRegex(greekAlphabet, "G")
    } else {
      simplifiedText
    }
  }

  def main(args: Array[String]): Unit = {
    val inFile = args(0)
    val outFile = args(1)
    val writer = new PrintWriter(new File(outFile))
    for (line <- Source.fromFile(inFile).getLines) {
      val parts = line.split("\t")
      if (parts.length == 2) {
        val image = parts(0)
        val text = parts(1)
        val simplifiedText = simplify(text).replaceAll("֏", "")
        if (text.nonEmpty) {
          writer.write(f"$image\t$simplifiedText\n")
          writer.flush()
        }
      }
    }
    writer.close()
  }
}
