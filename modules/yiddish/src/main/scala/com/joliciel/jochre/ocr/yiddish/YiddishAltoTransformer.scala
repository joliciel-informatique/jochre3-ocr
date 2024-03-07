package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.alto.AltoTransformer
import com.joliciel.jochre.ocr.core.corpus.TextSimplifier
import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.model
import com.joliciel.jochre.ocr.core.model.{
  AltoElement,
  Glyph,
  Hyphen,
  SpellingAlternative,
  TextLine,
  Word
}
import com.joliciel.jochre.ocr.core.utils.{StringUtils, XmlImplicits}
import com.joliciel.jochre.ocr.yiddish.YiddishAltoTransformer.{Purpose, punctuationAndNotRegex}
import com.joliciel.jochre.ocr.yiddish.lexicon.YivoLexicon
import enumeratum._

case class YiddishAltoTransformer(
    yiddishConfig: YiddishConfig,
    lexicon: YivoLexicon,
    override val textSimplifier: Option[TextSimplifier] = Some(
      YiddishTextSimpifier(replaceNonHebrewAlphabets = false)
    )
) extends AltoTransformer
    with XmlImplicits {
  // match an alef if:
  // - it's at the start of word and not immediately followed by a yud, vov, vov yud or tsvey yudn
  // - it's in the middle of word and not immediately followed by a komets or pasekh
  private val shtumerAlef = raw"^א(?![יוײ ַָ])|(?<!^)א(?![ַָ])".r

  private val nonAbbreviationApostropheRegex = raw"""(?U)['‛’](\w\w+)""".r
  override def getAlternatives(content: String): Set[SpellingAlternative] = {
    val contentWithoutNonAbbreviationApostrophes =
      if (punctuationAndNotRegex.findFirstIn(content).isDefined) {
        nonAbbreviationApostropheRegex.pattern.matcher(content).replaceAll("$1")
      } else {
        content
      }
    val yivo = lexicon.toYivo(contentWithoutNonAbbreviationApostrophes)

    val fixedYivo = {
      if (lexicon.getFrequency(yivo) > 0 && yivo != "א") {
        yivo
      } else if (shtumerAlef.findFirstIn(yivo).isEmpty) {
        yivo
      } else {
        val alternatives = Seq(yivo)
        // replace each impossible shtumer alef by both a pasekh alef and a komets alef
        val matcher = shtumerAlef.pattern.matcher(yivo)
        val newAlternatives = Iterator
          .continually(matcher.find())
          .takeWhile(matched => matched)
          .foldLeft(alternatives) { case (alternatives, _) =>
            alternatives.flatMap { alternative =>
              val sb = new StringBuilder()
              sb.append(alternative.substring(0, matcher.start))
              sb.append('A')
              sb.append(alternative.substring(matcher.end))
              val sb2 = new StringBuilder()
              sb2.append(alternative.substring(0, matcher.start))
              sb2.append('O')
              sb2.append(alternative.substring(matcher.end))
              Seq(sb.toString(), sb2.toString())
            }
          }
          .map(_.replace("A", "אַ").replace("O", "אָ"))
        val realWord = newAlternatives.find(lexicon.getFrequency(_) > 0)
        realWord.getOrElse(newAlternatives.head)
      }
    }
    val yivoAlternative = Option.when(fixedYivo != content)(
      SpellingAlternative(Purpose.YIVO.entryName, fixedYivo)
    )
    val romanized = YivoTransliterator.transliterate(fixedYivo)

    val romanizedAlternative = Option.when(romanized != content)(
      SpellingAlternative(Purpose.Roman.entryName, romanized)
    )
    Set(yivoAlternative, romanizedAlternative).flatten
  }

  override val getSpecificRules: Seq[PartialFunction[AltoElement, AltoElement]] = Seq(
    Option.when(yiddishConfig.addHyphenElement)(
      YiddishAltoTransformer.addHyphenRule(textSimplifier)
    ),
    Some(YiddishAltoTransformer.punctuationSplitRule(textSimplifier)),
    Some(YiddishAltoTransformer.reverseNumberRule)
  ).flatten

  override val removeGlyphs: Boolean = true
}

object YiddishAltoTransformer extends XmlImplicits with StringUtils {
  sealed trait Purpose extends EnumEntry

  object Purpose extends Enum[Purpose] {
    override val values: IndexedSeq[Purpose] = findValues

    case object YIVO extends Purpose

    case object Roman extends Purpose
  }

  private val punctuationAndNotRegex =
    raw"(?U)\p{Punct}[^\p{Punct}]|[^\p{Punct}]\p{Punct}".r
  private val punctuationRegex = raw"(?U)\p{Punct}+".r
  private val quoteRegex = raw"""(?U)[‛“'"’]""".r
  private val abbreviationRegex = raw"""(?U)\w+[‛“'"’]\w+""".r

  private val dotRegex = raw"""(?U)\.""".r
  private val decimalNumberRegex = raw"""(?U)\d+\.\d+""".r

  def punctuationSplitRule(
      textSimplifier: Option[TextSimplifier] = None
  ): PartialFunction[AltoElement, AltoElement] = { case textLine: TextLine =>
    val splitWords: Seq[model.WordOrSpace] = textLine.wordsAndSpaces.flatMap {
      case word: Word =>
        val content = word.content
        val confidence = word.confidence
        if (punctuationAndNotRegex.findFirstIn(content).isDefined) {
          // split by punctuation
          val glyphs = word.glyphs
          val glyphSequences = glyphs.foldLeft(Seq[Seq[Glyph]]()) { case (words, glyph) =>
            if (words.isEmpty) {
              Seq(Seq(glyph))
            } else {
              val lastWord = words.last.map(_.content).mkString("")
              val glyphContent = glyph.content
              val lastWasPunct = punctuationRegex.matches(lastWord)
              val currentIsPunct = punctuationRegex.matches(glyphContent)
              if (currentIsPunct != lastWasPunct) {
                words :+ Seq(glyph)
              } else {
                words.init :+ (words.last :+ glyph)
              }
            }
          }
          val contentSeq =
            glyphSequences.map(glyphs => glyphs.map(_.content).mkString(""))
          val contentTriplets = (contentSeq :+ "" :+ "")
            .lazyZip("" +: contentSeq :+ "")
            .lazyZip("" +: "" +: contentSeq)
            .toSeq

          val abbreviationIndexes = contentTriplets.zipWithIndex.flatMap {
            case ((next, current, prev), i) =>
              Option.when(
                (quoteRegex.matches(current) && abbreviationRegex.matches(
                  f"$prev$current$next"
                )) ||
                  (dotRegex.matches(current) && decimalNumberRegex.matches(
                    f"$prev$current$next"
                  ))
              )(i - 1)
          }.toSet

          val correctedGlyphSequences = glyphSequences.zipWithIndex.flatMap { case (nodes, i) =>
            if (abbreviationIndexes.contains(i)) {
              Some(glyphSequences(i - 1) ++ nodes ++ glyphSequences(i + 1))
            } else if (abbreviationIndexes.contains(i - 1)) {
              None
            } else if (abbreviationIndexes.contains(i + 1)) {
              None
            } else {
              Some(nodes)
            }
          }

          val correctedWords = correctedGlyphSequences.map { glyphSeq =>
            glyphsToWord(glyphSeq, confidence, textSimplifier)
          }
          correctedWords
        } else {
          Seq(word)
        }
      case other => Seq(other)
    }
    textLine.copy(wordsAndSpaces = splitWords)
  }

  private def mean(seq: Seq[Double]): Double =
    if (seq.isEmpty) 0 else seq.sum / seq.size

  private def glyphsToWord(
      glyphs: Seq[Glyph],
      confidence: Double,
      textSimplifier: Option[TextSimplifier]
  ): Word = {
    val content = glyphs.map(glyph => glyph.content).mkString("")
    val simplifiedContent =
      textSimplifier.map(_.simplify(content)).getOrElse(content)
    val vpos = glyphs.map(_.rectangle.top)
    val hpos = glyphs.map(_.rectangle.left)
    val heights = glyphs.map(_.rectangle.height)
    val widths = glyphs.map(_.rectangle.width)

    val bottom =
      vpos.zip(heights).map { case (top, height) => top + height }.max
    val right =
      hpos.zip(widths).map { case (right, width) => right + width }.max
    val top = vpos.min
    val left = hpos.min
    val height = bottom - top
    val width = right - left

    val glyphConfidence = mean(glyphs.map(_.confidence))

    val myConfidence = if (punctuationRegex.matches(content)) {
      glyphConfidence
    } else {
      confidence
    }

    Word(
      simplifiedContent,
      Rectangle(left, top, width, height),
      glyphs,
      Seq.empty,
      myConfidence
    )
  }

  private val numberRegex = raw"\d+\.?\d+".r

  private val reverseNumberRule: PartialFunction[AltoElement, AltoElement] = { case word: Word =>
    val content = word.content
    if (numberRegex.matches(content)) {
      val inverseNumber = content.reverse
      word.copy(content = inverseNumber)
    } else {
      word
    }
  }

  private val hyphenRegex = raw"^(.+)([-־])$$".r
  def addHyphenRule(
      textSimplifier: Option[TextSimplifier] = None
  ): PartialFunction[AltoElement, AltoElement] = { case textLine: TextLine =>
    val children = textLine.wordsAndSpaces
    val lastWordAndHyphen = children.lastOption match {
      case Some(word: Word) =>
        val content = word.content
        val confidence = word.confidence
        content match {
          case hyphenRegex(contentBeforeHyphen, hyphenContent) =>
            val lastGlyphWithHyphen = Option.when(
              word.glyphs.size > 1 && word.content.endsWith(
                word.glyphs.last.content
              )
            ) {
              word.glyphs.last
            }
            lastGlyphWithHyphen match {
              case Some(lastGlyph) =>
                val stringGlyphs = word.glyphs.init

                val (letterGlyph, hyphenGlyph) =
                  if (lastGlyph.content.length <= 1) {
                    None -> lastGlyph
                  } else {
                    // In some cases the last glyph contains several letters
                    // We separate the hyphen from the remaining letters - these will be added to the preceding word
                    val numLetters = lastGlyph.content.length
                    val width = lastGlyph.rectangle.width
                    val widthHyphen = width / numLetters
                    val widthLetters = widthHyphen * (numLetters - 1)
                    // TODO: left here only works for right-to-left text (Yiddish)
                    val letterGlyph = lastGlyph.copy(
                      content = lastGlyph.content.substring(0, numLetters - 1),
                      rectangle = lastGlyph.rectangle.copy(
                        left = lastGlyph.rectangle.left + widthHyphen,
                        width = widthLetters
                      )
                    )
                    val hyphenGlyph = lastGlyph.copy(
                      content = lastGlyph.content.substring(numLetters - 1),
                      rectangle = lastGlyph.rectangle.copy(
                        width = widthHyphen
                      )
                    )
                    Some(letterGlyph) -> hyphenGlyph
                  }

                val newStringGlyphs = letterGlyph
                  .map { glyph => stringGlyphs :+ glyph }
                  .getOrElse(stringGlyphs)

                if (newStringGlyphs.isEmpty) {
                  Seq.empty
                } else {
                  val stringElemWithoutHyphen =
                    glyphsToWord(newStringGlyphs, confidence, textSimplifier)
                  val hyphen =
                    Hyphen(hyphenGlyph.content, hyphenGlyph.rectangle)
                  Seq(stringElemWithoutHyphen, hyphen)
                }
              case None =>
                // No hyphen glyph, so we split based on wordcontent only.
                val contentChars = stringToChars(contentBeforeHyphen)
                val totalChars = contentChars.length + 1
                val width = word.rectangle.width
                val widthHyphen = width / totalChars
                val widthLetters = widthHyphen * (totalChars - 1)
                Seq(
                  // TODO: this is for right-to-left only
                  word.copy(
                    content = contentBeforeHyphen,
                    rectangle = word.rectangle.copy(
                      left = word.rectangle.left - widthHyphen,
                      width = widthLetters
                    )
                  ),
                  Hyphen(
                    hyphenContent,
                    Rectangle(
                      word.rectangle.left,
                      word.rectangle.height,
                      widthHyphen,
                      word.rectangle.top
                    )
                  )
                )
            }
          case _ =>
            Seq.empty
        }
      case _ =>
        Seq.empty
    }
    if (lastWordAndHyphen.nonEmpty) {
      val newChildren = children.init ++ lastWordAndHyphen

      textLine.copy(wordsAndSpaces = newChildren)
    } else {
      textLine
    }
  }

  def apply(yiddishConfig: YiddishConfig): YiddishAltoTransformer =
    YiddishAltoTransformer(
      yiddishConfig,
      YivoLexicon.fromYiddishConfig(yiddishConfig)
    )
}
