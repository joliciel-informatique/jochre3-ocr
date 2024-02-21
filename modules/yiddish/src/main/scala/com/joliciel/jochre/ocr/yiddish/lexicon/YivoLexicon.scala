package com.joliciel.jochre.ocr.yiddish.lexicon

import com.joliciel.jochre.ocr.core.lexicon.{Lexicon, TextFileLexicon}
import com.joliciel.jochre.ocr.yiddish.{YiddishConfig, YiddishTextSimpifier}
import com.joliciel.yivoTranscriber.YivoTranscriber
import zio.{Task, ZIO, ZLayer}

import java.io.File

object YivoLexiconService {
  val live: ZLayer[YiddishConfig, Nothing, YivoLexiconService] = ZLayer.fromFunction {
    YivoLexiconServiceImpl(_)
  }
}
trait YivoLexiconService {
  def getYivoLexicon: Task[YivoLexicon]
}

private case class YivoLexiconServiceImpl(yiddishConfig: YiddishConfig) extends YivoLexiconService {
  override def getYivoLexicon: Task[YivoLexicon] = ZIO.attempt {
    yiddishConfig.lexiconPath.map { path => YivoLexicon.load(new File(path)) }
      .getOrElse(new YivoLexiconImpl(Set.empty))
  }
}

trait YivoLexicon extends Lexicon {
  def toYivo(word: String, presimplified: Boolean = false): String
}

private class YivoLexiconImpl(entries: Set[String]) extends TextFileLexicon(entries) with YivoLexicon {
  private val yivoTranscriber = new YivoTranscriber()
  private var wordToYivo = Map.empty[String, String]

  override def getFrequency(word: String, presimplified: Boolean = false): Int = {
    val splits = splitWordOnPunctuation(word)
    val minFrequency = splits.map{
      case Part.Punctuation(_) => 1
      case Part.Number(_) => 1
      case Part.Abbreviation(text) =>
        val textWithoutQuote = quoteRegex.replaceAllIn(text, "")
        val yivoWord = toYivo(textWithoutQuote, presimplified)
        super.getFrequency(yivoWord)
      case Part.Text(text) =>
        val yivoWord = toYivo(text, presimplified)
        super.getFrequency(yivoWord)
    }.minOption.getOrElse(0)
    minFrequency
  }

  def toYivo(word: String, presimplified: Boolean = false): String = {
    wordToYivo.get(word)
      .getOrElse{
        val simplifiedWord = if (presimplified) { word } else { YivoLexicon.textSimplifier.simplify(word) }
        val yivoWord = yivoTranscriber.transcribe(simplifiedWord, false)
        wordToYivo = wordToYivo + (word -> yivoWord)
        yivoWord
      }
  }

  private val punctuationAndNotRegex = raw"(?U)\p{Punct}[^\p{Punct}]|[^\p{Punct}]\p{Punct}".r
  private val quoteRegex = raw"""(?U)[‛“'"’]""".r
  private val abbreviationRegex = raw"""(?U)\w+[‛“'"’]\w+""".r

  private val dotRegex = raw"""(?U)\.""".r
  private val decimalNumberRegex = raw"""(?U)\d+\.\d+""".r
  private val punctuationRegex = raw"""(?U)\p{Punct}+""".r
  private val numberRegex = raw"""(?U)\d+""".r

  private val punctuationSplitter = raw"""(?U)((?<=\p{Punct}+)|(?=\p{Punct}+))""".r

  private sealed trait Part
  private object Part {
    case class Text(text: String) extends Part
    case class Number(text: String) extends Part
    case class Abbreviation(text: String) extends Part
    case class Punctuation(text: String) extends Part
  }

  private def splitWordOnPunctuation(word: String): Seq[Part] = {
    if (punctuationAndNotRegex.findFirstIn(word).isDefined) {
      // split by punctuation
      val punctuationSplits = punctuationSplitter.split(word)
      val splitTriplets = (punctuationSplits :+ "" :+ "").lazyZip("" +: punctuationSplits :+ "").lazyZip("" +: "" +: punctuationSplits).toSeq

      val abbreviationIndexes = splitTriplets.zipWithIndex.flatMap {
        case ((next, current, prev), i) =>
          Option.when(
            (quoteRegex.matches(current) && abbreviationRegex.matches(f"$prev$current$next"))
          )(i - 1)
      }.toSet

      val decimalIndexes = splitTriplets.zipWithIndex.flatMap {
        case ((next, current, prev), i) =>
          Option.when(
              (dotRegex.matches(current) && decimalNumberRegex.matches(f"$prev$current$next"))
          )(i - 1)
      }.toSet

      val combinedIndexes = abbreviationIndexes ++ decimalIndexes

      val correctedSplits = punctuationSplits.zipWithIndex.flatMap { case (split, i) =>
        if (abbreviationIndexes.contains(i)) {
          Some(Part.Abbreviation(punctuationSplits(i - 1) ++ split ++ punctuationSplits(i + 1)))
        } else if (decimalIndexes.contains(i)) {
          Some(Part.Number(punctuationSplits(i - 1) ++ split ++ punctuationSplits(i + 1)))
        } else if (combinedIndexes.contains(i - 1)) {
          None
        } else if (combinedIndexes.contains(i + 1)) {
          None
        } else if (punctuationRegex.matches(split)) {
          Some(Part.Punctuation(split))
        } else if (numberRegex.matches(split)) {
          Some(Part.Number(split))
        } else {
          Some(Part.Text(split))
        }
      }
      correctedSplits
    } else {
      if (punctuationRegex.matches(word)) {
        Seq(Part.Punctuation(word))
      } else if (numberRegex.matches(word)) {
        Seq(Part.Number(word))
      } else {
        Seq(Part.Text(word))
      }
    }
  }

  private val impossibleWordRegex = raw"""(?U)(\w*[ןםךץף]\w+)|(\w*[LC]\w*[א-ת]\w*)|(\w*[א-ת]\w*[LC]\w*)|(\w*\D\w*\d\w*\D\w*)""".r

  override def isImpossible(word: String): Boolean = impossibleWordRegex.matches(word)
}

object YivoLexicon {
  val textSimplifier = YiddishTextSimpifier(false)
  def load(input: File): YivoLexicon =
    TextFileLexicon.load(input, Some(textSimplifier), (entries, _) => new YivoLexiconImpl(entries))

  def fromYiddishConfig(yiddishConfig: YiddishConfig): YivoLexicon =
    yiddishConfig.lexiconPath.map{ path => load(new File(path))}
      .getOrElse(new YivoLexiconImpl(Set.empty))
}
