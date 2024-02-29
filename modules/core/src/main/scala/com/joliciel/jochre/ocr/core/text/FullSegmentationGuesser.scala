package com.joliciel.jochre.ocr.core.text

import com.joliciel.jochre.ocr.core.JochreCLI
import com.joliciel.jochre.ocr.core.learning.{GlyphGuesser, GlyphGuesserForAnotherAlphabet, GlyphGuessersForOtherAlphabets, Prediction}
import com.joliciel.jochre.ocr.core.lexicon.Lexicon
import com.joliciel.jochre.ocr.core.model.{AltoElement, Block, BlockSorter, ComposedBlock, Page, TextBlock, TextLine, WithLanguage, WithRectangle, Word}
import com.joliciel.jochre.ocr.core.utils.{OutputLocation, StringUtils}
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import zio.{Task, ZIO, ZIOAppArgs, ZLayer}

import scala.collection.mutable

object FullSegmentationGuesserService {
  val live: ZLayer[GlyphGuesser with GlyphGuessersForOtherAlphabets with Lexicon with FullSegmentationGuesserConfig, Nothing, TextGuesserService] = ZLayer.fromFunction{
    FullSegmentationGuesserServiceImpl(_, _, _, _)
  }
}

private[text] case class FullSegmentationGuesserServiceImpl(glyphGuesser: GlyphGuesser, glyphGuessersForOtherAlphabets: GlyphGuessersForOtherAlphabets, lexicon: Lexicon, config: FullSegmentationGuesserConfig) extends TextGuesserService {
  def getTextGuesser(): Task[FullSegmentationGuesser] = {
    ZIO.attempt(FullSegmentationGuesser(glyphGuesser, glyphGuessersForOtherAlphabets, lexicon, config))
  }
}

case class FullSegmentationGuesserConfig(beamWidth: Int = 5, unknownWordFactor: Double = 0.75)

object FullSegmentationGuesserConfig {
  def fromConfig: FullSegmentationGuesserConfig = {
    val config = ConfigFactory.load ().getConfig ("jochre.ocr.text-guesser")

    val unknownWordFactor = config.getDouble ("unknown-word-factor")
    val beamWidth = config.getInt("beam-width")
    FullSegmentationGuesserConfig(beamWidth, unknownWordFactor)
  }

  val configLayer: ZLayer[Any, Throwable, FullSegmentationGuesserConfig] = ZLayer.fromZIO{ZIO.attempt{
    fromConfig
  }}

  val appArgsLayer: ZLayer[ZIOAppArgs, Throwable, FullSegmentationGuesserConfig] = ZLayer.fromZIO {
    for {
      args <- ZIOAppArgs.getArgs
      jochreCLI <- ZIO.attempt{new JochreCLI(args)}
    } yield {
      fromConfig.copy(
        beamWidth = jochreCLI.beamWidth.toOption.getOrElse(fromConfig.beamWidth),
        unknownWordFactor = jochreCLI.unknownWordFactor.toOption.getOrElse(fromConfig.unknownWordFactor)
      )
    }
  }
}

case class FullSegmentationGuesser(
  glyphGuesser: GlyphGuesser,
  glyphGuessersForOtherAlphabets: GlyphGuessersForOtherAlphabets,
  lexicon: Lexicon = Lexicon.default,
  config: FullSegmentationGuesserConfig
) extends TextGuesser {
  private val log = LoggerFactory.getLogger(getClass)

  private val beamWidth = config.beamWidth
  private val unknownWordFactor = config.unknownWordFactor

  /**
   * Given an image and a pre-segmented [[Page]] structure, attempt to guess the text within the page
   * by assigning content to the resulting page.
   */
  override def guess(page: Page, mat: Mat, fileName: String, debugLocation: Option[OutputLocation]): Task[Page] = ZIO.attempt{
    val withGuesses = if (beamWidth <= 1) {
      page.transform(guessWithoutBeam(mat))
    } else {
      page.transform(guessWithBeam(mat))
    }
    withGuesses.transform(changeTextLineLanguageIfRequired)
      .transform(changeTextBlockLanguageIfRequired)
      .transform(changePageLanguageIfRequired)
  }

  private def guessWithoutBeam(mat: Mat): PartialFunction[AltoElement, AltoElement] = {
    case word: Word =>
      val guessedWord = guessWordWithoutBeam(mat, word, glyphGuesser)
      val otherAlphabetWord = guessWithOtherAlphabets(mat, guessedWord).getOrElse(guessedWord)
      otherAlphabetWord
  }

  private def guessWordWithoutBeam(mat: Mat, word: Word, glyphGuesser: GlyphGuesser): Word = {
    val predictionPerGlyph = word.glyphs.map { glyph =>
      glyphGuesser.guess(mat, glyph, beamWidth).head
    }
    val guess = Guess(predictionPerGlyph)
    val glyphsWithContent = word.glyphs.zip(guess.guesses).map { case (glyph, prediction) =>
      glyph.copy(content = prediction.outcome, confidence = prediction.confidence)
    }
    if (log.isDebugEnabled) {
      log.debug(f"Guessed word: ${guess.word} with confidence ${guess.score}%.3f")
    }
    word.copy(glyphs = glyphsWithContent, content = guess.word, confidence = guess.score)
  }

  private case class Guess(guesses: Seq[Prediction]) extends Ordered[Guess] {
    val score: Double = Math.exp(guesses.foldLeft(0.0){ case (score, prediction) => score + Math.log(prediction.confidence) } / guesses.size)
    override def compare(that: Guess): Int = {
      this.score.compare(that.score)
    }

    private lazy val unsimplifiedWord = guesses.map(_.outcome).mkString

    lazy val word: String = lexicon.textSimplifier.map(_.simplify(unsimplifiedWord)).getOrElse(unsimplifiedWord)
  }

  private case class GuessWithScore(guess: Guess, score: Double)

  private def guessWithBeam(mat: Mat): PartialFunction[AltoElement, AltoElement] = {
    case word: Word =>
      val guessedWord = guessWordWithBeam(mat, word)
      val otherAlphabetWord = guessWithOtherAlphabets(mat, guessedWord).getOrElse(guessedWord)
      otherAlphabetWord
   }

  private def guessWordWithBeam(mat: Mat, word: Word): Word = {
    //TODO: currently frequency calculation isn't used for hyphenated words at end of line
    val beam = this.getBeam(mat, word)
    val end = Math.min(beamWidth, beam.size)
    val topGuesses = (1 to end).map(_ => beam.dequeue())
      .map { guess =>
        GuessWithScore(guess, guess.score)
      }.map { guessWithScore =>
      val frequency = lexicon.getFrequency(guessWithScore.guess.word, preSimplified = true)
      if (frequency > 0) {
        guessWithScore
      } else if (frequency < 0) {
        // lower the score of impossible words
        guessWithScore.copy(score = guessWithScore.score * 0.01)
      } else {
        // lower the score of unknown words
        guessWithScore.copy(score = guessWithScore.score * unknownWordFactor)
      }
    }.sortBy(0 - _.score) // And sort by the new highest scoring word
    if (log.isDebugEnabled) {
      log.debug("Guesses for next word")
      topGuesses.zipWithIndex.foreach { case (topGuess, i) =>
        log.debug(f"Guess $i: ${topGuess.guess.word}. Initial score: ${topGuess.guess.score}%.3f. Score: ${topGuess.score}%.3f")
      }
    }
    val topGuess = topGuesses.head
    val glyphsWithContent = word.glyphs.zip(topGuess.guess.guesses).map { case (glyph, guess) =>
      glyph.copy(content = guess.outcome, confidence = guess.confidence)
    }
    word.copy(content = topGuess.guess.word, glyphs = glyphsWithContent, confidence = topGuess.score)
  }

  private def getBeam(mat: Mat, word: Word): mutable.PriorityQueue[Guess] = {
    val predictionsPerGlyph = word.glyphs.map { glyph =>
      glyphGuesser.guess(mat, glyph, beamWidth)
    }
    val beam = predictionsPerGlyph.foldLeft(mutable.PriorityQueue(Guess(Seq.empty))) { case (beam, predictions) =>
      val end = Math.min(beamWidth, beam.size)
      val topChoices = (1 to end).map(_ => beam.dequeue())
      val newChoices = topChoices.flatMap { topChoice =>
        predictions.map { prediction =>
          topChoice.copy(guesses = topChoice.guesses :+ prediction)
        }
      }
      val newBeam = mutable.PriorityQueue.empty[Guess]
      newChoices.foreach(newBeam.enqueue(_))
      newBeam
    }
    beam
  }

  private def guessWithOtherAlphabets(mat: Mat, word: Word): Option[Word] = {
    //TODO: set entire line or block to language if all non-number non-punctuation elements are another language
    val otherAlphabetGuesser = glyphGuessersForOtherAlphabets.glyphGuessers.find{
      case GlyphGuesserForAnotherAlphabet(language, regex, _) =>
        val matched = regex.matches(word.content)
        if (matched) {
          if (log.isDebugEnabled) { log.debug(f"Using glyph guesser $language for word ${word.content}") }
        }
        matched
    }

    otherAlphabetGuesser.map{
      case GlyphGuesserForAnotherAlphabet(language, _, otherAlphabetGuesser) =>
        val leftToRight = StringUtils.isLeftToRight(language)
        val newGlyphs = if (word.isLeftToRight != leftToRight) {
          word.glyphs.sorted(WithRectangle.HorizontalOrdering(leftToRight))
        } else {
          word.glyphs
        }
        val updatedWord = word.copy(language = Some(language), glyphs =newGlyphs)
        guessWordWithoutBeam(mat, updatedWord, otherAlphabetGuesser)
    }
  }

  private def changeTextLineLanguageIfRequired: PartialFunction[AltoElement, AltoElement] = {
    case textLine: TextLine =>
      val languages = textLine.words.map(_.language).groupBy(identity).view.mapValues(_.size).toSeq.sortBy(0 - _._2)

      if (languages.nonEmpty && languages.head._1.isDefined) {
        val topLanguage = languages.head._1.get
        if (log.isDebugEnabled) {
          log.debug(f"Changing language to ${topLanguage} for textLine ${textLine.baseLine}. Word languages: ${textLine.words.map(_.language).mkString(", ")}")
        }
        textLine.copy(
          language = Some(topLanguage),
          wordsAndSpaces = textLine.wordsAndSpaces.map {
            case word: Word => word.withDefaultLanguage(topLanguage)
            case other => other
          }.sorted(WithRectangle.HorizontalOrdering(StringUtils.isLeftToRight(topLanguage)))
        )
      } else {
        textLine
      }
  }

  private def changeTextBlockLanguageIfRequired: PartialFunction[AltoElement, AltoElement] = {
    case textBlock: TextBlock =>
      val languages = textBlock.textLines.map(_.language).groupBy(identity).view.mapValues(_.size).toSeq.sortBy(0 - _._2)

      if (languages.nonEmpty && languages.head._1.isDefined) {
        val topLanguage = languages.head._1.get
        if (log.isDebugEnabled) {
          log.debug(f"Changing language to ${topLanguage} for textBlock ${textBlock.rectangle.coordinates}. TextLine languages: ${textBlock.textLines.map(_.language).mkString(", ")}")
        }
        textBlock.copy(
          language = Some(topLanguage),
          textLines = textBlock.textLines.map(_.withDefaultLanguage(topLanguage))
        )
      } else {
        textBlock
      }
  }

  private def changePageLanguageIfRequired: PartialFunction[AltoElement, AltoElement] = {
    case page: Page =>
      val languages = page.textBlocks.map(_.language).groupBy(identity).view.mapValues(_.size).toSeq.sortBy(0 - _._2).map(_._1)

      if (languages.nonEmpty && languages.head.isDefined) {
        val topLanguage = languages.head.get
        if (log.isDebugEnabled) {
          log.debug(f"Changing language to ${topLanguage} for page ${page.physicalPageNumber}")
          log.debug(f"Textblock languages: ${page.textBlocks.map(_.language).mkString(", ")}")
        }
        page.withLanguage(topLanguage)
      } else {
        page
      }
  }
}
