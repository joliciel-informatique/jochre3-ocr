package com.joliciel.jochre.ocr.core.text

import com.joliciel.jochre.ocr.core.JochreCLI
import com.joliciel.jochre.ocr.core.graphics.WithRectangle
import com.joliciel.jochre.ocr.core.learning.{
  GlyphGuesser,
  GlyphGuesserForAnotherAlphabet,
  GlyphGuessersForOtherAlphabets,
  Prediction
}
import com.joliciel.jochre.ocr.core.lexicon.Lexicon
import com.joliciel.jochre.ocr.core.model.{AltoElement, Page, SubsType, TextBlock, TextLine, Word, WordOrSpace}
import com.joliciel.jochre.ocr.core.utils.{OutputLocation, StringUtils}
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import zio.{Task, ZIO, ZIOAppArgs, ZLayer}

import java.time.Instant
import scala.collection.mutable
import scala.util.matching.Regex

object FullSegmentationGuesserService {
  val live: ZLayer[
    GlyphGuesser with GlyphGuessersForOtherAlphabets with Lexicon with FullSegmentationGuesserConfig,
    Nothing,
    TextGuesserService
  ] = ZLayer.fromFunction {
    FullSegmentationGuesserServiceImpl(_, _, _, _)
  }
}

private[text] case class FullSegmentationGuesserServiceImpl(
    glyphGuesser: GlyphGuesser,
    glyphGuessersForOtherAlphabets: GlyphGuessersForOtherAlphabets,
    lexicon: Lexicon,
    config: FullSegmentationGuesserConfig
) extends TextGuesserService {
  def getTextGuesser: Task[FullSegmentationGuesser] = {
    ZIO.attempt(
      FullSegmentationGuesser(
        glyphGuesser,
        glyphGuessersForOtherAlphabets,
        lexicon,
        config
      )
    )
  }
}

case class FullSegmentationGuesserConfig(
    hyphenRegex: Regex,
    beamWidth: Int = 5,
    unknownWordFactor: Double = 0.75
)

object FullSegmentationGuesserConfig {
  def fromConfig: FullSegmentationGuesserConfig = {
    val config = ConfigFactory.load().getConfig("jochre.ocr.text-guesser")

    val unknownWordFactor = config.getDouble("unknown-word-factor")
    val beamWidth = config.getInt("beam-width")
    val hyphenRegex = config.getString("hyphen-regex").r
    FullSegmentationGuesserConfig(hyphenRegex, beamWidth, unknownWordFactor)
  }

  val configLayer: ZLayer[Any, Throwable, FullSegmentationGuesserConfig] =
    ZLayer.fromZIO {
      ZIO.attempt {
        fromConfig
      }
    }

  val appArgsLayer: ZLayer[ZIOAppArgs, Throwable, FullSegmentationGuesserConfig] =
    ZLayer.fromZIO {
      for {
        args <- ZIOAppArgs.getArgs
        jochreCLI <- ZIO.attempt { new JochreCLI(args) }
      } yield {
        fromConfig.copy(
          beamWidth = jochreCLI.beamWidth.toOption.getOrElse(fromConfig.beamWidth),
          unknownWordFactor = jochreCLI.unknownWordFactor.toOption.getOrElse(
            fromConfig.unknownWordFactor
          )
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

  private sealed trait HyphenationStatus

  private object NonHyphenated extends HyphenationStatus
  private case class HyphenatedWithHyphen(hyphenatedWord: String) extends HyphenationStatus
  private case class HyphenatedWithoutHyphen(hyphenatedWord: String) extends HyphenationStatus
  private case class HyphenatedPart2WithHyphen(hyphenatedWord: String) extends HyphenationStatus
  private case class HyphenatedPart2WithoutHyphen(hyphenatedWord: String) extends HyphenationStatus

  /** Given an image and a pre-segmented [[Page]] structure, attempt to guess the text within the page by assigning
    * content to the resulting page.
    */
  override def guess(
      page: Page,
      mat: Mat,
      fileName: String,
      debugLocation: Option[OutputLocation]
  ): Task[Page] = ZIO.attempt {
    val startTime = Instant.now
    val withGuesses = if (beamWidth <= 1) {
      page.transform(guessWithoutBeam(mat))
    } else {
      page.transform(guessTextBlockWithBeam(mat))
    }
    val withLanguageCorrected = withGuesses
      .transform(changeTextLineLanguageIfRequired)
      .transform(changeTextBlockLanguageIfRequired)
      .transform(changePageLanguageIfRequired)

    val duration = (Instant.now.toEpochMilli - startTime.toEpochMilli).toDouble / 1000.0
    log.info(f"Finished guessing content for $fileName in $duration%.2f seconds")

    withLanguageCorrected
  }

  private def guessWithoutBeam(
      mat: Mat
  ): PartialFunction[AltoElement, AltoElement] = { case word: Word =>
    val guessedWord = guessWordWithoutBeam(mat, word, glyphGuesser)
    val otherAlphabetWord =
      guessWithOtherAlphabets(mat, guessedWord).getOrElse(guessedWord)
    otherAlphabetWord
  }

  private def guessWordWithoutBeam(
      mat: Mat,
      word: Word,
      glyphGuesser: GlyphGuesser
  ): Word = {
    val predictionPerGlyph = word.glyphs.map { glyph =>
      glyphGuesser.guess(mat, glyph, beamWidth).head
    }
    val guess = Guess(predictionPerGlyph)
    val glyphsWithContent =
      word.glyphs.zip(guess.glyphPredictions).map { case (glyph, prediction) =>
        glyph.copy(
          content = prediction.outcome,
          confidence = prediction.confidence
        )
      }
    if (log.isDebugEnabled) {
      log.debug(
        f"Guessed word: ${guess.word} with confidence ${guess.score}%.3f"
      )
    }
    word.copy(
      glyphs = glyphsWithContent,
      content = guess.word,
      confidence = guess.score
    )
  }

  private case class Guess(glyphPredictions: Seq[Prediction]) extends Ordered[Guess] {
    val score: Double = Math.exp(glyphPredictions.foldLeft(0.0) { case (score, prediction) =>
      score + Math.log(prediction.confidence)
    } / glyphPredictions.size)
    override def compare(that: Guess): Int = {
      this.score.compare(that.score)
    }

    private lazy val unsimplifiedWord = glyphPredictions.map(_.outcome).mkString

    lazy val word: String = lexicon.textSimplifier
      .map(_.simplify(unsimplifiedWord))
      .getOrElse(unsimplifiedWord)
  }

  private case class GuessWithScore(guess: Guess, score: Double)

  private case class WordWithBeam(
      word: WordOrSpace,
      beam: Seq[GuessWithScore]
  ) {
    def selectBestGuess(mat: Mat): WordOrSpace = word match {
      case word: Word =>
        val rescored = rescoreBeam(beam)
        if (log.isDebugEnabled) {
          log.debug("Guesses for next word")
          beam.zipWithIndex.foreach { case (topGuess, i) =>
            log.debug(
              f"Guess $i: ${topGuess.guess.word}. Initial score: ${topGuess.guess.score}%.3f. Score: ${topGuess.score}%.3f"
            )
          }
        }
        val guessedWord = guessToWord(mat, word, rescored.head)
        val otherAlphabetWord =
          guessWithOtherAlphabets(mat, guessedWord).getOrElse(guessedWord)
        otherAlphabetWord
      case other =>
        other
    }
  }

  private def guessTextBlockWithBeam(
      mat: Mat
  ): PartialFunction[AltoElement, AltoElement] = { case textBlock: TextBlock =>
    val textLinesWithBeams = textBlock.textLines.map { textLine =>
      val guesses = textLine.wordsAndSpaces.map {
        case word: Word => WordWithBeam(word, getBeam(mat, word))
        case other      => WordWithBeam(other, Seq.empty)
      }
      textLine -> guesses
    }

    val newTextLines = if (textLinesWithBeams.size <= 1) {
      textLinesWithBeams.map { case (textLine, guesses) =>
        val newWordsAndSpaces = guesses.map(_.selectBestGuess(mat))
        textLine.copy(wordsAndSpaces = newWordsAndSpaces)
      }
    } else {
      val textLinePairs =
        textLinesWithBeams.zip(textLinesWithBeams.drop(1).map(Some(_)) :+ None)
      val (newTextLines, _) =
        textLinePairs.foldLeft(Seq.empty[TextLine] -> Option.empty[Word]) {
          case ((newTextLines, firstWord), ((textLine, guesses), None)) =>
            // Last line, no hyphenation possible
            val newWordsAndSpaces = guesses match {
              case Nil => Seq.empty
              case head :: tail =>
                firstWord.getOrElse(head.selectBestGuess(mat)) +: tail.map(
                  _.selectBestGuess(mat)
                )
            }

            val newTextLine = textLine.copy(wordsAndSpaces = newWordsAndSpaces)
            (newTextLines :+ newTextLine) -> None
          case (
                (newTextLines, firstWord),
                ((textLine, guesses), Some((_, nextGuesses)))
              ) =>
            val lastWordWithHyphen = guesses.lastOption.find(lastWord =>
              lastWord.beam.exists { guessWithScore =>
                // At least one guess ends with a hyphen
                guessWithScore.guess.glyphPredictions.nonEmpty && config.hyphenRegex
                  .matches(guessWithScore.guess.glyphPredictions.last.outcome)
              }
            )
            val firstWordNextLine = nextGuesses.headOption

            (lastWordWithHyphen, firstWordNextLine) match {
              case (
                    Some(WordWithBeam(word1: Word, beam1)),
                    Some(WordWithBeam(word2: Word, beam2))
                  ) =>
                val scoredPairs = beam1
                  .flatMap { guessWithScore =>
                    val endsWithHyphen =
                      guessWithScore.guess.glyphPredictions.nonEmpty && config.hyphenRegex
                        .matches(
                          guessWithScore.guess.glyphPredictions.last.outcome
                        )
                    if (endsWithHyphen) {
                      // Current guess ends with a hyphen
                      beam2.map { nextWordGuessWithScore =>
                        val combinedGuessWithHyphen = Guess(
                          guessWithScore.guess.glyphPredictions ++ nextWordGuessWithScore.guess.glyphPredictions
                        )
                        val combinedGuessWithHyphenFrequency =
                          lexicon.getFrequency(
                            combinedGuessWithHyphen.word,
                            preSimplified = true
                          )
                        val combinedGuessWithoutHyphen = Guess(
                          guessWithScore.guess.glyphPredictions.init ++ nextWordGuessWithScore.guess.glyphPredictions
                        )
                        val combinedGuessWithoutHyphenFrequency =
                          lexicon.getFrequency(
                            combinedGuessWithoutHyphen.word,
                            preSimplified = true
                          )
                        val combinedGuessMaxFrequency = Math.max(
                          combinedGuessWithHyphenFrequency,
                          combinedGuessWithoutHyphenFrequency
                        )

                        val hyphenationStatus =
                          if (combinedGuessWithHyphenFrequency > combinedGuessWithoutHyphenFrequency) {
                            HyphenatedWithHyphen(combinedGuessWithHyphen.word)
                          } else {
                            HyphenatedWithoutHyphen(combinedGuessWithoutHyphen.word)
                          }

                        val initialCombinedScore = Math.sqrt(
                          guessWithScore.score * nextWordGuessWithScore.score
                        )
                        val factor = if (combinedGuessMaxFrequency > 0) {
                          1
                        } else if (combinedGuessMaxFrequency < 0) {
                          // lower the score of impossible words
                          0.01
                        } else {
                          // lower the score of unknown words
                          unknownWordFactor
                        }

                        val rescoredGuess1 = guessWithScore
                          .copy(score = guessWithScore.score * factor)
                        val rescoredGuess2 = nextWordGuessWithScore
                          .copy(score = nextWordGuessWithScore.score * factor)
                        val combinedScore = initialCombinedScore * factor
                        (rescoredGuess1, rescoredGuess2, combinedScore, hyphenationStatus)
                      }
                    } else {
                      val rescoredGuess1 = rescoreGuess(guessWithScore)
                      val rescoredBeam2 = rescoreBeam(beam2)
                      rescoredBeam2.map { rescoredGuess2 =>
                        val combinedScore =
                          Math.sqrt(rescoredGuess2.score * rescoredGuess2.score)
                        (rescoredGuess1, rescoredGuess2, combinedScore, NonHyphenated)
                      }
                    }
                  }
                  .sortBy(0 - _._3)

                if (log.isDebugEnabled) {
                  log.debug("Guesses for hyphenated pair")
                  scoredPairs.zipWithIndex.foreach {
                    case ((guessWithScore1, guessWithScore2, score, hyphenationStatus), i) =>
                      log.debug(
                        f"Guess $i: Word 1: ${guessWithScore1.guess.word}. Word 2: ${guessWithScore2.guess.word}. Score 1: ${guessWithScore1.guess.score}%.3f. Score 2: ${guessWithScore2.guess.score}%.3f. Combined score: $score%.3f"
                      )
                  }
                }

                val (bestGuessWord1, bestGuessWord2, _, hyphenationStatus) = scoredPairs.head

                val word2HyphenationStatus = hyphenationStatus match {
                  case HyphenatedWithHyphen(word)    => HyphenatedPart2WithHyphen(word)
                  case HyphenatedWithoutHyphen(word) => HyphenatedPart2WithoutHyphen(word)
                  case _                             => NonHyphenated
                }
                val newWord1 = guessToWord(mat, word1, bestGuessWord1, hyphenationStatus)
                val newWord2 = guessToWord(mat, word2, bestGuessWord2, word2HyphenationStatus)

                val newWordsAndSpaces = (guesses.init match {
                  case Nil => Seq.empty
                  case head :: tail =>
                    firstWord.getOrElse(head.selectBestGuess(mat)) +: tail.map(
                      _.selectBestGuess(mat)
                    )
                }) :+ newWord1

                val newTextLine =
                  textLine.copy(wordsAndSpaces = newWordsAndSpaces)
                (newTextLines :+ newTextLine) -> Some(newWord2)
              case _ =>
                val newWordsAndSpaces = guesses match {
                  case Nil => Seq.empty
                  case head :: tail =>
                    firstWord.getOrElse(head.selectBestGuess(mat)) +: tail.map(
                      _.selectBestGuess(mat)
                    )
                }
                val newTextLine =
                  textLine.copy(wordsAndSpaces = newWordsAndSpaces)
                (newTextLines :+ newTextLine) -> None
            }
        }
      newTextLines
    }
    textBlock.copy(textLines = newTextLines)
  }

  private def guessToWord(
      mat: Mat,
      word: Word,
      topGuess: GuessWithScore,
      hyphenationStatus: HyphenationStatus = NonHyphenated
  ): Word = {
    val glyphsWithContent =
      word.glyphs.zip(topGuess.guess.glyphPredictions).map { case (glyph, guess) =>
        glyph.copy(content = guess.outcome, confidence = guess.confidence)
      }

    val guessedWord = word.copy(
      content = topGuess.guess.word,
      glyphs = glyphsWithContent,
      confidence = topGuess.score,
      subsType = hyphenationStatus match {
        case NonHyphenated                   => None
        case HyphenatedWithHyphen(_)         => Some(SubsType.HypPart1)
        case HyphenatedWithoutHyphen(_)      => Some(SubsType.HypPart1)
        case HyphenatedPart2WithHyphen(_)    => Some(SubsType.HypPart2)
        case HyphenatedPart2WithoutHyphen(_) => Some(SubsType.HypPart2)
      },
      subsContent = hyphenationStatus match {
        case NonHyphenated                      => None
        case HyphenatedWithHyphen(word)         => Some(word)
        case HyphenatedWithoutHyphen(word)      => Some(word)
        case HyphenatedPart2WithHyphen(word)    => Some(word)
        case HyphenatedPart2WithoutHyphen(word) => Some(word)
      }
    )
    val otherAlphabetWord =
      guessWithOtherAlphabets(mat, guessedWord).getOrElse(guessedWord)
    otherAlphabetWord
  }

  private def rescoreBeam(beam: Seq[GuessWithScore]): Seq[GuessWithScore] = {
    // Rescore the guesses and sort by the new highest scoring word
    val topGuesses = beam.map(rescoreGuess).sortBy(0 - _.score)
    topGuesses
  }

  private def rescoreGuess(guessWithScore: GuessWithScore): GuessWithScore = {
    val frequency =
      lexicon.getFrequency(guessWithScore.guess.word, preSimplified = true)
    if (frequency > 0) {
      guessWithScore
    } else if (frequency < 0) {
      // lower the score of impossible words
      guessWithScore.copy(score = guessWithScore.score * 0.01)
    } else {
      // lower the score of unknown words
      guessWithScore.copy(score = guessWithScore.score * unknownWordFactor)
    }
  }

  private def getBeam(mat: Mat, word: Word): Seq[GuessWithScore] = {
    val predictionsPerGlyph = word.glyphs.map { glyph =>
      glyphGuesser.guess(mat, glyph, beamWidth)
    }
    val beam =
      predictionsPerGlyph.foldLeft(mutable.PriorityQueue(Guess(Seq.empty))) { case (beam, predictions) =>
        val end = Math.min(beamWidth, beam.size)
        val topChoices = (1 to end).map(_ => beam.dequeue())
        val newChoices = topChoices.flatMap { topChoice =>
          predictions.map { prediction =>
            topChoice.copy(glyphPredictions = topChoice.glyphPredictions :+ prediction)
          }
        }
        val newBeam = mutable.PriorityQueue.empty[Guess]
        newChoices.foreach(newBeam.enqueue(_))
        newBeam
      }
    val end = Math.min(beamWidth, beam.size)
    val topGuesses = (1 to end)
      .map(_ => beam.dequeue())
      .map { guess =>
        GuessWithScore(guess, guess.score)
      }
    topGuesses
  }

  private def guessWithOtherAlphabets(mat: Mat, word: Word): Option[Word] = {
    // TODO: set entire line or block to language if all non-number non-punctuation elements are another language
    val otherAlphabetGuesser =
      glyphGuessersForOtherAlphabets.glyphGuessers.find { case GlyphGuesserForAnotherAlphabet(language, regex, _) =>
        val matched = regex.matches(word.content)
        if (matched) {
          if (log.isDebugEnabled) {
            log.debug(
              f"Using glyph guesser $language for word ${word.content}"
            )
          }
        }
        matched
      }

    otherAlphabetGuesser.map { case GlyphGuesserForAnotherAlphabet(language, _, otherAlphabetGuesser) =>
      val leftToRight = StringUtils.isLeftToRight(language)
      val newGlyphs = if (word.isLeftToRight != leftToRight) {
        word.glyphs.sorted(WithRectangle.HorizontalOrdering(leftToRight))
      } else {
        word.glyphs
      }
      val updatedWord =
        word.copy(language = Some(language), glyphs = newGlyphs)
      guessWordWithoutBeam(mat, updatedWord, otherAlphabetGuesser)
    }
  }

  private def changeTextLineLanguageIfRequired: PartialFunction[AltoElement, AltoElement] = { case textLine: TextLine =>
    val languages = textLine.words
      .map(_.language)
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .toSeq
      .sortBy(0 - _._2)

    if (languages.nonEmpty && languages.head._1.isDefined) {
      val topLanguage = languages.head._1.get
      if (log.isDebugEnabled) {
        log.debug(
          f"Changing language to $topLanguage for textLine ${textLine.baseLine}. Word languages: ${textLine.words.map(_.language).mkString(", ")}"
        )
      }
      textLine.copy(
        language = Some(topLanguage),
        wordsAndSpaces = textLine.wordsAndSpaces
          .map {
            case word: Word => word.withDefaultLanguage(topLanguage)
            case other      => other
          }
          .sorted(
            WithRectangle.HorizontalOrdering(
              StringUtils.isLeftToRight(topLanguage)
            )
          )
      )
    } else {
      textLine
    }
  }

  private def changeTextBlockLanguageIfRequired: PartialFunction[AltoElement, AltoElement] = {
    case textBlock: TextBlock =>
      val languages = textBlock.textLines
        .map(_.language)
        .groupBy(identity)
        .view
        .mapValues(_.size)
        .toSeq
        .sortBy(0 - _._2)

      if (languages.nonEmpty && languages.head._1.isDefined) {
        val topLanguage = languages.head._1.get
        if (log.isDebugEnabled) {
          log.debug(
            f"Changing language to $topLanguage for textBlock ${textBlock.rectangle.coordinates}. TextLine languages: ${textBlock.textLines.map(_.language).mkString(", ")}"
          )
        }
        textBlock.copy(
          language = Some(topLanguage),
          textLines = textBlock.textLines.map(_.withDefaultLanguage(topLanguage))
        )
      } else {
        textBlock
      }
  }

  private def changePageLanguageIfRequired: PartialFunction[AltoElement, AltoElement] = { case page: Page =>
    val languages = page.textBlocks
      .map(_.language)
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .toSeq
      .sortBy(0 - _._2)
      .map(_._1)

    if (languages.nonEmpty && languages.head.isDefined) {
      val topLanguage = languages.head.get
      if (log.isDebugEnabled) {
        log.debug(
          f"Changing language to $topLanguage for page ${page.physicalPageNumber}"
        )
        log.debug(
          f"Textblock languages: ${page.textBlocks.map(_.language).mkString(", ")}"
        )
      }
      page.withLanguage(topLanguage)
    } else {
      page
    }
  }
}
