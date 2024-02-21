package com.joliciel.jochre.ocr.core.text

import com.joliciel.jochre.ocr.core.JochreCLI
import com.joliciel.jochre.ocr.core.learning.{GlyphGuesser, Prediction}
import com.joliciel.jochre.ocr.core.lexicon.Lexicon
import com.joliciel.jochre.ocr.core.model.{AltoElement, Page, Word}
import com.joliciel.jochre.ocr.core.utils.OutputLocation
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import zio.{Task, ZIO, ZIOAppArgs, ZLayer}

import scala.collection.mutable

object FullSegmentationGuesserService {
  val live: ZLayer[GlyphGuesser with Lexicon with FullSegmentationGuesserConfig, Nothing, TextGuesserService] = ZLayer.fromFunction{
    FullSegmentationGuesserServiceImpl(_, _, _)
  }
}

private[text] case class FullSegmentationGuesserServiceImpl(glyphGuesser: GlyphGuesser, lexicon: Lexicon, config: FullSegmentationGuesserConfig) extends TextGuesserService {
  def getTextGuesser(): Task[FullSegmentationGuesser] = {
    ZIO.attempt(FullSegmentationGuesser(glyphGuesser, lexicon, config))
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
    if (beamWidth <= 1) {
      page.transform(guessWithoutBeam(mat))
    } else {
      page.transform(guessWithBeam(mat))
    }
  }

  private def guessWithoutBeam(mat: Mat): PartialFunction[AltoElement, AltoElement] = {
    case word: Word =>
      val predictionPerGlyph = word.glyphs.map { glyph =>
        glyphGuesser.guess(mat, glyph, beamWidth).head
      }
      val guess = Guess(predictionPerGlyph)
      val glyphsWithContent = word.glyphs.zip(guess.guesses).map{ case (glyph, prediction) =>
        glyph.copy(rectangle = glyph.rectangle.copy(label = prediction.outcome), confidence = prediction.confidence)
      }
      word.copy(glyphs = glyphsWithContent, rectangle = word.rectangle.copy(label = guess.word), confidence = guess.score)
  }

  private case class Guess(guesses: Seq[Prediction]) extends Ordered[Guess] {
    val score = Math.exp(guesses.foldLeft(0.0){ case (score, prediction) => score + Math.log(prediction.confidence) } / guesses.size)
    override def compare(that: Guess): Int = {
      this.score.compare(that.score)
    }

    private lazy val unsimplifiedWord = guesses.map(_.outcome).mkString

    lazy val word = lexicon.textSimplifier.map(_.simplify(unsimplifiedWord)).getOrElse(unsimplifiedWord)
  }

  private case class GuessWithScore(guess: Guess, score: Double)
  private def guessWithBeam(mat: Mat): PartialFunction[AltoElement, AltoElement] = {
    case word: Word =>
      //TODO: currently frequency calculation isn't used for hyphenated words at end of line
      val beam = this.getBeam(mat, word)
      val end = Math.min(beamWidth, beam.size)
      val topGuesses = (1 to end).map(_ => beam.dequeue())
        .map{ guess =>
          GuessWithScore(guess, guess.score)
        }.map { guessWithScore =>
          val frequency = lexicon.getFrequency(guessWithScore.guess.word, preSimplified = true)
          if (frequency>0) {
            guessWithScore
          } else if (frequency<0) {
            // lower the score of impossible words
            guessWithScore.copy(score = guessWithScore.score * 0.01)
          } else {
            // lower the score of unknown words
            guessWithScore.copy(score = guessWithScore.score * unknownWordFactor)
          }
      }.sortBy(0 - _.score) // And sort by the new highest scoring word
      if (log.isDebugEnabled) {
        log.debug("Guesses for next word")
        topGuesses.zipWithIndex.foreach{ case (topGuess, i) =>
          log.debug(f"Guess $i: ${topGuess.guess.word}. Initial score: ${topGuess.guess.score}%.3f. Score: ${topGuess.score}%.3f")
        }
      }
      val topGuess = topGuesses.head
      val glyphsWithContent = word.glyphs.zip(topGuess.guess.guesses).map{ case (glyph, guess) =>
        glyph.copy(rectangle = glyph.rectangle.copy(label = guess.outcome), confidence = guess.confidence)
      }
      word.copy(glyphs = glyphsWithContent, rectangle = word.rectangle.copy(label = topGuess.guess.word), confidence = topGuess.score)
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
}
