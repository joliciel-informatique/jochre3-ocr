package com.joliciel.jochre.ocr.core.text

import com.joliciel.jochre.ocr.core.learning.{GlyphGuesser, Prediction}
import com.joliciel.jochre.ocr.core.model.{AltoElement, Glyph, Page, Word}
import com.joliciel.jochre.ocr.core.utils.OutputLocation
import org.bytedeco.opencv.opencv_core.Mat
import zio.{Task, ZIO, ZLayer}

import scala.collection.mutable

object FullSegmentationGuesserService {
  val live: ZLayer[GlyphGuesser, Nothing, TextGuesserService] = ZLayer.fromFunction(FullSegmentationGuesserImpl(_))
}

private[text] case class FullSegmentationGuesserImpl(glyphGuesser: GlyphGuesser) extends TextGuesserService {
  def getTextGuesser(): Task[FullSegmentationGuesser] = {
    ZIO.attempt(FullSegmentationGuesser(glyphGuesser))
  }
}

case class FullSegmentationGuesser(glyphGuesser: GlyphGuesser, beamWidth: Int = 5) extends TextGuesser {
  /**
   * Given an image and a pre-segmented [[Page]] structure, attempt to guess the text within the page
   * by assigning content to the resulting page.
   */
  override def guess(page: Page, mat: Mat, fileName: String, debugLocation: Option[OutputLocation]): Task[Page] = ZIO.attempt{
    page.transform(guessWithoutBeam(mat))
  }

  private def guessWithoutBeam(mat: Mat): PartialFunction[AltoElement, AltoElement] = {
    case word: Word =>
      val glyphsWithContent = word.glyphs.map { glyph =>
        val predictions = glyphGuesser.guess(mat, glyph)
        glyph.copy(rectangle = glyph.rectangle.copy(label = predictions(0).outcome))
      }
      val content = glyphsWithContent.map(_.content).mkString("")
      word.copy(glyphs = glyphsWithContent, rectangle = word.rectangle.copy(label = content))
  }

  private case class Guess(guesses: Seq[Prediction]) extends Ordered[Guess] {
    val score = Math.exp(guesses.foldLeft(0.0){ case (score, prediction) => score + Math.log(prediction.confidence) } / guesses.size)
    override def compare(that: Guess): Int = {
      this.score.compare(that.score)
    }
  }

  private def guessWithBeam(mat: Mat): PartialFunction[AltoElement, AltoElement] = {
    case word: Word =>
      val beam = this.getBeam(mat, word)
      val topGuess = beam.dequeue().guesses
      val glyphsWithContent = word.glyphs.zip(topGuess).map{ case (glyph, guess) =>
        glyph.copy(rectangle = glyph.rectangle.copy(label = guess.outcome))
      }
      word.copy(glyphs = glyphsWithContent)
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
