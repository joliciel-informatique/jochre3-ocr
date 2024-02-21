package com.joliciel.jochre.ocr.core.lexicon

import com.joliciel.jochre.ocr.core.corpus.TextSimplifier

trait Lexicon {
  def textSimplifier: Option[TextSimplifier]

  /**
   * If [[hasFrequency]], this will return an actual corpus-based frequency, 0 for an unknown word, and -1 for an impossible word.
   *
   * If not, this will return 1 for a known word, 0 for an unknown word, and -1 for an impossible word.
   *
   * The "word" here is actually a string without whitespace, but could contain punctuation as well as one or more words (e.g. separated by a dash).
   *
   *
   * @param word
   * @param preSimplified
   * @return
   */
  def getFrequency(word: String, preSimplified: Boolean = false): Int

  /**
   * Is an individual word impossible? (this should exclude punctuation)
   * @param word
   * @return
   */
  def isImpossible(word: String): Boolean

  /**
   * Does this lexicon have true corpus-based frequency measurements.
   */
  def hasFrequency: Boolean = false
}

object Lexicon {
  val default: Lexicon = new Lexicon {
    override def textSimplifier: Option[TextSimplifier] = None

    override def getFrequency(word: String, preSimplified: Boolean): Int = 1

    override def isImpossible(word: String): Boolean = false
  }
}
