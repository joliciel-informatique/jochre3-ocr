package com.joliciel.jochre.ocr.core.text

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.joliciel.jochre.ocr.core.lexicon.Lexicon
import org.bytedeco.opencv.opencv_core.Mat
import com.joliciel.jochre.ocr.core.model.Word
import com.joliciel.jochre.ocr.core.lexicon.TextFileLexicon
import com.joliciel.jochre.ocr.core.learning.Prediction

class FullSegmenationGuesserTest extends AnyFlatSpec with Matchers {
  private class YiddishLexicon(entries: Set[String]) extends TextFileLexicon(entries, None) {
    private val impossibleWordRegex =
      raw"""(?U)(\w*[ןםךץף]\w+)""".r

    override def isImpossible(word: String): Boolean =
      impossibleWordRegex.matches(word)
  }

  private object MockFullSegmentationGuesser extends FullSegmentationGuesserHelper {

    lazy val lexicon: Lexicon = new YiddishLexicon(Set("ארץ", "ישראל", "ארץ־ישראל", "געװאָרן"))

    lazy val config: FullSegmentationGuesserConfig = FullSegmentationGuesserConfig("־".r, 5, 0.5)

    override def guessWithOtherAlphabets(mat: Mat, word: Word): Option[Word] = None

  }

  private val guesser = MockFullSegmentationGuesser

  "removeExternalPunct" should "remove punctuation before and after word" in {
    guesser.removeExternalPunct("„רבונו־של־עולם“,") shouldEqual "רבונו־של־עולם"
  }

  it should "leave a string without punctuation as is" in {
    guesser.removeExternalPunct("רבונו־של־עולם") shouldEqual "רבונו־של־עולם"
  }

  private def stringToPredictions(string: String): Seq[Prediction] = {
    string.map(c => Prediction(f"$c", 0.5)).toSeq
  }

  "getHyphenationStatus" should "guess hyphenated word when it's in the lexicon" in {
    val lastWordWithHyphenGuess =
      guesser.GuessWithScore(guesser.Guess(stringToPredictions("ארץ־")), 0.5)
    val firstWordNextLineGuess =
      guesser.GuessWithScore(guesser.Guess(stringToPredictions("ישראל")), 0.5)
    val scoredPair = guesser.getHyphenationStatus(lastWordWithHyphenGuess, firstWordNextLineGuess)

    scoredPair.hyphenationStatus shouldEqual guesser.HyphenatedWithHyphen("ארץ־ישראל")
  }

  it should "guess unhyphanted word when it's in the lexicon" in {
    val lastWordWithHyphenGuess =
      guesser.GuessWithScore(guesser.Guess(stringToPredictions("גע־")), 0.5)
    val firstWordNextLineGuess =
      guesser.GuessWithScore(guesser.Guess(stringToPredictions("װאָרן")), 0.5)
    val scoredPair = guesser.getHyphenationStatus(lastWordWithHyphenGuess, firstWordNextLineGuess)

    scoredPair.hyphenationStatus shouldEqual guesser.HyphenatedWithoutHyphen("געװאָרן")
  }

  it should "guess hyphenated word if unhyphenated word is impossible" in {
    val lastWordWithHyphenGuess =
      guesser.GuessWithScore(guesser.Guess(stringToPredictions("ארץ־")), 0.5)
    val firstWordNextLineGuess =
      guesser.GuessWithScore(guesser.Guess(stringToPredictions("כוש")), 0.5)
    val scoredPair = guesser.getHyphenationStatus(lastWordWithHyphenGuess, firstWordNextLineGuess)

    scoredPair.hyphenationStatus shouldEqual guesser.HyphenatedWithHyphen("ארץ־כוש")
  }

  it should "guess unhyphanted word for unknown but possible word" in {
    val lastWordWithHyphenGuess =
      guesser.GuessWithScore(guesser.Guess(stringToPredictions("גע־")), 0.5)
    val firstWordNextLineGuess =
      guesser.GuessWithScore(guesser.Guess(stringToPredictions("שריבן")), 0.5)
    val scoredPair = guesser.getHyphenationStatus(lastWordWithHyphenGuess, firstWordNextLineGuess)

    scoredPair.hyphenationStatus shouldEqual guesser.HyphenatedWithoutHyphen("געשריבן")
  }

}
