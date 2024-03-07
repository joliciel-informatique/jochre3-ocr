package com.joliciel.jochre.ocr.core.learning

import scala.util.matching.Regex

/** On the assumption that the main glyph guesser recognizes other alphabets, and marks them with a
  * certain glyph, this class will find words which have been marked as another alphabet (based on
  * the regex) and apply another glyph guesser to these words.
  */
case class GlyphGuesserForAnotherAlphabet(
    language: String,
    regex: Regex,
    glyphGuesser: GlyphGuesser
)

case class GlyphGuessersForOtherAlphabets(
    glyphGuessers: Seq[GlyphGuesserForAnotherAlphabet]
)
