package com.joliciel.jochre.ocr.core.utils

trait StringUtils {

  /** Splits a string into characters, but keeps combining characters combined with their base
    * character.
    */
  def stringToChars(string: String): Seq[String] =
    string.foldLeft(Seq.empty[String]) { case (chars, char) =>
      val charType = Character.getType(char)
      if (
        charType == Character.COMBINING_SPACING_MARK || charType == Character.ENCLOSING_MARK || charType == Character.NON_SPACING_MARK
      ) {
        chars match {
          case Nil   => chars :+ f"$char"
          case chars => chars.init :+ f"${chars.last}$char"
        }
      } else {
        chars :+ f"$char"
      }
    }

  private val rightToLeftLanguages = Set(
    "ar",
    "dv",
    "fa",
    "ha",
    "he",
    "iw",
    "ji",
    "ps",
    "sd",
    "ug",
    "ur",
    "yi"
  )

  def isLeftToRight(language: String): Boolean = {
    !rightToLeftLanguages.contains(language)
  }
}

object StringUtils extends StringUtils
