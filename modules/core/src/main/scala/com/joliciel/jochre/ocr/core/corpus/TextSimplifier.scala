package com.joliciel.jochre.ocr.core.corpus

trait TextSimplifier {
  def simplify(text: String): String
}

object TextSimplifier {
  val default: TextSimplifier = new TextSimplifier {
    override def simplify(text: String): String = text
  }
}