package com.joliciel.jochre.ocr.core.evaluation

trait TextEvaluationMetric {
  def evaluate(predicted: String, expected: String): Double

  def name: String
}

object CharacterCount extends TextEvaluationMetric {
  override def evaluate(predicted: String, expected: String): Double = expected.length

  override val name: String = "CharCount"
}