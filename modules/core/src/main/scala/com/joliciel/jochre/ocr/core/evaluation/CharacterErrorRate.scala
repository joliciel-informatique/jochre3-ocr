package com.joliciel.jochre.ocr.core.evaluation

import org.apache.commons.text.similarity.LevenshteinDistance

object CharacterErrorRate extends TextEvaluationMetric {
  val levenshteinDistanceFinder = LevenshteinDistance.getDefaultInstance
  override def evaluate(predicted: String, expected: String): Double = {
    val levenshteinDistance = levenshteinDistanceFinder.apply(predicted, expected)
    val cer = levenshteinDistance.toDouble / expected.length.toDouble
    cer
  }

  override val name = "CER"
}
