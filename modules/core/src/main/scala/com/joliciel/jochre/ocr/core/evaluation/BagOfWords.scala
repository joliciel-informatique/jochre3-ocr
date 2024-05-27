package com.joliciel.jochre.ocr.core.evaluation

import org.apache.commons.text.similarity.{CosineDistance, CosineSimilarity}

import scala.jdk.CollectionConverters._

object BagOfWords extends TextEvaluationMetric {

  override def evaluate(predicted: String, expected: String): Double = {
    val expectedBag = expected
      .split(raw"\s+")
      .groupBy(w => w)
      .view
      .mapValues(group => Integer.valueOf(group.size))
      .toMap[CharSequence, Integer]
      .asJava

    val predictedBag = predicted
      .split(raw"\s+")
      .groupBy(w => w)
      .view
      .mapValues(group => Integer.valueOf(group.size))
      .toMap[CharSequence, Integer]
      .asJava

    val cosineSimilarity = new CosineSimilarity()
    cosineSimilarity.cosineSimilarity(expectedBag, predictedBag)
  }

  override def name: String = "BagOfWords"
}
