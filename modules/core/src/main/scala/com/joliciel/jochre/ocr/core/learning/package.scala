package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.corpus.{AnnotatedImageTransformer, RotationTransformer}
import com.joliciel.jochre.ocr.core.transform.BrightnessAndContrastTransform
import com.typesafe.config.ConfigFactory

package object learning {
  case class Prediction(outcome: String, confidence: Double)

  private val config = ConfigFactory.load().getConfig("jochre.ocr.transforms")
  private val applyContrastAndBrightness =
    config.getBoolean("apply-contrast-and-brightness")
  private val contrast = config.getDouble("contrast")
  private val brightness = config.getInt("brightness")

  val transforms: Seq[AnnotatedImageTransformer[_]] = Seq[Option[AnnotatedImageTransformer[_]]](
    // Increase contrast and brightness
    Option.when(applyContrastAndBrightness)(
      AnnotatedImageTransformer(
        new BrightnessAndContrastTransform(contrast, brightness)
      )
    ),
    Some(RotationTransformer)
  ).flatten
}
