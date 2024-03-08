package com.joliciel.jochre.ocr.core.graphics

case class PredictedRectangle(
    label: String,
    rectangle: Rectangle,
    confidence: Double
) extends WithRectangle
