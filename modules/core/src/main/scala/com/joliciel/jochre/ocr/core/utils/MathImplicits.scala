package com.joliciel.jochre.ocr.core.utils

trait MathImplicits {
  implicit class DoubleRounder(n: Double) {
    def roundTo(precision: Int): Double = { val s = math pow (10, precision); (math round n * s) / s }
  }
}

object MathImplicits extends MathImplicits
