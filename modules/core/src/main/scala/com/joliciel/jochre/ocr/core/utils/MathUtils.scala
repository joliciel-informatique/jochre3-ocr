package com.joliciel.jochre.ocr.core.utils

object MathUtils {
  trait MathImplicits {
    implicit class DoubleRounder(n: Double) {
      def roundTo(precision: Int): Double = {
        val s = math pow (10, precision); (math round n * s) / s
      }
    }
  }

  object MathImplicits extends MathImplicits

  def argMax[A, B](
      c: Iterable[A]
  )(f: A => B)(implicit o: Ordering[B]): Iterable[A] = {
    val maxOption = c.map(f).maxOption(o)
    maxOption
      .map { max =>
        c.filter(f(_) == max)
      }
      .getOrElse(Iterable.empty)
  }

  def argMaxFirst[A, B](
      c: Iterable[A]
  )(f: A => B)(implicit o: Ordering[B]): Option[A] = {
    val maxOption = c.map(f).maxOption(o)
    maxOption.flatMap { max =>
      c.find(f(_) == max)
    }
  }
}
