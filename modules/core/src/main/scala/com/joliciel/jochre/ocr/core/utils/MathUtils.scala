package com.joliciel.jochre.ocr.core.utils

import Numeric.Implicits._

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

  def mean[T: Numeric](xs: Iterable[T]): Double = xs.sum.toDouble / xs.size

  def variance[T: Numeric](xs: Iterable[T]): Double = {
    val avg = mean(xs)

    xs.map(_.toDouble).map(a => math.pow(a - avg, 2)).sum / xs.size
  }

  def stdDev[T: Numeric](xs: Iterable[T]): Double = math.sqrt(variance(xs))
}
