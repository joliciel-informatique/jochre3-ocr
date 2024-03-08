package com.joliciel.jochre.ocr.core.graphics

trait WithRectangle {
  def rectangle: Rectangle

  def left: Int = rectangle.left

  def right: Int = rectangle.right

  def top: Int = rectangle.top

  def bottom: Int = rectangle.bottom

  def width: Int = right - left

  def verticalOverlap(that: WithRectangle): Int = {
    val maxTop = Math.max(this.top, that.top)
    val minBottom = Math.min(this.bottom, that.bottom)
    val verticalOverlap = minBottom - maxTop
    if (verticalOverlap < 0) {
      0
    } else {
      verticalOverlap
    }
  }

  def horizontalOverlap(that: WithRectangle): Int = {
    val maxLeft = Math.max(this.left, that.left)
    val minRight = Math.min(this.right, that.right)
    val horizontalOverlap = minRight - maxLeft
    if (horizontalOverlap < 0) {
      0
    } else {
      horizontalOverlap
    }
  }
}

object WithRectangle {
  case class HorizontalOrdering[T <: WithRectangle](leftToRight: Boolean) extends Ordering[T] {
    def compare(a: T, b: T): Int =
      a.rectangle.horizontalCompare(b.rectangle, leftToRight)
  }

  case class VerticalOrdering[T <: WithRectangle]() extends Ordering[T] {
    def compare(a: T, b: T): Int = a.rectangle.verticalCompare(b.rectangle)
  }

  case class SimplePageLayoutOrdering[T <: WithRectangle](leftToRight: Boolean) extends Ordering[T] {
    def compare(a: T, b: T): Int =
      a.rectangle.simplePageLayoutCompare(b.rectangle, leftToRight)
  }
}
