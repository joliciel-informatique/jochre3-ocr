package com.joliciel.jochre.ocr.core.model

import org.bytedeco.opencv.opencv_core.RotatedRect

sealed trait ImageLabel {
  val label: String
}

object ImageLabel {
  case class Rectangle(label: String, left: Int, top: Int, width: Int, height: Int) extends ImageLabel with Ordered[Rectangle] {
    val area: Int = width * height

    val right: Int = left + width
    val bottom: Int = top + height

    def contains(that: Rectangle): Boolean = left <= that.left && top <= that.top &&
      left + width >= that.left + that.width && top + height >= that.top + that.height

    def intersection(that: Rectangle): Option[Rectangle] = {
      val maxLeft = math.max(this.left, that.left)
      val maxTop = math.max(this.top, that.top)
      val minRight = math.min(this.right, that.right)
      val minBottom = math.min(this.bottom, that.bottom)

      Option.when(maxTop < minBottom && maxLeft < minRight)(Rectangle(label, maxLeft, maxTop, minRight - maxLeft, minBottom - maxTop))
    }

    private def areaOfIntersection(that: Rectangle): Double = intersection(that).map(_.area.toDouble).getOrElse(0.0)

    private def areaOfUnion(that: Rectangle): Double = {
      (area + that.area) - areaOfIntersection(that)
    }

    def iou(that: Rectangle): Double = areaOfIntersection(that) / areaOfUnion(that)

    import scala.math.Ordered.orderingToOrdered

    def compare(that: Rectangle): Int = (this.top, this.left, this.width, this.height, this.label) compare (that.top, that.left, that.width, that.height, that.label)
  }

  object Rectangle {
    def apply(label: String, rotatedRect: RotatedRect): Rectangle =
      Rectangle(label, rotatedRect.boundingRect().x, rotatedRect.boundingRect().y, rotatedRect.boundingRect().width, rotatedRect.boundingRect.height)
  }

  case class Line(label: String, x1: Int, y1: Int, x2: Int, y2: Int) extends ImageLabel with Ordered[Line] {
    val height: Int = Math.abs(y2 - y1)
    val width: Int = Math.abs(x2 - x1)

    def overlapsHorizontallyWith(that: Line, maxGap: Int): Boolean = {
      val overlapLeft = math.max(this.x1, that.x1)
      val overlapTop = math.abs(this.y1 - that.y1)
      val overlapRight = math.min(this.x2, that.x2)
      val overlapBottom = math.abs(this.y2 - that.y2)
      overlapLeft < overlapRight && overlapTop < maxGap && overlapBottom < maxGap
    }

    def horizontalSpan: Int = this.x2 - this.x1

    import scala.math.Ordered.orderingToOrdered

    def compare(that: Line): Int = (this.y1, this.x1, this.y2, this.x2, this.label) compare (that.y1, that.x1, that.y2, that.x2, that.label)
  }
}