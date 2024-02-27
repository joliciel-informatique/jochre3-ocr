package com.joliciel.jochre.ocr.core.model

import org.bytedeco.opencv.opencv_core.RotatedRect

import java.awt
import scala.xml.Node

sealed trait ImageLabel {}

object ImageLabel {
  case class PredictedRectangle(label: String, rectangle: Rectangle, confidence: Double) extends ImageLabel with WithRectangle {
  }

  case class Rectangle(override val left: Int, override val top: Int, override val width: Int, height: Int) extends ImageLabel with WithRectangle {
    val rectangle: Rectangle = this
    val area: Int = width * height

    override val right: Int = left + width
    override val bottom: Int = top + height

    val xCenter: Int = (left + right) / 2
    val yCenter: Int = (top + bottom) / 2

    def contains(that: Rectangle): Boolean = left <= that.left && top <= that.top &&
      left + width >= that.left + that.width && top + height >= that.top + that.height

    def intersection(that: Rectangle): Option[Rectangle] = {
      val maxLeft = math.max(this.left, that.left)
      val maxTop = math.max(this.top, that.top)
      val minRight = math.min(this.right, that.right)
      val minBottom = math.min(this.bottom, that.bottom)

      Option.when(maxTop < minBottom && maxLeft < minRight)(Rectangle(maxLeft, maxTop, minRight - maxLeft, minBottom - maxTop))
    }

    def union(that: Rectangle): Rectangle = {
      val minLeft = math.min(this.left, that.left)
      val minTop = math.min(this.top, that.top)
      val maxRight = math.max(this.right, that.right)
      val maxBottom = math.max(this.bottom, that.bottom)
      Rectangle(minLeft, minTop, maxRight - minLeft, maxBottom - minTop)
    }

    def areaOfIntersection(that: Rectangle): Double = intersection(that).map(_.area.toDouble).getOrElse(0.0)

    def percentageIntersection(that: Rectangle): Double = this.areaOfIntersection(that) / this.area.toDouble

    def areaOfUnion(that: Rectangle): Double = {
      (area + that.area) - areaOfIntersection(that)
    }

    def iou(that: Rectangle): Double = areaOfIntersection(that) / areaOfUnion(that)

    /**
     * Note: this compare will fail for complex pages.
     * Use BlockSorter instead (which tries this compare and, if it fails,
     * performs a more complex compare).
     */
    def simplePageLayoutCompare(that: Rectangle): Int = {
      // We'll assume right-to-left for now
      if (this.left >= that.right) return -1
      if (this.right <= that.left) return 1
      if (this.top < that.top) return -1
      if (that.top < this.top) return 1
      if (this.bottom < that.bottom) return -1
      if (that.bottom < this.bottom) return 1
      if (this.right > that.right) return -1
      if (that.right > this.right) return 1
      if (this.left < that.left) return 1
      if (that.left < this.left) return -1
      0
    }

    def horizontalCompare(that: Rectangle): Int = {
      if (this.right > that.right) return -1
      if (that.right > this.right) return 1
      if (this.left < that.left) return 1
      if (that.left < this.left) return -1
      0
    }

    def verticalCompare(that: Rectangle): Int = {
      if (this.top < that.top) return -1
      if (that.top < this.top) return 1
      if (this.bottom < that.bottom) return -1
      if (that.bottom < this.bottom) return 1
      0
    }

    /**
     * Return 0 if there's an overlap > 50%.
     * Return -1 if we should check later rectangles.
     * Return 1 if we should check earlier rectangles.
     */
    def testVerticalOverlap(that: Rectangle): Int = {
      if (this.top >= that.bottom) return 1
      if (this.bottom <= that.top) return -1
      if (areaOfIntersection(that) / that.area.toDouble > 0.5) return 0
      if (this.top < that.top) return -1
      if (that.top < this.top) return 1
      if (this.bottom < that.bottom) return -1
      if (that.bottom < this.bottom) return 1
      -1
    }

    /**
     * Return 0 if there's an overlap > 50%.
     * Return -1 if we should check later rectangles.
     * Return 1 if we should check earlier rectangles.
     */
    def testHorizontalOverlap(that: Rectangle): Int = {
      // We'll assume right-to-left for now
      if (this.left >= that.right) return -1
      if (this.right <= that.left) return 1
      if (areaOfIntersection(that) / that.area.toDouble > 0.5) return 0
      if (this.right > that.right) return 1
      if (that.right > this.right) return -1
      if (this.left < that.left) return -1
      if (that.left < this.left) return 1
      -1
    }

    def rescale(scale: Double): Rectangle =
      Rectangle((left.toDouble * scale).toInt, (top.toDouble * scale).toInt, (width.toDouble * scale).toInt, (height.toDouble * scale).toInt)

    def translate(xDiff: Int, yDiff: Int): Rectangle =
      Rectangle(left + xDiff, top + yDiff, width, height)

    def rotate(imageInfo: ImageInfo): Rectangle = {
      val (x1r, y1r) = imageInfo.rotate(left, top)
      val (x2r, y2r) = imageInfo.rotate(left+width, top+height)
      Rectangle(x1r, y1r, x2r-x1r, y2r-y1r)
    }

    def toAWT: java.awt.Rectangle = new awt.Rectangle(this.left, this.top, this.width, this.height)

    lazy val coordinates: String = f"Rectangle(l$left,t$top,r$right,b$bottom)"
  }

  object Rectangle {
    def apply(rotatedRect: RotatedRect): Rectangle =
      Rectangle(rotatedRect.boundingRect().x, rotatedRect.boundingRect().y, rotatedRect.boundingRect().width, rotatedRect.boundingRect.height)

    def fromXML(node: Node): Rectangle = Rectangle(
      left=(node \@ "HPOS").toIntOption.getOrElse(0),
      top=(node \@ "VPOS").toIntOption.getOrElse(0),
      width=(node \@ "WIDTH").toIntOption.getOrElse(1),
      height = (node \@ "HEIGHT").toIntOption.getOrElse(1)
    )

    object HorizontalOrdering extends Ordering[Rectangle] {
      def compare(a: Rectangle, b: Rectangle): Int = a.horizontalCompare(b)
    }

    object VerticalOrdering extends Ordering[Rectangle] {
      def compare(a: Rectangle, b: Rectangle): Int = a.verticalCompare(b)
    }

    object SimplePageLayoutOrdering extends Ordering[Rectangle] {
      def compare(a: Rectangle, b: Rectangle): Int = a.simplePageLayoutCompare(b)
    }
  }

  case class Line(x1: Int, y1: Int, x2: Int, y2: Int) extends ImageLabel with Ordered[Line] {
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

    def compare(that: Line): Int = (this.y1, this.x1, this.y2, this.x2) compare (that.y1, that.x1, that.y2, that.x2)

    def rescale(scale: Double): Line =
      Line((x1.toDouble * scale).toInt, (y1.toDouble * scale).toInt, (x2.toDouble * scale).toInt, (y2.toDouble * scale).toInt)

    def rotate(imageInfo: ImageInfo): Line = {
      val (x1r, y1r) = imageInfo.rotate(x1, y1)
      val (x2r, y2r) = imageInfo.rotate(x2, y2)
      Line(x1r, y1r, x2r, y2r)
    }

    def translate(xDiff: Int, yDiff: Int): Line =
      Line(x1 + xDiff, y1 + yDiff, x2 + xDiff, y2 + yDiff)
  }

  object Line {
    def fromXML(imageInfo: ImageInfo, node: Node): Line = {
      // BASELINE: According to Alto: Pixel coordinates based on the left-hand top corner of an image which define a polyline on which a line of text rests
      // In Jochre2, this is a single INT (indicating the Y coordinate only, with X taken from LEFT)
      val baseLineText = node \@ "BASELINE"

      val left = (node \@ "HPOS").toIntOption.getOrElse(0)
      val width = (node \@ "WIDTH").toIntOption.getOrElse(1)

      val (x1, y1, x2, y2) = baseLineText.toIntOption match {
        case Some(yCoord) =>
          val x1 = left
          val y1 = yCoord
          val right = x1+width
          val (x2, y2) = imageInfo.rotate(right, y1)
          // I can't explain why right gives better results than x2 here.
          // Hopefully this only affects the strange case of Jochre2 with a single int value for the BASELINE
          (x1, y1, x2, y2)
        case None =>
          if (baseLineText.isEmpty) {
            (0, 0, 0, 0)
          } else {
            val sets = baseLineText.split(' ')
            if (sets.length == 1) {
              val ints = sets(0).split(',').map(_.toInt)
              val x1 = ints(0)
              val y1 = ints(1)
              val (x2, y2) = imageInfo.rotate(x1 + width, y1)
              (x1, y1, x2, y2)
            } else {
              val ints = sets.map(_.split(',').map(_.toInt))
              val x1 = ints(0)(0)
              val y1 = ints(0)(1)
              val x2 = ints(1)(0)
              val y2 = ints(1)(1)
              (x1, y1, x2, y2)
            }
          }
      }

      Line(x1, y1, x2, y2)
    }
  }
}