package com.joliciel.jochre.ocr.core.model

import org.bytedeco.opencv.opencv_core.RotatedRect

import java.awt
import scala.xml.Node

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

    def rescale(scale: Double): Rectangle =
      Rectangle(label, (left.toDouble * scale).toInt, (top.toDouble * scale).toInt, (width.toDouble * scale).toInt, (height.toDouble * scale).toInt)

    def translate(xDiff: Int, yDiff: Int): Rectangle =
      Rectangle(label, left + xDiff, top + yDiff, width, height)

    def rotate(imageInfo: ImageInfo): Rectangle = {
      val (x1r, y1r) = imageInfo.rotate(left, top)
      val (x2r, y2r) = imageInfo.rotate(left+width, top+height)
      Rectangle(label, x1r, y1r, x2r-x1r, y2r-y1r)
    }

    def toAWT(): java.awt.Rectangle = new awt.Rectangle(this.left, this.top, this.width, this.height)
  }

  object Rectangle {
    def apply(label: String, rotatedRect: RotatedRect): Rectangle =
      Rectangle(label, rotatedRect.boundingRect().x, rotatedRect.boundingRect().y, rotatedRect.boundingRect().width, rotatedRect.boundingRect.height)

    def fromXML(label: String, node: Node): Rectangle = Rectangle(label, left=(node \@ "HPOS").toInt, top=(node \@ "VPOS").toInt, width=(node \@ "WIDTH").toInt, height = (node \@ "HEIGHT").toInt)
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

    def rescale(scale: Double): Line =
      Line(label, (x1.toDouble * scale).toInt, (y1.toDouble * scale).toInt, (x2.toDouble * scale).toInt, (y2.toDouble * scale).toInt)

    def rotate(imageInfo: ImageInfo): Line = {
      val (x1r, y1r) = imageInfo.rotate(x1, y1)
      val (x2r, y2r) = imageInfo.rotate(x2, y2)
      Line(label, x1r, y1r, x2r, y2r)
    }

    def translate(xDiff: Int, yDiff: Int): Line =
      Line(label, x1 + xDiff, y1 + yDiff, x2 + xDiff, y2 + yDiff)
  }

  object Line {
    def fromXML(imageInfo: ImageInfo, node: Node): Line = {
      // BASELINE: According to Alto: Pixel coordinates based on the left-hand top corner of an image which define a polyline on which a line of text rests
      // In Jochre2, this is a single INT (indicating the Y coordinate only, with X taken from LEFT)
      val baseLineText = (node \@ "BASELINE")

      val left = (node \@ "HPOS").toInt
      val width = (node \@ "WIDTH").toInt

      val (x1, y1, x2, y2) = baseLineText.toIntOption match {
        case Some(yCoord) =>
          val x1 = left
          val y1 = yCoord
          val right = x1+width
          val (x2, y2) = imageInfo.rotate(right, y1)
          // I can't explain why right gives better results than x2 here.
          // Hopefully this only affects the strange case of Jochre2 with a single int value for the BASELINE
          (x1, y1, right, y2)
        case None =>
          val sets = baseLineText.split(' ')
          if (sets.length==1) {
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

      Line("", x1, y1, x2, y2)
    }
  }
}