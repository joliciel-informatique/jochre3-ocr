package com.joliciel.jochre.ocr.core.graphics

import org.bytedeco.opencv.opencv_core.RotatedRect

import java.awt
import scala.xml.Node

case class Rectangle(
    override val left: Int,
    override val top: Int,
    override val width: Int,
    override val height: Int
) extends WithRectangle {
  val rectangle: Rectangle = this
  val area: Int = width * height

  override val right: Int = left + width
  override val bottom: Int = top + height

  val xCenter: Int = (left + right) / 2
  val yCenter: Int = (top + bottom) / 2

  def contains(that: Rectangle): Boolean =
    left <= that.left && top <= that.top &&
      left + width >= that.left + that.width && top + height >= that.top + that.height

  def intersection(that: Rectangle): Option[Rectangle] = {
    val maxLeft = math.max(this.left, that.left)
    val maxTop = math.max(this.top, that.top)
    val minRight = math.min(this.right, that.right)
    val minBottom = math.min(this.bottom, that.bottom)

    Option.when(maxTop < minBottom && maxLeft < minRight)(
      Rectangle(maxLeft, maxTop, minRight - maxLeft, minBottom - maxTop)
    )
  }

  def union(that: Rectangle): Rectangle = {
    val minLeft = math.min(this.left, that.left)
    val minTop = math.min(this.top, that.top)
    val maxRight = math.max(this.right, that.right)
    val maxBottom = math.max(this.bottom, that.bottom)
    Rectangle(minLeft, minTop, maxRight - minLeft, maxBottom - minTop)
  }

  def areaOfIntersection(that: Rectangle): Double =
    intersection(that).map(_.area.toDouble).getOrElse(0.0)

  def percentageIntersection(that: Rectangle): Double =
    this.areaOfIntersection(that) / this.area.toDouble

  def areaOfUnion(that: Rectangle): Double = {
    (area + that.area) - areaOfIntersection(that)
  }

  def iou(that: Rectangle): Double =
    areaOfIntersection(that) / areaOfUnion(that)

  /** Note: this compare will fail for complex pages. Use BlockSorter instead (which tries this compare and, if it
    * fails, performs a more complex compare).
    */
  def simplePageLayoutCompare(that: Rectangle, leftToRight: Boolean): Int = {
    if (leftToRight) {
      if (this.right >= that.left) return 1
      if (this.left <= that.right) return -1
      if (this.top < that.top) return -1
      if (that.top < this.top) return 1
      if (this.bottom < that.bottom) return -1
      if (that.bottom < this.bottom) return 1
      if (this.left > that.left) return 1
      if (that.left > this.left) return -1
      if (this.right < that.right) return -1
      if (that.right < this.right) return 1
      0
    } else {
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
  }

  def horizontalCompare(that: Rectangle, leftToRight: Boolean): Int = {
    if (leftToRight) {
      if (this.left > that.left) return 1
      if (that.left > this.left) return -1
      if (this.right < that.right) return -1
      if (that.right < this.right) return 1
      0
    } else {
      if (this.right > that.right) return -1
      if (that.right > this.right) return 1
      if (this.left < that.left) return 1
      if (that.left < this.left) return -1
      0
    }
  }

  def verticalCompare(that: Rectangle): Int = {
    if (this.top < that.top) return -1
    if (that.top < this.top) return 1
    if (this.bottom < that.bottom) return -1
    if (that.bottom < this.bottom) return 1
    0
  }

  /** Return 0 if there's an overlap > 50%. Return -1 if we should check later rectangles. Return 1 if we should check
    * earlier rectangles.
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

  /** Return 0 if there's an overlap > 50%. Return -1 if we should check later rectangles. Return 1 if we should check
    * earlier rectangles.
    */
  def testHorizontalOverlap(that: Rectangle, leftToRight: Boolean): Int = {
    if (leftToRight) {
      if (this.right >= that.left) return 1
      if (this.left <= that.right) return -1
      if (areaOfIntersection(that) / that.area.toDouble > 0.5) return 0
      if (this.left > that.left) return -1
      if (that.left > this.left) return 1
      if (this.right < that.right) return 1
      if (that.right < this.right) return -1
      -1
    } else {
      if (this.left >= that.right) return -1
      if (this.right <= that.left) return 1
      if (areaOfIntersection(that) / that.area.toDouble > 0.5) return 0
      if (this.right > that.right) return 1
      if (that.right > this.right) return -1
      if (this.left < that.left) return -1
      if (that.left < this.left) return 1
      -1
    }
  }

  def rescale(scale: Double): Rectangle =
    Rectangle(
      (left.toDouble * scale).toInt,
      (top.toDouble * scale).toInt,
      (width.toDouble * scale).toInt,
      (height.toDouble * scale).toInt
    )

  def translate(xDiff: Int, yDiff: Int): Rectangle =
    Rectangle(left + xDiff, top + yDiff, width, height)

  def rotate(imageInfo: ImageInfo): Rectangle = {
    val (x1r, y1r) = imageInfo.rotate(left, top)
    val (x2r, y2r) = imageInfo.rotate(left + width, top + height)
    Rectangle(x1r, y1r, x2r - x1r, y2r - y1r)
  }

  def toAWT: java.awt.Rectangle =
    new awt.Rectangle(this.left, this.top, this.width, this.height)

  def tile(
      horizontalTiles: Int,
      verticalTiles: Int,
      marginPercentage: Double = 0.25
  ): Seq[Rectangle] = {
    val heightSegment = this.height / verticalTiles
    val verticalMargin = (heightSegment.toDouble * marginPercentage).toInt
    val widthSegment = this.width / horizontalTiles
    val horizontalMargin = (widthSegment.toDouble * marginPercentage).toInt
    (0 until horizontalTiles).flatMap { i =>
      (0 until verticalTiles).map { j =>
        val left = (i * widthSegment) - horizontalMargin
        val top = (j * heightSegment) - verticalMargin
        Rectangle(
          left,
          top,
          widthSegment + (2 * horizontalMargin),
          heightSegment + (2 * verticalMargin)
        ).intersection(this.rectangle).get
      }
    }
  }

  lazy val coordinates: String = f"Rectangle(l$left,t$top,r$right,b$bottom)"
}

object Rectangle {
  def apply(rotatedRect: RotatedRect): Rectangle =
    Rectangle(
      rotatedRect.boundingRect().x,
      rotatedRect.boundingRect().y,
      rotatedRect.boundingRect().width,
      rotatedRect.boundingRect.height
    )

  def fromXML(node: Node): Rectangle = Rectangle(
    left = (node \@ "HPOS").toIntOption.getOrElse(0),
    top = (node \@ "VPOS").toIntOption.getOrElse(0),
    width = (node \@ "WIDTH").toIntOption.getOrElse(1),
    height = (node \@ "HEIGHT").toIntOption.getOrElse(1)
  )
}
