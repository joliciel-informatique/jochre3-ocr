package com.joliciel.jochre.ocr.core.graphics

import scala.xml.Node

case class Line(x1: Int, y1: Int, x2: Int, y2: Int) extends Ordered[Line] {
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

  def compare(that: Line): Int = (this.y1, this.x1, this.y2, this.x2) compare(that.y1, that.x1, that.y2, that.x2)

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
        val right = x1 + width
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
