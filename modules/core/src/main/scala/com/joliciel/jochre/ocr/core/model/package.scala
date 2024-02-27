package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import org.bytedeco.opencv.opencv_core.Mat

import scala.xml.Elem

package object model {
  case class ImageInfo(width: Int, height: Int, rotation: Double) {
    private val r = Math.toRadians(rotation)
    private val cosR = Math.cos(r)
    private val sinR = Math.sin(r)

    private val centerX = width.toDouble / 2.0
    private val centerY = height.toDouble / 2.0

    def rotate(x: Int, y: Int): (Int, Int) = {
      val rotX = (centerX + (x - centerX) * cosR - (y - centerY) * sinR).toInt
      val rotY = (centerY + (x - centerX) * sinR + (y - centerY) * cosR).toInt
      (rotX, rotY)
    }
  }

  trait AltoElement {
    def toXml: Elem

    def transform(partialFunction: PartialFunction[AltoElement, AltoElement]): AltoElement
  }

  trait PageElement extends AltoElement {
    def translate(xDiff: Int, yDiff: Int): PageElement

    def rotate(imageInfo: ImageInfo): PageElement

    def rescale(scale: Double): PageElement

    def draw(mat: Mat): Unit

    def content: String
  }

  trait WithRectangle {
    def rectangle: Rectangle

    def left: Int = rectangle.left

    def right: Int = rectangle.right

    def top: Int = rectangle.top

    def bottom: Int = rectangle.bottom

    def width: Int = right - left

    def verticalOverlap(that: WithRectangle): Int = {
      val maxTop = Math.max(this.top, that.top)
      val minBottom = Math.max(this.bottom, that.bottom)
      val verticalOverlap = minBottom - maxTop
      if (verticalOverlap < 0) {
        0
      } else {
        verticalOverlap
      }
    }

    def horizontalOverlap(that: WithRectangle): Int = {
      val maxLeft = Math.max(this.left, that.left)
      val minRight = Math.max(this.right, that.right)
      val horizontalOverlap = minRight - maxLeft
      if (horizontalOverlap < 0) {
        0
      } else {
        horizontalOverlap
      }
    }
  }

  trait Block extends PageElement with WithRectangle {
    def rectangle: Rectangle
  }

  trait TextContainer extends Block

  trait WordOrSpace extends PageElement with Ordered[WordOrSpace] {
    def rectangle: Rectangle

    override def compareTo(that: WordOrSpace): Int = this.rectangle.left.compare(that.rectangle.left)
  }

  trait Tag {
    def toXml: Elem
  }
}
