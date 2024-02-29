package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.utils.StringUtils
import com.typesafe.config.ConfigFactory
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

  object WithRectangle {
    case class HorizontalOrdering[T <: WithRectangle](leftToRight: Boolean) extends Ordering[T] {
      def compare(a: T, b: T): Int = a.rectangle.horizontalCompare(b.rectangle, leftToRight)
    }

    case class VerticalOrdering[T <: WithRectangle]() extends Ordering[T] {
      def compare(a: T, b: T): Int = a.rectangle.verticalCompare(b.rectangle)
    }

    case class SimplePageLayoutOrdering[T <: WithRectangle](leftToRight: Boolean) extends Ordering[T] {
      def compare(a: T, b: T): Int = a.rectangle.simplePageLayoutCompare(b.rectangle, leftToRight)
    }
  }

  trait Block extends PageElement with WithRectangle {
    def rectangle: Rectangle
  }

  trait TextContainer extends Block

  trait WithLanguage {
    def language: Option[String]
    def defaultLanguage: Option[String]
    def withDefaultLanguage(defaultLanguage: String): WithLanguage

    def languageOrDefault: String = {
      getEffectiveLanguage(this.language, this.defaultLanguage)
    }

    def isLeftToRight: Boolean = {
      StringUtils.isLeftToRight(this.languageOrDefault)
    }

    def getEffectiveLanguage(language: Option[String], defaultLanguage: Option[String]): String = {
      language.getOrElse(
        defaultLanguage.getOrElse {
          ConfigFactory.load().getConfig("jochre.ocr").getString("language")
        }
      )
    }
  }

  trait WordOrSpace extends PageElement with WithRectangle

  trait Tag {
    def toXml: Elem
  }
}
