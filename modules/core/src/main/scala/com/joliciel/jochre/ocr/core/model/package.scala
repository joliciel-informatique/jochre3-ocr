package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle

import scala.xml.Elem

package object model {
  case class ImageInfo(width: Int, height: Int, rotation: Double) {
    private val r = Math.toRadians(rotation)
    private val cosR = Math.cos(r)
    private val sinR = Math.sin(r)

    private val centerX = (width.toDouble / 2.0)
    private val centerY = (height.toDouble / 2.0)

    def rotate(x: Int, y: Int): (Int, Int) = {
      val rotX = (centerX + (x - centerX) * cosR - (y - centerY) * sinR).toInt
      val rotY = (centerY + (x - centerX) * sinR + (y - centerY) * cosR).toInt
      (rotX, rotY)
    }
  }

  trait PageElement {
    def translate(xDiff: Int, yDiff: Int): PageElement

    def rotate(imageInfo: ImageInfo): PageElement

    def toXml(id: String = ""): Elem
  }

  trait Block extends PageElement with Ordered[Block] {
    def rectangle: Rectangle

    override def compareTo(that: Block): Int = this.rectangle.compare(that.rectangle)
  }

  trait WordOrSpace extends PageElement with Ordered[WordOrSpace] {
    def rectangle: Rectangle

    override def compareTo(that: WordOrSpace): Int = this.rectangle.left.compare(that.rectangle.left)
  }
}
