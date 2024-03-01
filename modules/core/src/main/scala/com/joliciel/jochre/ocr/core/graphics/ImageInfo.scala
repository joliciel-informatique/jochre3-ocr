package com.joliciel.jochre.ocr.core.graphics

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
