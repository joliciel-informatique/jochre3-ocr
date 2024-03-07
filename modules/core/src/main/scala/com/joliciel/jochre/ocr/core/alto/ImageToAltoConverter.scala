package com.joliciel.jochre.ocr.core.alto

import zio.Task

import java.awt.image.BufferedImage
import scala.xml.Elem

/** Given an image, produces Alto XML.
  */
trait ImageToAltoConverter {
  def analyze(image: BufferedImage): Task[Elem]
}
