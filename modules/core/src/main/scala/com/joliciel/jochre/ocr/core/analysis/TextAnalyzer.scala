package com.joliciel.jochre.ocr.core.analysis

import java.awt.image.BufferedImage
import scala.xml.Elem

/**
 * Given an image, produces Alto XML.
 */
trait TextAnalyzer {
  def analyze(image: BufferedImage): Option[Elem]
}
