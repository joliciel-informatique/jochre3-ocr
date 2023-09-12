package com.joliciel.jochre.ocr.core.analysis

import java.awt.image.BufferedImage
import scala.xml.Elem

trait TextAnalyzer {
  def analyze(image: BufferedImage): Elem
}
