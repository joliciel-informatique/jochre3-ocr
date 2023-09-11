package com.joliciel.jochre.ocr.core

package object model {
  case class ImageAndLabels(width: Int, height: Int, labels: Seq[ImageLabel])
}
