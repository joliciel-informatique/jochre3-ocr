package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.model.ImageLabel
import com.joliciel.jochre.ocr.core.utils.OutputLocation
import org.bytedeco.opencv.opencv_core.Mat

trait ImageLabelDetector[T <: ImageLabel] {
  def outputLocation: Option[OutputLocation]

  def detect(image: Mat, label: String): Seq[T]
}
