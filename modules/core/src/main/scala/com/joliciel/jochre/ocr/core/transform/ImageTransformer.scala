package com.joliciel.jochre.ocr.core.transform

import org.bytedeco.opencv.opencv_core.Mat

trait ImageTransformer[D] {
  def transform(path: String, mat: Mat): (Mat, D)
}
