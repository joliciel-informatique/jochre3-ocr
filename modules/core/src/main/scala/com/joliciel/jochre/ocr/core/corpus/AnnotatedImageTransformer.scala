package com.joliciel.jochre.ocr.core.corpus

import com.joliciel.jochre.ocr.core.model.Page
import org.bytedeco.opencv.opencv_core.Mat

trait AnnotatedImageTransformer[D] {
  def transform(path: String, mat: Mat, page: Page): (Mat, Page, D)
}
