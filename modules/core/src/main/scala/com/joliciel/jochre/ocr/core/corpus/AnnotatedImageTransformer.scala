package com.joliciel.jochre.ocr.core.corpus

import com.joliciel.jochre.ocr.core.model.Page
import org.bytedeco.opencv.opencv_core.Mat

trait AnnotatedImageTransformer[D] {
  /**
   * Transform the initial image so that both the image and the Page continue
   * to correspond after the transformation (e.g. both have been rotated by the same angle).
   */
  def transform(path: String, mat: Mat, page: Page): (Mat, Page, D)
}
