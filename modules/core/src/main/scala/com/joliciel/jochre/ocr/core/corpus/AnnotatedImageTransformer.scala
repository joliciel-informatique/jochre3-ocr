package com.joliciel.jochre.ocr.core.corpus

import com.joliciel.jochre.ocr.core.model.Page
import com.joliciel.jochre.ocr.core.transform.ImageTransformer
import org.bytedeco.opencv.opencv_core.Mat

trait AnnotatedImageTransformer[D] {

  /** Transform the initial image so that both the image and the Page continue to correspond after the transformation
    * (e.g. both have been rotated by the same angle).
    */
  def transform(path: String, mat: Mat, page: Page): (Mat, Page, D)
}

object AnnotatedImageTransformer {
  def apply[T](
      imageTransformer: ImageTransformer[T]
  ): AnnotatedImageTransformer[T] =
    (path: String, mat: Mat, page: Page) => {
      val (transformedMat, result) = imageTransformer.transform(path, mat)
      (transformedMat, page, result)
    }
}
