package com.joliciel.jochre.ocr.core.corpus

import com.joliciel.jochre.ocr.core.model.Page
import com.joliciel.jochre.ocr.core.utils.ImageUtils
import org.bytedeco.opencv.opencv_core.Mat

object RotationTransformer extends AnnotatedImageTransformer[Double] with ImageUtils {
  override def transform(
      path: String,
      mat: Mat,
      page: Page
  ): (Mat, Page, Double) = {
    val angle = page.rotation
    val unrotatedMat = this.unrotate(angle, mat)
    val unrotatedPage = page.unrotate()
    (unrotatedMat, unrotatedPage, angle)
  }
}
