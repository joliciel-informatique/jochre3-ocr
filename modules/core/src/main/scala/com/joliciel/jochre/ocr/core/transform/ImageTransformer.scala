package com.joliciel.jochre.ocr.core.transform

import org.bytedeco.opencv.global.opencv_core
import org.bytedeco.opencv.global.opencv_imgproc.{INTER_LINEAR, getRotationMatrix2D, warpAffine}
import org.bytedeco.opencv.opencv_core.{Mat, Point2f, Scalar}

trait ImageTransformer[D] {
  def transform(path: String, mat: Mat): (Mat, D)

  def rotate(angle: Double, mat: Mat): Mat = {
    val cy = mat.rows.toFloat / 2f
    val cx = mat.cols.toFloat / 2f
    val result = new Mat
    val matrix2D = getRotationMatrix2D(new Point2f(cx, cy), 0 - angle, 1)
    warpAffine(mat, result, matrix2D, mat.size, INTER_LINEAR, opencv_core.BORDER_REFLECT, new Scalar(0.0))
    result
  }
}
