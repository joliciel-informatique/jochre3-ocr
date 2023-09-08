package com.joliciel.jochre.ocr.core.transform

import org.bytedeco.opencv.global.opencv_core
import org.bytedeco.opencv.opencv_core.Mat

class BoxTransform(dimension: Int) extends ImageTransformer {
  override def transform(path: String, src: Mat): Mat = {
    val width = src.cols()
    val height = src.rows()

    val top = (dimension - height) / 2
    val bottom = dimension - (height + top)
    val left = (dimension - width) / 2
    val right = dimension - (width + left)

    val dest = new Mat()
    opencv_core.copyMakeBorder(src, dest, top, bottom, left, right, opencv_core.BORDER_CONSTANT)
    dest
  }
}
