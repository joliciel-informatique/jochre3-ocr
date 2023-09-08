package com.joliciel.jochre.ocr.core.transform

import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.opencv_core.{Mat, Size}
import org.slf4j.LoggerFactory

class ResizeImageAndKeepAspectRatio(val newLongestDimension: Int) extends ImageTransformer {
  private val log = LoggerFactory.getLogger(getClass)

  private var newWidth = 0
  private var newHeight = 0

  private var srch = 0
  private var srcw = 0

  override def transform(path: String, mat: Mat): Mat = {
    val result = new Mat
    srch = mat.rows
    srcw = mat.cols
    if (mat.rows > mat.cols) {
      newHeight = newLongestDimension
      newWidth = (newHeight.toDouble * (mat.cols.toDouble / mat.rows.toDouble)).round.toInt
    } else {
      newWidth = newLongestDimension
      newHeight = (newWidth.toDouble * (mat.rows.toDouble / mat.cols.toDouble)).round.toInt
    }
    if (log.isTraceEnabled) log.trace("newWidth: " + newWidth + ", newHeight: " + newHeight)
    opencv_imgproc.resize(mat, result, new Size(newWidth, newHeight), 0, 0, opencv_imgproc.INTER_AREA)
    result
  }
}
