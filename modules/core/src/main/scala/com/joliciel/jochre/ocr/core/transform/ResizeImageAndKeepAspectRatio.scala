package com.joliciel.jochre.ocr.core.transform

import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.opencv_core.{Mat, Size}
import org.slf4j.LoggerFactory

class ResizeImageAndKeepAspectRatio(val newLongestDimension: Int) extends ImageTransformer[Scale] {
  private val log = LoggerFactory.getLogger(getClass)

  override def transform(path: String, mat: Mat): (Mat, Scale) = {
    val initialHeight = mat.rows
    val initialWidth = mat.cols

    val (dest, scale) = if (initialHeight > initialWidth) {
      if (newLongestDimension < initialHeight) {
        val newHeight = newLongestDimension
        val newWidth = (newHeight.toDouble * (initialWidth.toDouble / initialHeight.toDouble)).round.toInt
        val scale = newHeight.toDouble / initialHeight.toDouble
        if (log.isTraceEnabled) log.trace("newWidth: " + newWidth + ", newHeight: " + newHeight)
        val dest = new Mat
        opencv_imgproc.resize(mat, dest, new Size(newWidth, newHeight), 0, 0, opencv_imgproc.INTER_AREA)
        dest -> scale
      } else {
        mat -> 1.0
      }
    } else {
      if (newLongestDimension < initialWidth) {
        val newWidth = newLongestDimension
        val newHeight = (newWidth.toDouble * (initialHeight.toDouble / initialWidth.toDouble)).round.toInt
        val scale = newWidth.toDouble / initialWidth.toDouble
        if (log.isTraceEnabled) log.trace("newWidth: " + newWidth + ", newHeight: " + newHeight)
        val dest = new Mat
        opencv_imgproc.resize(mat, dest, new Size(newWidth, newHeight), 0, 0, opencv_imgproc.INTER_AREA)
        dest -> scale
      } else {
        mat -> 1.0
      }
    }
    dest -> Scale(scale)
  }
}
