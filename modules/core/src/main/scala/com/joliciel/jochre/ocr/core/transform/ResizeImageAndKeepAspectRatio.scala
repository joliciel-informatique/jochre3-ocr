package com.joliciel.jochre.ocr.core.transform

import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.opencv_core.{Mat, Size}
import org.slf4j.LoggerFactory

class ResizeImageAndKeepAspectRatio(val maxWidth: Int, maxHeight: Int) extends ImageTransformer[Scale] {
  private val log = LoggerFactory.getLogger(getClass)

  override def transform(path: String, mat: Mat): (Mat, Scale) = {
    val initialHeight = mat.rows
    val initialWidth = mat.cols

    val widthRatio = initialWidth.toDouble / maxWidth.toDouble
    val heightRatio = initialHeight.toDouble / maxHeight.toDouble

    val scale = Math.max(heightRatio, widthRatio)

    val scaledWidth = if (heightRatio > widthRatio) {
      initialWidth.toDouble / heightRatio
    } else {
      maxWidth.toDouble
    }
    val scaledHeight = if (heightRatio > widthRatio) {
      maxHeight.toDouble
    } else {
      initialHeight.toDouble / widthRatio
    }

    if (scale > 1.0) {
      if (log.isTraceEnabled)
        log.trace("scaledWidth: " + scaledWidth + ", scaledHeight: " + scaledHeight)

      val dest = new Mat
      opencv_imgproc.resize(
        mat,
        dest,
        new Size(scaledWidth.toInt, scaledHeight.toInt),
        0,
        0,
        opencv_imgproc.INTER_AREA
      )
      dest -> Scale(1 / scale)
    } else {
      mat -> Scale(1.0)
    }
  }
}
