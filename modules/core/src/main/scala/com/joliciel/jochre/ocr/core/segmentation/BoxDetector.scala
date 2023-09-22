package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.utils.{OpenCvUtils, OutputLocation}
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.global.opencv_imgproc._
import org.bytedeco.opencv.opencv_core.{Mat, MatVector, RotatedRect}

class BoxDetector(thresh: Int = 240, override val outputLocation: Option[OutputLocation] = None) extends ImageLabelDetector[Rectangle] with OpenCvUtils {
  def detect(image: Mat, label: String): Seq[Rectangle] = {
    outputLocation.foreach(outputLocation => saveImage(image, outputLocation.resolve(f"_${label}_box0_image.png").toString))

    val gray = new Mat()
    threshold(image, gray, thresh, 255, THRESH_BINARY)
    outputLocation.foreach(outputLocation => saveImage(gray, outputLocation.resolve(f"_${label}_box1_thresh.png").toString))

    val canny = new Mat()
    Canny(gray, canny, 175.0, 200.0, 3, true)
    outputLocation.foreach(outputLocation => saveImage(canny, outputLocation.resolve(f"_${label}_box4_canny.png").toString))

    val contours: MatVector = new MatVector
    val mode = opencv_imgproc.CV_RETR_EXTERNAL
    val method = opencv_imgproc.CV_CHAIN_APPROX_SIMPLE
    findContours(gray, contours, mode, method)

    val rectangles = (0 until contours.size().toInt).map { i =>
      val contour: Mat = contours.get(i)
      val rect: RotatedRect = minAreaRect(contour)
      Rectangle(label, rect)
    }.sortBy(0 - _.area)

    val noContains = rectangles.zipWithIndex.filterNot { case (rect, i) => rectangles.slice(0, i).exists(_.contains(rect)) }.map(_._1)
    noContains.sorted
  }
}
