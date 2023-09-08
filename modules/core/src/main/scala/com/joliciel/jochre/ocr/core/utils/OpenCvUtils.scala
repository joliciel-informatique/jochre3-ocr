package com.joliciel.jochre.ocr.core.utils

import org.bytedeco.javacv.{Java2DFrameConverter, Java2DFrameUtils, OpenCVFrameConverter}
import org.bytedeco.opencv.global.opencv_imgproc.{LINE_8, cvtColor}
import org.bytedeco.opencv.global.{opencv_core, opencv_imgproc}
import org.bytedeco.opencv.helper.opencv_core.RGB
import org.bytedeco.opencv.opencv_core.{IplImage, Mat, MatVector, Point2f, RotatedRect}
import org.slf4j.LoggerFactory

import java.awt.Color
import java.awt.image.BufferedImage

trait OpenCvUtils {
  private val log = LoggerFactory.getLogger(getClass)

  def drawRotatedRect(mat: Mat, rotatedRect: RotatedRect, color: Color = Color.BLACK, thickness: Int = 1): Unit = {
    val vertices = new Point2f(4)
    rotatedRect.points(vertices)
    log.debug(f"rotatedRect: (${vertices.get(0)}, ${vertices.get(1)}), (${vertices.get(2)}, ${vertices.get(3)}), (${vertices.get(4)}, ${vertices.get(5)}), (${vertices.get(6)}, ${vertices.get(7)})")
    this.drawPolygon(mat, vertices, color, thickness)
  }

  def drawPolygon(mat: Mat, listOfPoints: Point2f, color: Color = Color.BLACK, thickness: Int = 1): Unit = {
    val vertexVector = new MatVector(1)
    val matOfInt = new Mat()
    val numPoints = listOfPoints.capacity().toInt
    (new Mat(listOfPoints)).reshape(1, numPoints).convertTo(matOfInt, opencv_core.CV_32S)
    vertexVector.put(0, matOfInt)
    val openCvColor = RGB(color.getRed, color.getGreen, color.getBlue)
    opencv_imgproc.polylines(mat, vertexVector, true, openCvColor, thickness, LINE_8, 0)
  }

  def greyscaleToColor(src: Mat): Mat = {
    val dest = new Mat()
    if (src.`type`()==opencv_core.CV_8UC1) {
      cvtColor(src, dest, opencv_imgproc.CV_GRAY2RGB)
      dest
    } else {
      src
    }
  }

  def fromBufferedImage(bufferedImage: BufferedImage): Mat = {
    // convert black-and-white to greyscale first
    val imageToConvert = if (bufferedImage.getType==BufferedImage.TYPE_BYTE_BINARY) {
      val greyImage = new BufferedImage(bufferedImage.getWidth, bufferedImage.getHeight, BufferedImage.TYPE_BYTE_GRAY);
      val graphics = greyImage.getGraphics();
      graphics.drawImage(bufferedImage, 0, 0, null);
      graphics.dispose();
      greyImage
    } else {
      bufferedImage
    }
    Java2DFrameUtils.toMat(imageToConvert)
  }

  def toBufferedImage(mat: Mat): BufferedImage = {
    //bufferedImageConverter.convert(cvConverter.convert(mat))
    Java2DFrameUtils.toBufferedImage(mat)
  }
}
