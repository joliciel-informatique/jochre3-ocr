package com.joliciel.jochre.ocr.core.utils

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import org.bytedeco.javacv.Java2DFrameUtils
import org.bytedeco.opencv.global.opencv_imgcodecs._
import org.bytedeco.opencv.global.opencv_imgproc._
import org.bytedeco.opencv.global.{opencv_core, opencv_imgproc}
import org.bytedeco.opencv.helper.opencv_core.RGB
import org.bytedeco.opencv.opencv_core.{Mat, MatVector, Point2f, Rect, RotatedRect, Scalar}
import org.slf4j.LoggerFactory

import java.awt.Color
import java.awt.image.BufferedImage
import java.net.URL
import java.nio.file.Path
import javax.imageio.ImageIO
import scala.util.{Try, Using}

trait ImageUtils {
  private val log = LoggerFactory.getLogger(getClass)

  def loadImage(path: Path): Mat =
    imread(path.toFile.getPath, IMREAD_GRAYSCALE)

  def saveImage(mat: Mat, path: Path): Unit =
    imwrite(path.toFile.getPath, mat)

  def rotate(angle: Double, mat: Mat): Mat = {
    val cy = mat.rows.toFloat / 2f
    val cx = mat.cols.toFloat / 2f
    val result = new Mat
    val matrix2D = getRotationMatrix2D(new Point2f(cx, cy), angle, 1)
    warpAffine(
      mat,
      result,
      matrix2D,
      mat.size,
      INTER_LINEAR,
      opencv_core.BORDER_REFLECT,
      new Scalar(0.0)
    )
    result
  }

  def unrotate(angle: Double, mat: Mat): Mat =
    rotate(0 - angle, mat)

  def drawRotatedRect(
      mat: Mat,
      rotatedRect: RotatedRect,
      color: Color = Color.BLACK,
      thickness: Int = 1
  ): Unit = {
    val vertices = new Point2f(4)
    rotatedRect.points(vertices)
    log.debug(f"rotatedRect: (${vertices.get(0)}, ${vertices
      .get(1)}), (${vertices.get(2)}, ${vertices.get(3)}), (${vertices
      .get(4)}, ${vertices.get(5)}), (${vertices.get(6)}, ${vertices.get(7)})")
    this.drawPolygon(mat, vertices, color, thickness)
  }

  private def drawPolygon(
      mat: Mat,
      listOfPoints: Point2f,
      color: Color = Color.BLACK,
      thickness: Int = 1
  ): Unit = {
    val vertexVector = new MatVector(1)
    val matOfInt = new Mat()
    val numPoints = listOfPoints.capacity().toInt
    new Mat(listOfPoints)
      .reshape(1, numPoints)
      .convertTo(matOfInt, opencv_core.CV_32S)
    vertexVector.put(0, matOfInt)
    val openCvColor = RGB(color.getRed, color.getGreen, color.getBlue)
    opencv_imgproc.polylines(
      mat,
      vertexVector,
      true,
      openCvColor,
      thickness,
      LINE_8,
      0
    )
  }

  def toRGB(src: Mat): Mat = {
    val dest = new Mat()
    if (src.`type`() == opencv_core.CV_8UC1) {
      cvtColor(src, dest, opencv_imgproc.CV_GRAY2RGB)
      dest
    } else {
      src
    }
  }

  def toGrayscale(src: Mat): Mat = {
    val dest = new Mat()
    val imageType = src.`type`()
    if (imageType == opencv_core.CV_8UC3) {
      cvtColor(src, dest, opencv_imgproc.CV_RGB2GRAY)
      dest
    } else if (imageType == opencv_core.CV_8UC4) {
      cvtColor(src, dest, opencv_imgproc.CV_RGBA2GRAY)
      dest
    } else {
      src
    }
  }

  def fromBufferedImage(bufferedImage: BufferedImage): Mat = {
    // convert black-and-white to greyscale first
    val imageToConvert =
      if (bufferedImage.getType == BufferedImage.TYPE_BYTE_BINARY) {
        val greyImage = new BufferedImage(
          bufferedImage.getWidth,
          bufferedImage.getHeight,
          BufferedImage.TYPE_BYTE_GRAY
        )
        val graphics = greyImage.getGraphics
        graphics.drawImage(bufferedImage, 0, 0, null)
        graphics.dispose()
        greyImage
      } else {
        bufferedImage
      }
    Java2DFrameUtils.toMat(imageToConvert)
  }

  def toBufferedImage(mat: Mat): BufferedImage = {
    // bufferedImageConverter.convert(cvConverter.convert(mat))
    Java2DFrameUtils.toBufferedImage(mat)
  }

  def crop(src: Mat, rectangle: Rectangle): Mat = {
    val fullRect = Rectangle(0, 0, src.cols(), src.rows())
    val adjustedRect = fullRect.intersection(rectangle).get
    val rect: Rect =
      new Rect(adjustedRect.left, adjustedRect.top, adjustedRect.width, adjustedRect.height)
    val cropped: Mat = new Mat(src, rect)
    cropped
  }

  def getImageFromUrl(urlStr: String): Try[BufferedImage] = {
    Using.Manager { use =>
      val stream = use {
        val url = new URL(urlStr)
        url.openStream
      }
      ImageIO.read(stream)
    }
  }

  /** @param src
    *   The source image
    * @param contrast
    *   Alpha value, > 1 to increase contrast, < 1 to decrease contrast
    * @param brightness
    *   Beta value between -255 (to darken) to +255 (to lighten)
    * @return
    *   The converted image
    */
  def changeContrastAndBrightness(
      src: Mat,
      contrast: Double,
      brightness: Int
  ): Mat = {
    val dest = new Mat()
    src.convertTo(dest, -1, contrast, brightness.toDouble)
    dest
  }
}

object ImageUtils extends ImageUtils
