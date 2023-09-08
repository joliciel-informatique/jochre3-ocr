package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.model.ImageLabel
import com.joliciel.jochre.ocr.core.utils.{OpenCvUtils, OutputLocation}
import org.bytedeco.opencv.global.opencv_imgcodecs.imwrite
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.opencv_core.{AbstractScalar, Mat, Point}
import org.slf4j.LoggerFactory

import java.awt.image.BufferedImage
import java.io.FileOutputStream
import javax.imageio.ImageIO

trait SegmentationPredictor[T <: ImageLabel] extends OpenCvUtils {
  def mat: Mat
  def fileName: String
  def outputLocation: Option[OutputLocation]
  def detector: ImageLabelDetector[T]
  def extension: String
  def predictor: BufferedImage => Map[String, BufferedImage]

  private val log = LoggerFactory.getLogger(getClass)

  def predict(): Seq[T] = {
    val bufferedImage = toBufferedImage(mat)

    val prediction = predictor(bufferedImage)

    val imageLabels = prediction.view
      .map { case (label, image) =>
        val mat = fromBufferedImage(image)
        outputLocation.foreach{ outputLocation =>
          ImageIO.write(image, "png", new FileOutputStream(outputLocation.resolve(f"_${label}${extension}").toFile))
          imwrite(outputLocation.resolve(f"_${label}_mat.png").toString, mat)
        }
        val detected = detector.detect(mat, label)
        detected
      }.toSeq
      .flatten

    imageLabels.zipWithIndex.foreach { case (imageLabel, i) => log.debug(s"Label $i: ${imageLabel}") }

    outputLocation.foreach { outputLocation =>
      val newMat: Mat = greyscaleToColor(mat.clone())

      imageLabels.foreach {
        case ImageLabel.Rectangle(label, left, top, width, height) =>
          opencv_imgproc.rectangle(newMat, new Point(left, top), new Point(left + width, top + height), AbstractScalar.RED)
          opencv_imgproc.putText(newMat, label, new Point(left + width + 2, top + height + 20), opencv_imgproc.FONT_HERSHEY_DUPLEX, 3, AbstractScalar.GREEN)
        case ImageLabel.Line(label, x1, y1, x2, y2) =>
          opencv_imgproc.line(newMat, new Point(x1, y1), new Point(x2, y2), AbstractScalar.RED)
          opencv_imgproc.putText(newMat, label, new Point(x2 + 20, y2 + 5), opencv_imgproc.FONT_HERSHEY_DUPLEX, 1, AbstractScalar.GREEN)
      }

      imwrite(outputLocation.resolve("_labels.png").toString, newMat)
    }
    imageLabels
  }
}
