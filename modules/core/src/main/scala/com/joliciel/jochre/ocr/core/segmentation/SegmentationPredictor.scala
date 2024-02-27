package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.model.ImageLabel
import com.joliciel.jochre.ocr.core.utils.{ImageUtils, OutputLocation}
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.opencv_core.{AbstractScalar, Mat, Point}
import org.slf4j.LoggerFactory
import zio.{Task, ZIO}

import java.awt.image.BufferedImage

trait SegmentationPredictor[T <: ImageLabel] {
  def predict(): Task[Seq[T]]
}

trait SegmentationPredictorBase[T <: ImageLabel] extends SegmentationPredictor[T] with ImageUtils {
  private[segmentation] def mat: Mat
  private[segmentation] def fileName: String
  private[segmentation] def outputLocation: Option[OutputLocation]
  private[segmentation] def extension: String
  private[segmentation] def predictor: BufferedImage => Task[Seq[T]]

  private val log = LoggerFactory.getLogger(getClass)

  private[segmentation] def transform(): Mat

  def predict(): Task[Seq[T]] = {
    for {
      bufferedImage <- ZIO.attempt{
        val transformed = transform()
        toBufferedImage(transformed)
      }
      predictions <- predictor(bufferedImage)
      _ <- ZIO.attempt{
        predictions.zipWithIndex.foreach { case (imageLabel, i) => log.debug(s"Label $i: ${imageLabel}") }

        outputLocation.foreach { outputLocation =>
          val labelled: Mat = toRGB(mat.clone())

          predictions.foreach {
            case ImageLabel.PredictedRectangle(_, ImageLabel.Rectangle(left, top, width, height), confidence) =>
              opencv_imgproc.rectangle(labelled, new Point(left, top), new Point(left + width, top + height), AbstractScalar.RED)
              //opencv_imgproc.putText(labelled, label, new Point(left + width, top + height + 40), opencv_imgproc.FONT_HERSHEY_DUPLEX, 1, AbstractScalar.GREEN)
              val confidenceForPrint = (confidence * 100).toInt
              opencv_imgproc.putText(labelled, f"$confidenceForPrint", new Point(left + 2, top + 20), opencv_imgproc.FONT_HERSHEY_DUPLEX, 0.5, AbstractScalar.BLACK)
            case ImageLabel.Rectangle(left, top, width, height) =>
              opencv_imgproc.rectangle(labelled, new Point(left, top), new Point(left + width, top + height), AbstractScalar.RED)
              //opencv_imgproc.putText(labelled, label, new Point(left + width + 2, top + height + 20), opencv_imgproc.FONT_HERSHEY_DUPLEX, 3, AbstractScalar.GREEN)
            case ImageLabel.Line(x1, y1, x2, y2) =>
              opencv_imgproc.line(labelled, new Point(x1, y1), new Point(x2, y2), AbstractScalar.RED)
              //opencv_imgproc.putText(labelled, label, new Point(x2 + 20, y2 + 5), opencv_imgproc.FONT_HERSHEY_DUPLEX, 1, AbstractScalar.GREEN)
          }

          saveImage(labelled, outputLocation.resolve(extension))
        }
      }
    } yield predictions
  }
}
