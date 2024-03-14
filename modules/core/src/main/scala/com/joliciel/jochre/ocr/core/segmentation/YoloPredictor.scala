package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.graphics.{PredictedRectangle, Rectangle}
import com.joliciel.jochre.ocr.core.transform.ResizeImageAndKeepAspectRatio
import com.joliciel.jochre.ocr.core.utils.{ImageUtils, OutputLocation}
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.opencv_core.{AbstractScalar, Mat, Point}
import org.slf4j.LoggerFactory
import sttp.capabilities
import sttp.capabilities.zio.ZioStreams
import sttp.client3.circe._
import sttp.client3.httpclient.zio.SttpClient
import sttp.client3.{SttpBackend, UriContext, basicRequest, multipart}
import sttp.model.StatusCode
import zio.{Task, ZIO, ZLayer}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import javax.imageio.ImageIO
import scala.jdk.DurationConverters._

trait YoloPredictorService {
  def getYoloPredictor: Task[YoloPredictor]
}

private[segmentation] case class YoloPredictorServiceImpl(
    httpClient: SttpBackend[Task, ZioStreams with capabilities.WebSockets]
) extends YoloPredictorService {
  def getYoloPredictor: Task[YoloPredictor] =
    ZIO.attempt(
      new YoloPredictorImpl(
        httpClient
      )
    )
}

trait YoloPredictor {
  def predict(
      predictionType: YoloPredictionType,
      mat: Mat,
      fileName: String,
      outputLocation: Option[OutputLocation] = None,
      minConfidence: Option[Double] = None
  ): Task[Seq[PredictedRectangle]]
}

private[segmentation] class YoloPredictorImpl(
    httpClient: SttpBackend[Task, ZioStreams with capabilities.WebSockets]
) extends YoloPredictor
    with ImageUtils {
  private val log = LoggerFactory.getLogger(getClass)

  private val config = ConfigFactory.load().getConfig("jochre.ocr.yolo")
  private val documentLayoutAnalysisUrl = config.getString("url")
  private val requestTimeout = config.getDuration("request-timeout").toScala

  def predict(
      predictionType: YoloPredictionType,
      mat: Mat,
      fileName: String,
      outputLocation: Option[OutputLocation] = None,
      minConfidence: Option[Double] = None
  ): Task[Seq[PredictedRectangle]] = {
    import YoloImplicits._
    val resizer = new ResizeImageAndKeepAspectRatio(predictionType.maxWidth, predictionType.maxHeight)

    for {
      imageAndScale <- ZIO.attempt {
        val (resized, scale) = resizer.transform(fileName, mat)
        toBufferedImage(resized) -> scale
      }
      image = imageAndScale._1
      scale = imageAndScale._2
      request <- ZIO.attempt {
        val out = new ByteArrayOutputStream()
        ImageIO.write(image, "png", out)
        val in = new ByteArrayInputStream(out.toByteArray)
        val myMinConfidence = minConfidence.getOrElse(predictionType.defaultMinConfidence)

        val uri =
          uri"$documentLayoutAnalysisUrl/${predictionType.endpoint}?min-confidence=$myMinConfidence"
        if (log.isDebugEnabled) log.debug(f"Uri: $uri")
        val request = basicRequest
          .readTimeout(requestTimeout)
          .post(uri)
          .multipartBody(
            multipart("imageFile", in).fileName(fileName)
          )
          .response(asJson[List[YoloResult]])

        if (log.isDebugEnabled())
          log.debug(
            f"Predicting YOLO ${predictionType.entryName} for $fileName"
          )
        request
      }
      response <- httpClient.send(request)
      predictions <- ZIO
        .attempt {
          response.code match {
            case StatusCode.Ok =>
              response.body match {
                case Left(exception) =>
                  throw new Exception(f"Got error $exception")
                case Right(yoloResults) =>
                  yoloResults.map { case yoloResult @ YoloResult(_, category, _) =>
                    val label = predictionType.getLabel(category)
                    yoloResult.toPredictedRectangle(label)
                  }
              }
            case statusCode =>
              throw new Exception(
                s"Could not predict ${predictionType.entryName}. Status code: ${statusCode.code}. Status text: ${response.statusText}"
              )
          }
        }
        .mapAttempt {
          _.map(predictedRectangle =>
            predictedRectangle
              .copy(rectangle = predictedRectangle.rectangle.rescale(1.0 / scale.value))
          )
        }
      _ <- ZIO.attempt {
        predictions.zipWithIndex.foreach { case (imageLabel, i) =>
          log.debug(s"Label $i: $imageLabel")
        }

        outputLocation.foreach { outputLocation =>
          val labelled: Mat = toRGB(mat.clone())

          predictions.foreach {
            case PredictedRectangle(
                  _,
                  Rectangle(left, top, width, height),
                  confidence
                ) =>
              opencv_imgproc.rectangle(
                labelled,
                new Point(left, top),
                new Point(left + width, top + height),
                AbstractScalar.RED
              )
              // opencv_imgproc.putText(labelled, label, new Point(left + width, top + height + 40), opencv_imgproc.FONT_HERSHEY_DUPLEX, 1, AbstractScalar.GREEN)
              val confidenceForPrint = (confidence * 100).toInt
              opencv_imgproc.putText(
                labelled,
                f"$confidenceForPrint",
                new Point(left + 2, top + 20),
                opencv_imgproc.FONT_HERSHEY_DUPLEX,
                0.5,
                AbstractScalar.BLACK
              )
          }

          saveImage(labelled, outputLocation.resolve(predictionType.extension))
        }
      }
    } yield predictions
  }
}

object YoloPredictorService {
  val live: ZLayer[SttpClient, Nothing, YoloPredictorService] =
    ZLayer.fromFunction(YoloPredictorServiceImpl(_))
}
