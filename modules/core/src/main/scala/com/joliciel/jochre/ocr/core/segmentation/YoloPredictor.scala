package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.graphics.PredictedRectangle
import com.joliciel.jochre.ocr.core.transform.ResizeImageAndKeepAspectRatio
import com.joliciel.jochre.ocr.core.utils.OutputLocation
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import sttp.capabilities
import sttp.capabilities.zio.ZioStreams
import sttp.client3.circe._
import sttp.client3.httpclient.zio.SttpClient
import sttp.client3.{SttpBackend, UriContext, basicRequest, multipart}
import sttp.model.StatusCode
import zio.{Task, ZIO, ZLayer}

import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import javax.imageio.ImageIO

import scala.jdk.DurationConverters._

trait YoloPredictorService {
  def getYoloPredictor(predictionType: YoloPredictionType, mat: Mat, fileName: String, outputLocation: Option[OutputLocation] = None, minConfidence: Option[Double] = None): Task[SegmentationPredictor]
}

private[segmentation] case class YoloPredictorServiceImpl(httpClient: SttpBackend[Task, ZioStreams with capabilities.WebSockets]) extends YoloPredictorService {
  def getYoloPredictor(predictionType: YoloPredictionType, mat: Mat, fileName: String, outputLocation: Option[OutputLocation] = None, minConfidence: Option[Double] = None): Task[SegmentationPredictor] =
    ZIO.attempt(new YoloPredictor(httpClient, predictionType, mat, fileName, outputLocation, minConfidence))
}

private[segmentation] class YoloPredictor(
  httpClient: SttpBackend[Task, ZioStreams with capabilities.WebSockets],
  predictionType: YoloPredictionType,
  override val mat: Mat,
  override val fileName: String,
  override val outputLocation: Option[OutputLocation] = None,
  minConfidence: Option[Double] = None,
) extends SegmentationPredictorBase {
  private val log = LoggerFactory.getLogger(getClass)

  private val config = ConfigFactory.load().getConfig("jochre.ocr.block-predictor")
  private val documentLayoutAnalysisUrl = config.getString("url")
  private val longerSide: Int = config.getInt("longer-side")
  private val requestTimeout = config.getDuration("request-timeout").toScala

  override val extension: String = predictionType.extension

  import YoloImplicits._

  private val resizer = new ResizeImageAndKeepAspectRatio(longerSide)
  private val (resized, scale) = resizer.transform(fileName, mat)

  def transform(): Mat = resized

  override def predictor: BufferedImage => Task[Seq[PredictedRectangle]] = { image =>
    for {
      request <- ZIO.attempt{
        val out = new ByteArrayOutputStream()
        ImageIO.write(image, "png", out)
        val in = new ByteArrayInputStream(out.toByteArray)

        val uri = uri"$documentLayoutAnalysisUrl/${predictionType.endpoint}?${minConfidence.map(conf => f"min-confidence=$conf").getOrElse("")}"
        if (log.isDebugEnabled) log.debug(f"Uri: $uri")
        val request = basicRequest
          .readTimeout(requestTimeout)
          .post(uri)
          .multipartBody(
            multipart("imageFile", in).fileName(fileName)
          )
          .response(asJson[List[YoloResult]])

        if (log.isDebugEnabled()) log.debug(f"Predicting YOLO ${predictionType.entryName} for $fileName")
        request
      }
      response <- httpClient.send(request)
      predictions <- ZIO.attempt{
        response.code match {
          case StatusCode.Ok =>
            response.body match {
              case Left(exception) => throw new Exception(f"Got error $exception")
              case Right(yoloResults) =>
                yoloResults.map{
                  case yoloResult@YoloResult(_, category, _) =>
                    val label = predictionType.getLabel(category)
                    yoloResult.toPredictedRectangle(label)
                }
            }
          case statusCode =>
            throw new Exception(s"Could not predict ${predictionType.entryName}. Status code: ${statusCode.code}. Status text: ${response.statusText}")
        }
      }.mapAttempt{
        _.map(predictedRectangle =>
          predictedRectangle.copy(rectangle = predictedRectangle.rectangle.rescale(1.0 / scale.value))
        )
      }
    } yield predictions
  }
}

object YoloPredictorService {
  val live: ZLayer[SttpClient, Nothing, YoloPredictorService] = ZLayer.fromFunction(YoloPredictorServiceImpl(_))
}