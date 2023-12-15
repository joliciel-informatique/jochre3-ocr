package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.model.ImageLabel.{PredictedRectangle, Rectangle}
import com.joliciel.jochre.ocr.core.transform.ResizeImageAndKeepAspectRatio
import com.joliciel.jochre.ocr.core.utils.OutputLocation
import com.typesafe.config.ConfigFactory
import io.circe.Decoder
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import sttp.capabilities
import sttp.capabilities.zio.ZioStreams
import sttp.client3.httpclient.zio.SttpClient
import sttp.client3.circe._
import sttp.client3.{SttpBackend, UriContext, asByteArray, basicRequest, multipart}
import sttp.model.StatusCode
import zio.{Scope, Task, ZIO, ZLayer}

import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import javax.imageio.ImageIO

trait BlockPredictorService {
  def getBlockPredictor(mat: Mat, fileName: String, outputLocation: Option[OutputLocation] = None): Task[SegmentationPredictor[PredictedRectangle]]
}

private[segmentation] case class BlockPredictorServiceImpl(httpClient: SttpBackend[Task, ZioStreams with capabilities.WebSockets]) extends BlockPredictorService {
  def getBlockPredictor(mat: Mat, fileName: String, outputLocation: Option[OutputLocation] = None): Task[SegmentationPredictor[PredictedRectangle]] =
    ZIO.attempt(new BlockPredictor(httpClient, mat, fileName, outputLocation))
}

private[segmentation] class BlockPredictor(
  httpClient: SttpBackend[Task, ZioStreams with capabilities.WebSockets],
  override val mat: Mat,
  override val fileName: String,
  override val outputLocation: Option[OutputLocation] = None
) extends SegmentationPredictorBase[PredictedRectangle] {
  private val log = LoggerFactory.getLogger(getClass)

  private val config = ConfigFactory.load().getConfig("jochre.ocr.block-predictor")
  private val documentLayoutAnalysisUrl = config.getString("url")
  private val longerSide: Int = config.getInt("longer-side")

  override val extension: String = "_block_prediction.png"

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

        val request = basicRequest
          .post(uri"${documentLayoutAnalysisUrl}/analyze-blocks")
          .multipartBody(
            multipart("imageFile", in).fileName(fileName)
          )
          .response(asJson[List[YoloResult]])

        if (log.isDebugEnabled()) log.debug(f"Predicting blocks for $fileName")
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
                    val label = BlockType.withYoloName(category).map(_.entryName).getOrElse(throw new Exception(f"Unknown BlockType: $category"))
                    yoloResult.toPredictedRectangle(label)
                }
            }
          case statusCode =>
            throw new Exception(s"Could not predict blocks. Status code: ${statusCode.code}. Status text: ${response.statusText}")
        }
      }.mapAttempt{
        _.sortBy(_.rectangle)
          .map(predictedRectangle =>
            predictedRectangle.copy(rectangle = predictedRectangle.rectangle.rescale(1.0 / scale.value))
          )
      }
    } yield predictions
  }
}

object BlockPredictorService {
  val live: ZLayer[SttpClient, Nothing, BlockPredictorService] = ZLayer.fromFunction(BlockPredictorServiceImpl(_))
}