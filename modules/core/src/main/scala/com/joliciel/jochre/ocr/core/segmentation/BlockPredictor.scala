package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.utils.OutputLocation
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import sttp.capabilities
import sttp.capabilities.zio.ZioStreams
import sttp.client3.httpclient.zio.SttpClient
import sttp.client3.{SttpBackend, UriContext, asByteArray, basicRequest, multipart}
import sttp.model.StatusCode
import zio.{Task, ZIO, ZLayer}

import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import javax.imageio.ImageIO

trait BlockPredictorService {
  def getBlockPredictor(mat: Mat, fileName: String, outputLocation: Option[OutputLocation] = None): Task[BlockPredictor]
}

case class BlockPredictorServiceImpl(httpClient: SttpBackend[Task, ZioStreams with capabilities.WebSockets]) extends BlockPredictorService {
  def getBlockPredictor(mat: Mat, fileName: String, outputLocation: Option[OutputLocation] = None): Task[BlockPredictor] =
    ZIO.attempt(new BlockPredictor(httpClient, mat, fileName, outputLocation))
}

class BlockPredictor(httpClient: SttpBackend[Task, ZioStreams with capabilities.WebSockets], override val mat: Mat, override val fileName: String, override val outputLocation: Option[OutputLocation] = None) extends SegmentationPredictor[Rectangle] {
  private val log = LoggerFactory.getLogger(getClass)

  private val config = ConfigFactory.load().getConfig("jochre.ocr.block-predictor")
  private val documentLayoutAnalysisUrl = config.getString("url")
  override val detector: ImageLabelDetector[Rectangle] = new BoxDetector(outputLocation = outputLocation)

  override val extension: String = "_block_prediction.png"

  override def predictor: BufferedImage => Task[Map[String, BufferedImage]] = { image =>

    for {
      request <- ZIO.attempt{
        val out = new ByteArrayOutputStream()
        ImageIO.write(image, "png", out)
        val in = new ByteArrayInputStream(out.toByteArray)

        val request = basicRequest
          .post(uri"${documentLayoutAnalysisUrl}/analyze")
          .multipartBody(
            multipart("imageFile", in).fileName(fileName)
          )
          .response(asByteArray)

        if (log.isDebugEnabled()) log.debug(f"Predicting blocks for $fileName")
        request
      }
      response <- httpClient.send(request)
      labelMap <- ZIO.attempt{
        response.code match {
          case StatusCode.Ok =>
            response.body match {
              case Left(exception) => throw new Exception(f"Got error $exception")
              case Right(byteArray) =>
                val is = new ByteArrayInputStream(byteArray)
                val predictedImage = ImageIO.read(is)
                val imageHeight = image.getHeight
                val imageWidth = image.getWidth
                val subImages = (0 until 4).map { i =>
                  predictedImage.getSubimage(0, i * imageHeight, imageWidth, imageHeight)
                }
                val labels = BlockType.values.map(_.entryName)
                labels.zip(subImages).toMap
            }
          case statusCode =>
            throw new Exception(s"Could not predict blocks. Status code: ${statusCode.code}. Status text: ${response.statusText}")
        }
      }
    } yield labelMap
  }
}

object BlockPredictorService {
  val live: ZLayer[SttpClient, Nothing, BlockPredictorService] = ZLayer.fromFunction(BlockPredictorServiceImpl(_))
}