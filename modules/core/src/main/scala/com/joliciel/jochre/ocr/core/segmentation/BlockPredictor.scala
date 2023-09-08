package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.utils.OutputLocation
import org.bytedeco.opencv.opencv_core.Mat
import sttp.client3.{SimpleHttpClient, UriContext, asByteArray, basicRequest, multipart}

import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, PipedInputStream, PipedOutputStream}
import javax.imageio.ImageIO
import scala.concurrent.Future

case class BlockPredictor(override val mat: Mat, override val fileName: String, override val outputLocation: Option[OutputLocation] = None) extends SegmentationPredictor[Rectangle] {
  override val detector: ImageLabelDetector[Rectangle] = new BoxDetector(outputLocation = outputLocation)

  override val extension: String = "_block_prediction.png"

  import scala.concurrent.ExecutionContext.Implicits.global
  override def predictor: BufferedImage => Map[String, BufferedImage] = { image =>
    val out = new PipedOutputStream()
    val in = new PipedInputStream(out)

    Future {
      try {
        ImageIO.write(image, "png", out)
        out.flush()
      } finally {
        out.close()
      }
    }

    val client = SimpleHttpClient()
    val request = basicRequest
      .post(uri"http://localhost:8444/analyze")
      .multipartBody(
        multipart("imageFile", in).fileName(fileName)
      )
      .response(asByteArray)

    val response = client.send(request)
    response.body match {
      case Left(exception) => throw new Exception(f"Got error $exception")
      case Right(byteArray) =>
        val is = new ByteArrayInputStream(byteArray)
        val predictedImage = ImageIO.read(is)
        val imageHeight = image.getHeight
        val imageWidth = image.getWidth
        val subImages = (0 until 4).map{ i =>
          predictedImage.getSubimage(0, i * imageHeight, imageWidth, imageHeight)
        }
        val labels = Seq("paragraph", "textbox", "image", "table")
        labels.zip(subImages).toMap
    }
  }
}
