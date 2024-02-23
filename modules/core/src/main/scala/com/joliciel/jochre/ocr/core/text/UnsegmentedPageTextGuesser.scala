package com.joliciel.jochre.ocr.core.text

import com.joliciel.jochre.ocr.core.alto.ImageToAltoConverter
import com.joliciel.jochre.ocr.core.model.{Block, Illustration, Page}
import com.joliciel.jochre.ocr.core.utils.{ImageUtils, OutputLocation}
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import zio.{Task, ZIO, ZLayer}

object UnsegmentedPageTextGuesserService {
  val live: ZLayer[ImageToAltoConverter, Nothing, TextGuesserService] = ZLayer.fromFunction(UnsegmentedPageTextGuesserServiceImpl(_))
}

private[text] case class UnsegmentedPageTextGuesserServiceImpl(imageToAltoConverter: ImageToAltoConverter) extends TextGuesserService {
  def getTextGuesser(): Task[UnsegmentedPageTextGuesser] = {
    ZIO.attempt(new UnsegmentedPageTextGuesser(imageToAltoConverter))
  }
}

/**
 * A text guesser which applies guessing to an entire page, with no blocks inside it.
 */
private[text] class UnsegmentedPageTextGuesser(imageToAltoConverter: ImageToAltoConverter) extends TextGuesser with ImageUtils {
  private val log = LoggerFactory.getLogger(getClass)

  override def guess(page: Page, mat: Mat, fileName: String, debugLocation: Option[OutputLocation]): Task[Page] = {
    val image = toBufferedImage(mat)
    (for {
      alto <- imageToAltoConverter.analyze(image)
    } yield {
      Page.fromXML(alto).withCleanIds
    }).catchSome {
      case _: AnalysisExceptionToIgnore =>
        ZIO.succeed(page)
    }
  }
}
