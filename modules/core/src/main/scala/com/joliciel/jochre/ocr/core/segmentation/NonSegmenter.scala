package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.model.ImageLabel.{PredictedRectangle, Rectangle}
import com.joliciel.jochre.ocr.core.model.{Illustration, Page, TextBlock}
import com.joliciel.jochre.ocr.core.utils.OutputLocation
import org.bytedeco.opencv.opencv_core.Mat
import zio.{Task, ZIO, ZLayer}

object NonSegmenterService {
  val live: ZLayer[Any, Nothing, SegmenterService] = ZLayer.succeed(NonSegmenterServiceImpl)
}

private[segmentation] object NonSegmenterServiceImpl extends SegmenterService {
  def getSegmenter(): Task[Segmenter] = {
    ZIO.succeed(NonSegmenter)
  }
}

/**
 * Given an image, creates an empty page.
 */
private[segmentation] object NonSegmenter extends Segmenter {
  override def segment(mat: Mat, fileName: String, debugLocation: Option[OutputLocation]): Task[Page] = {
    for {
      page <- ZIO.attempt{
        Page(
          id = fileName,
          height = mat.rows(),
          width = mat.cols(),
          physicalPageNumber = 1,
          rotation = 0,
          language = "yi",
          confidence = 1.0,
          blocks = Seq.empty
        )
      }
    } yield page
  }
}