package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.model.ImageLabel.{PredictedRectangle, Rectangle}
import com.joliciel.jochre.ocr.core.model.{Illustration, Page, TextBlock}
import com.joliciel.jochre.ocr.core.utils.OutputLocation
import org.bytedeco.opencv.opencv_core.Mat
import zio.{Task, ZIO, ZLayer}

object BlockOnlySegmenterService {
  val live: ZLayer[YoloPredictorService, Nothing, SegmenterService] = ZLayer.fromFunction(BlockOnlySegmenterServiceImpl(_))
}

private[segmentation] case class BlockOnlySegmenterServiceImpl(yoloPredictorService: YoloPredictorService) extends SegmenterService {
  def getSegmenter(): Task[BlockOnlySegmenter] = {
    ZIO.attempt(new BlockOnlySegmenter(yoloPredictorService))
  }
}

/**
 * Given an image, creates a page with top-level blocks only (text blocks, illustrations).
 */
private[segmentation] class BlockOnlySegmenter(yoloPredictorService: YoloPredictorService) extends Segmenter {
  override def segment(mat: Mat, fileName: String, debugLocation: Option[OutputLocation]): Task[Page] = {
    for {
      blockPredictor <- yoloPredictorService.getYoloPredictor(YoloPredictionType.Blocks, mat, fileName, debugLocation)
      annotations <- blockPredictor.predict()
      page <- ZIO.attempt{
        val blocks = annotations.flatMap{
          case PredictedRectangle(rect@Rectangle(label, _, _, _, _), _) =>
            val blockType = BlockType.withName(label)
            blockType match {
              case BlockType.TextBox => Some(TextBlock(rect, Seq.empty))
              case BlockType.Paragraph => Some(TextBlock(rect, Seq.empty))
              case BlockType.Image => Some(Illustration(rect))
              case BlockType.Table => None
            }
        }
        Page(
          id = fileName,
          height = mat.rows(),
          width = mat.cols(),
          physicalPageNumber = 1,
          rotation = 0,
          language = "yi",
          confidence = 1.0,
          blocks = blocks
        )
      }
    } yield page
  }
}