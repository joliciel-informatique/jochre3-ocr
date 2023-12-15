package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.model.ImageLabel.{PredictedRectangle, Rectangle}
import com.joliciel.jochre.ocr.core.model.{Illustration, Page, TextBlock}
import com.joliciel.jochre.ocr.core.utils.OutputLocation
import org.bytedeco.opencv.opencv_core.Mat
import sttp.client3.httpclient.zio.SttpClient
import zio.{&, Task, ZIO, ZLayer}

object BlockOnlySegmenterService {
  val live: ZLayer[SttpClient & BlockPredictorService, Nothing, SegmenterService] = ZLayer.fromFunction(BlockOnlySegmenterServiceImpl(_))
}

private[segmentation] case class BlockOnlySegmenterServiceImpl(blockPredictorService: BlockPredictorService) extends SegmenterService {
  def getSegmenter(): Task[BlockOnlySegmenter] = {
    ZIO.attempt(new BlockOnlySegmenter(blockPredictorService))
  }
}

/**
 * Given an image, creates a page with top-level blocks only (text blocks, illustrations).
 */
private[segmentation] class BlockOnlySegmenter(blockPredictorService: BlockPredictorService) extends Segmenter {
  override def segment(mat: Mat, fileName: String, outputLocation: Option[OutputLocation]): Task[Page] = {
    for {
      blockPredictor <- blockPredictorService.getBlockPredictor(mat, fileName, outputLocation)
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