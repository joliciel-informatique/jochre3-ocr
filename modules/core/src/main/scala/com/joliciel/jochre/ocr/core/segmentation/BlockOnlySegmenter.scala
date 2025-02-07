package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.graphics.{BlockSorter, PredictedRectangle, Rectangle}
import com.joliciel.jochre.ocr.core.model.{Illustration, Page, TextBlock}
import com.joliciel.jochre.ocr.core.utils.{OutputLocation, StringUtils}
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.opencv_core.Mat
import zio.{Task, ZIO, ZLayer}

object BlockOnlySegmenterService {
  val live: ZLayer[YoloPredictorService, Nothing, SegmenterService] =
    ZLayer.fromFunction(BlockOnlySegmenterServiceImpl(_))
}

private[segmentation] case class BlockOnlySegmenterServiceImpl(
    yoloPredictorService: YoloPredictorService
) extends SegmenterService {
  def getSegmenter: Task[BlockOnlySegmenter] = {
    ZIO.attempt(new BlockOnlySegmenter(yoloPredictorService))
  }
}

/** Given an image, creates a page with top-level blocks only (text blocks, illustrations).
  */
private[segmentation] class BlockOnlySegmenter(
    yoloPredictorService: YoloPredictorService
) extends Segmenter {
  private val language =
    ConfigFactory.load().getConfig("jochre.ocr").getString("language")
  private val leftToRight = StringUtils.isLeftToRight(language)

  override def segment(
      mat: Mat,
      fileName: String,
      debugLocation: Option[OutputLocation],
      testRectangle: Option[Rectangle] = None
  ): Task[Page] = {
    for {
      blockPredictor <- yoloPredictorService.getYoloPredictor
      blockPredictions <- blockPredictor.predict(YoloPredictionType.Blocks, mat, fileName, debugLocation)
      page <- ZIO.attempt {
        val sortedBlockPredictions = BlockSorter
          .sort(blockPredictions, leftToRight)
          .collect { case p: PredictedRectangle =>
            p
          }

        val blocks = sortedBlockPredictions.flatMap { case PredictedRectangle(label, rect, _) =>
          val blockType = BlockType.withName(label)
          blockType match {
            case BlockType.TopLevelTextBlock =>
              Some(TextBlock(rect, Seq.empty))
            case BlockType.Illustration => Some(Illustration(rect))
          }
        }

        Page(
          id = fileName,
          height = mat.rows(),
          width = mat.cols(),
          physicalPageNumber = 1,
          rotation = 0,
          language = language,
          confidence = 1.0,
          blocks = blocks
        ).withCleanIds.withDefaultLanguage
      }
    } yield page
  }
}
