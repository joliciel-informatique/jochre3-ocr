package com.joliciel.jochre.ocr.core.text

import com.joliciel.jochre.ocr.core.analysis.TextAnalyzer
import com.joliciel.jochre.ocr.core.model.{Illustration, Page}
import com.joliciel.jochre.ocr.core.utils.{ImageUtils, OutputLocation}
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import zio.{Task, ZIO, ZLayer}

object BlockTextGuesserService {
  val live: ZLayer[TextAnalyzer, Nothing, TextGuesserService] = ZLayer.fromFunction(BlockTextGuesserServiceImpl(_))
}

private[text] case class BlockTextGuesserServiceImpl(textAnalyzer: TextAnalyzer) extends TextGuesserService {
  def getTextGuesser(): Task[BlockTextGuesser] = {
    ZIO.attempt(new BlockTextGuesser(textAnalyzer))
  }
}

/**
 * A text guesser which applies guessing to a entire pre-segmented blocks, with no additional segmentation inside them.
 */
private[text] class BlockTextGuesser(textAnalyzer: TextAnalyzer) extends TextGuesser with ImageUtils {
  private val log = LoggerFactory.getLogger(getClass)

  /**
   * Given an image and a pre-segmented [[Page]] structure, attempt to guess the text within the page
   * by assigning content to the resulting page.
   * Assumes the page and image are unrotated and have the same scale, which is the desired scale for analysis.
   */
  override def guess(page: Page, mat: Mat, fileName: String, outputLocation: Option[OutputLocation]): Task[Page] = {
    for {
      // convert existing blocks to image segments
      segments <- ZIO.attempt {
          // re-scale coordinates
          val rectangles = page.blocks.map(_.rectangle)
          val imageSegmentExtractor = ImageSegmentExtractor(mat, rectangles, outputLocation)
          imageSegmentExtractor.segments
        }
      pageWithContent <- ZIO.attempt {
        // analyze OCR in text blocks
        val blocks = segments.zipWithIndex.map {
          case (TextSegment(block, subImage), i) =>
            // Analyze OCR on each text segment and extract the analyzed blocks
            log.debug(f"About to perform OCR analysis for text segment $block")
            outputLocation.foreach { outputLocation =>
              saveImage(fromBufferedImage(subImage), outputLocation.resolve(f"_textblock$i.png").toString)
            }

            textAnalyzer.analyze(subImage).map { altoXml =>
              val jochreSubImage = Page.fromXML(altoXml)
              val translatedSubImage = jochreSubImage.translate(block.left, block.top)
              log.debug(f"OCR analysis complete for $block")
              translatedSubImage.blocks
            }.getOrElse(Seq.empty)
          case (IllustrationSegment(block), _) =>
            Seq(Illustration(block))
        }.flatten.sortBy(_.rectangle)

        log.debug(f"Found ${blocks.size} blocks")

        page.copy(
          blocks = blocks
        )
      }
    } yield pageWithContent
  }
}
