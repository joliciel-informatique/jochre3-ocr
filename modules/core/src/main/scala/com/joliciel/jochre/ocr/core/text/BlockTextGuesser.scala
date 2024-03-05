package com.joliciel.jochre.ocr.core.text

import com.joliciel.jochre.ocr.core.alto.ImageToAltoConverter
import com.joliciel.jochre.ocr.core.graphics.BlockSorter
import com.joliciel.jochre.ocr.core.model.{Block, Illustration, Page}
import com.joliciel.jochre.ocr.core.utils.{ImageUtils, OutputLocation, StringUtils}
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import zio.{Task, ZIO, ZLayer}

object BlockTextGuesserService {
  val live: ZLayer[ImageToAltoConverter, Nothing, TextGuesserService] = ZLayer.fromFunction(BlockTextGuesserServiceImpl(_))
}

private[text] case class BlockTextGuesserServiceImpl(imageToAltoConverter: ImageToAltoConverter) extends TextGuesserService {
  def getTextGuesser(): Task[BlockTextGuesser] = {
    ZIO.attempt(new BlockTextGuesser(imageToAltoConverter))
  }
}

/**
 * A text guesser which applies guessing to a entire pre-segmented blocks, with no additional segmentation inside them.
 */
private[text] class BlockTextGuesser(imageToAltoConverter: ImageToAltoConverter) extends TextGuesser with ImageUtils {
  private val log = LoggerFactory.getLogger(getClass)

  private val language = ConfigFactory.load().getConfig("jochre.ocr").getString("language")
  private val leftToRight = StringUtils.isLeftToRight(language)

  /**
   * Given an image and a pre-segmented [[Page]] structure, attempt to guess the text within the page
   * by assigning content to the resulting page.
   * Assumes the page and image are unrotated and have the same scale, which is the desired scale for analysis.
   */
  override def guess(page: Page, mat: Mat, fileName: String, debugLocation: Option[OutputLocation]): Task[Page] = {
    for {
      // convert existing blocks to image segments
      segments <- ZIO.attempt {
          val imageSegmentExtractor = ImageSegmentExtractor(mat, page.blocks, debugLocation)
          imageSegmentExtractor.segments
        }
      pageWithContent <- ZIO.foreach(segments.zipWithIndex){
        case (TextSegment(block, subImage), i) =>
          // Analyze OCR on each text segment and extract the analyzed blocks
          log.debug(f"About to perform OCR analysis for text segment $block")
          debugLocation.foreach { outputLocation =>
            saveImage(fromBufferedImage(subImage), outputLocation.resolve(f"_textblock$i.png"))
          }

          imageToAltoConverter.analyze(subImage).map { altoXml =>
            val jochreSubImage = Page.fromXML(altoXml)
            val translatedSubImage = jochreSubImage.translate(block.left, block.top)
            log.debug(f"OCR analysis complete for $block")
            translatedSubImage.blocks
          }.catchSome{
            case _: AnalysisExceptionToIgnore =>
              ZIO.succeed(Seq.empty[Block])
          }
        case (IllustrationSegment(block), _) =>
          ZIO.succeed(Seq(Illustration(block)))
      }.mapAttempt{ blocks =>
        val sortedBlocks = BlockSorter.sort(blocks.flatten, leftToRight)
          .collect{
            case b: Block => b
          }
        log.debug(f"Found ${sortedBlocks.size} blocks")

        page.copy(
          blocks = sortedBlocks
        ).withCleanIds.withDefaultLanguage
      }
    } yield pageWithContent
  }
}
