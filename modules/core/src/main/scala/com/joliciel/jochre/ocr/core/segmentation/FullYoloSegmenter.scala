package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.graphics.{BlockSorter, Line, PredictedRectangle, Rectangle, WithRectangle}
import com.joliciel.jochre.ocr.core.model.{
  Block,
  ComposedBlock,
  Glyph,
  Illustration,
  Page,
  Space,
  TextBlock,
  TextLine,
  Word
}
import com.joliciel.jochre.ocr.core.utils.{ImageUtils, MathUtils, OutputLocation, StringUtils}
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import zio.{Task, ZIO, ZLayer}

import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.time.Instant
import scala.annotation.tailrec
import scala.util.Using

object FullYoloSegmenterService {
  val live: ZLayer[YoloPredictorService, Nothing, SegmenterService] =
    ZLayer.fromFunction(FullYoloSegmenterServiceImpl(_))
}

private[segmentation] case class FullYoloSegmenterServiceImpl(
    yoloPredictorService: YoloPredictorService
) extends SegmenterService {
  def getSegmenter: Task[FullYoloSegmenter] = {
    ZIO.attempt(new FullYoloSegmenter(yoloPredictorService))
  }
}

private[segmentation] class FullYoloSegmenter(
    yoloPredictorService: YoloPredictorService
) extends Segmenter
    with ImageUtils {
  private val log = LoggerFactory.getLogger(getClass)

  private val config = ConfigFactory.load().getConfig("jochre.ocr.yolo")
  private val cropToPrintArea = config.getBoolean("crop-to-print-area")
  private val cropMargin = config.getDouble("crop-margin")
  private val glyphImageTileCount = config.getInt("glyph-image-tile-count")
  private val tileMargin = config.getDouble("tile-margin")
  private val alwaysRetainBlockThreshold =
    config.getDouble("always-retain-block-threshold")

  private val language =
    ConfigFactory.load().getConfig("jochre.ocr").getString("language")
  private val leftToRight = StringUtils.isLeftToRight(language)

  /** Transform an image into a segmented [[Page]] structure. The page might only be segmented down to a given level
    * (e.g. blocks, lines, strings, or glyphs)
    */
  override def segment(
      mat: Mat,
      fileName: String,
      debugLocation: Option[OutputLocation],
      testRectangle: Option[Rectangle] = None
  ): Task[Page] = {
    val startTime = Instant.now
    for {
      yoloPredictor <- yoloPredictorService.getYoloPredictor
      blockPredictions <- yoloPredictor.predict(YoloPredictionType.Blocks, mat, fileName, debugLocation)
      pageWithBlocks <- ZIO.attempt {
        val textBlockPredictions =
          blockPredictions.filter(p => BlockType.withName(p.label).isText)
        val imageBlockPredictions =
          blockPredictions.filterNot(p => BlockType.withName(p.label).isText)
        val sortedBlockPredictions = BlockSorter
          .sort(textBlockPredictions, leftToRight)
          .collect { case p: PredictedRectangle =>
            p
          }

        // For now, we simply remove overlaps
        val withoutOverlaps = removeOverlapsUnordered(sortedBlockPredictions)
        val allPredictions = withoutOverlaps ++ imageBlockPredictions

        val blocks = allPredictions.flatMap { case PredictedRectangle(label, rect, _) =>
          val blockType = BlockType.withName(label)
          blockType match {
            case BlockType.TopLevelTextBlock =>
              Some(TextBlock(rect, Seq.empty))
            case BlockType.Illustration => Some(Illustration(rect))
          }
        }

        log.info(
          f"Predicted ${blocks.size} blocks for $fileName, of which ${withoutOverlaps.size} text blocks and ${imageBlockPredictions.size} illustrations."
        )

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
      croppedPrintArea <- ZIO.attempt {
        if (cropToPrintArea) {
          pageWithBlocks.croppedPrintArea(cropMargin)
        } else {
          pageWithBlocks.rectangle
        }
      }
      printAreaMat <- ZIO.attempt {
        if (cropToPrintArea) {
          crop(mat, croppedPrintArea)
        } else {
          mat
        }
      }
      croppedRectangle = Rectangle(
        0,
        0,
        printAreaMat.cols(),
        printAreaMat.rows()
      )
      paragraphPredictions <- yoloPredictor.predict(
        YoloPredictionType.TextBlocks,
        printAreaMat,
        fileName,
        debugLocation
      )
      linePredictions <- yoloPredictor.predict(
        YoloPredictionType.Lines,
        printAreaMat,
        fileName,
        debugLocation
      )
      wordPredictions <- yoloPredictor.predict(
        YoloPredictionType.Words,
        printAreaMat,
        fileName,
        debugLocation
      )
      glyphPredictions <- {
        log.info(f"Predicted ${paragraphPredictions.size} paragraphs for $fileName")
        log.info(f"Predicted ${linePredictions.size} lines for $fileName")
        log.info(f"Predicted ${wordPredictions.size} words for $fileName")

        // Get glyph predictions for overlapping tiles
        // The assumption is that if the same glyph is predicted by two tiles, one of the two will be eliminated downstream
        val tiles = croppedRectangle.tile(
          glyphImageTileCount,
          glyphImageTileCount,
          tileMargin
        )
        ZIO
          .foreach(tiles) { tile =>
            val tileMat = crop(printAreaMat, tile)
            for {
              glyphPredictions <- yoloPredictor.predict(
                YoloPredictionType.Glyphs,
                tileMat,
                fileName,
                debugLocation
              )
            } yield {
              log.info(
                f"Predicted ${glyphPredictions.size} glyphs for $fileName, tile ${tile.coordinates}"
              )
              glyphPredictions.map { prediction =>
                prediction.copy(rectangle = prediction.rectangle.translate(tile.left, tile.top))
              }
            }
          }
          .map(_.flatten)
      }
      page <- ZIO.attempt {
        val predictionDuration =
          (Instant.now.toEpochMilli - startTime.toEpochMilli).toDouble / 1000.0

        log.info(f"Predicted ${glyphPredictions.size} glyphs for $fileName")
        log.info(
          f"Finished predicting segments for page $fileName in $predictionDuration%.2f seconds"
        )

        val textBlocks = pageWithBlocks.textBlocks
        val textBlocksToConsider = testRectangle
          .map(testRect =>
            textBlocks.filter { block =>
              block.rectangle.areaOfIntersection(
                testRect
              ) / block.rectangle.area.toDouble > 0
            }
          )
          .getOrElse(textBlocks)

        val illustrations = pageWithBlocks.illustrations

        // Place lines inside blocks
        val translatedLinePredictions = linePredictions.map(p =>
          p.copy(rectangle = p.rectangle.translate(croppedPrintArea.left, croppedPrintArea.top))
        )
        // lines are rectangles predicted vertically centered around the baseline - we want to move them upwards to place them as much as possible inside the text box
        val linesBumpedUp = translatedLinePredictions
          .filter(line => line.rectangle.bottom <= pageWithBlocks.height)
          .map(p => p.copy(rectangle = p.rectangle.translate(0, 0 - (p.rectangle.height / 2))))
        val linesToPlace = testRectangle
          .map(testRect =>
            linesBumpedUp.filter(line =>
              line.rectangle
                .areaOfIntersection(testRect) / line.rectangle.area.toDouble > 0
            )
          )
          .getOrElse(linesBumpedUp)

        val textBlockToLineMap = placeRectanglesInTextBlocks(
          textBlocksToConsider,
          linesToPlace,
          "TextLine",
          minIntersection = 0.01,
          splitHorizontally = true
        )

        val textBlocksWithLines = textBlocksToConsider.map { textBlock =>
          val myLineRects = textBlockToLineMap
            .getOrElse(textBlock, Seq.empty)
            .sorted(WithRectangle.VerticalOrdering())
            .map(lineRect =>
              lineRect.copy(rectangle =
                lineRect.rectangle.copy(left = textBlock.rectangle.left, width = textBlock.rectangle.width)
              )
            )

          val myLineRectsWithoutOverlaps = removeOverlaps(myLineRects)
          val myLines = myLineRectsWithoutOverlaps.map(lineRect =>
            TextLine(
              Line(
                textBlock.rectangle.left,
                lineRect.rectangle.bottom,
                textBlock.rectangle.right,
                lineRect.rectangle.bottom
              ),
              Seq.empty
            )
          )
          textBlock.copy(textLines = myLines)
        }

        // Place words inside blocks
        val translatedWordPredictions = wordPredictions
          .map(p => p.copy(rectangle = p.rectangle.translate(croppedPrintArea.left, croppedPrintArea.top)))
          // Avoid words that overlap the bottom of the page
          .filter(word => word.rectangle.bottom < pageWithBlocks.height - 1)

        val wordsToPlace = testRectangle
          .map(testRect =>
            translatedWordPredictions.filter(word =>
              word.rectangle.areaOfIntersection(
                testRect
              ) / word.rectangle.area.toDouble > 0.8
            )
          )
          .getOrElse(translatedWordPredictions)
        val textBlockToWordMap =
          placeRectanglesInTextBlocks(textBlocksWithLines, wordsToPlace, "Word")

        val textBlocksWithWords = textBlocksWithLines.map { textBlock =>
          val wordsToInclude =
            textBlockToWordMap.getOrElse(textBlock, Seq.empty)
          val textLineToWordMap =
            placeRectanglesInTextLines(textBlock, wordsToInclude, "Word")

          textBlock.copy(textLines = textBlock.textLines.map { textLine =>
            val myWordRects = textLineToWordMap.getOrElse(textLine, Seq.empty)
            val myWords =
              myWordRects.map(rect => Word("", rect.rectangle, Seq.empty, Seq.empty, 1.0))

            textLine.copy(wordsAndSpaces = myWords)
          })
        }

        // Place glyphs inside words
        val translatedGlyphPredictions = glyphPredictions.map(p =>
          p.copy(rectangle = p.rectangle.translate(croppedPrintArea.left, croppedPrintArea.top))
        )
        val glyphsToPlace = testRectangle
          .map(testRect =>
            translatedGlyphPredictions.filter(glyph =>
              glyph.rectangle.areaOfIntersection(
                testRect
              ) / glyph.rectangle.area.toDouble > 0.8
            )
          )
          .getOrElse(translatedGlyphPredictions)

        val textBlockToGlyphMap = placeRectanglesInTextBlocks(
          textBlocksWithWords,
          glyphsToPlace,
          "Glyph"
        )
        val textBlocksWithGlyphs = textBlocksWithWords
          .map { textBlock =>
            val glyphsToInclude =
              textBlockToGlyphMap.getOrElse(textBlock, Seq.empty)
            val textLineToGlyphMap =
              placeRectanglesInTextLines(textBlock, glyphsToInclude, "Glyph")
            val textLinesWithGlyphs = textBlock.textLines.map { textLine =>
              val textLineGlyphs =
                textLineToGlyphMap.getOrElse(textLine, Seq.empty)
              if (log.isDebugEnabled)
                log.debug(
                  f"In TextLine ${textLine.baseLine} with ${textLine.words.size} words, found glyphs: ${textLineGlyphs
                    .map(_.rectangle.coordinates)
                    .mkString(", ")}"
                )
              if (textLineGlyphs.nonEmpty) {
                val wordToGlyphMap =
                  placeRectanglesInWords(textLine, textLineGlyphs)
                val nonEmptyWords = textLine.words.flatMap { word =>
                  val myGlyphRects = wordToGlyphMap.getOrElse(word, Seq.empty)
                  Option.when(myGlyphRects.nonEmpty) {
                    // Take average border between two sequential glyphs as their border
                    val borders =
                      myGlyphRects.zip(myGlyphRects.tail).map { case (firstRect, secondRect) =>
                        (firstRect.rectangle.left + secondRect.rectangle.right) / 2
                      }
                    val rightLeftPairs =
                      (myGlyphRects.head.rectangle.right +: borders)
                        .zip(borders :+ myGlyphRects.last.rectangle.left)

                    val myGlyphs = rightLeftPairs.map { case (right, left) =>
                      Glyph(
                        "",
                        Rectangle(
                          left = left,
                          top = word.rectangle.top,
                          width = right - left,
                          height = word.rectangle.height
                        ),
                        1.0
                      )
                    }
                    word.copy(glyphs = myGlyphs)
                  }
                }

                val wordsAndSpaces = Option
                  .when(nonEmptyWords.size > 1)(
                    nonEmptyWords.zip(nonEmptyWords.tail).flatMap { case (word, nextWord) =>
                      if (leftToRight) {
                        val spaceWidth =
                          nextWord.rectangle.left - word.rectangle.right
                        if (spaceWidth > 0) {
                          Seq(
                            word,
                            Space(
                              Rectangle(
                                word.rectangle.right,
                                word.rectangle.top,
                                spaceWidth,
                                word.rectangle.height
                              )
                            )
                          )
                        } else {
                          Seq(word)
                        }
                      } else {
                        val spaceWidth =
                          word.rectangle.left - nextWord.rectangle.right
                        if (spaceWidth > 0) {
                          Seq(
                            word,
                            Space(
                              Rectangle(
                                nextWord.rectangle.right,
                                word.rectangle.top,
                                spaceWidth,
                                word.rectangle.height
                              )
                            )
                          )
                        } else {
                          Seq(word)
                        }
                      }
                    } :+ nonEmptyWords.last
                  )
                  .getOrElse(nonEmptyWords)

                textLine.copy(wordsAndSpaces = wordsAndSpaces)
              } else {
                textLine
              }
            }
            val nonEmptyLines = textLinesWithGlyphs.filter(_.words.nonEmpty)
            textBlock.copy(textLines = nonEmptyLines)
          }
          .filter(_.textLines.nonEmpty)

        // Place individual paragraphs inside top-level text-blocks
        val translatedParagraphs = paragraphPredictions.map(p =>
          p.copy(rectangle = p.rectangle.translate(croppedPrintArea.left, croppedPrintArea.top))
        )

        val paragraphsToPlace = testRectangle
          .map(testRect =>
            translatedParagraphs.filter(paragraph =>
              paragraph.rectangle
                .areaOfIntersection(testRect) / paragraph.rectangle.area.toDouble > 0
            )
          )
          .getOrElse(translatedParagraphs)

        val sortedParagraphs = BlockSorter
          .sort(paragraphsToPlace, leftToRight)
          .collect { case p: PredictedRectangle =>
            p
          }

        val paragraphsWithoutOverlaps = removeOverlapsUnordered(sortedParagraphs)

        val textBlockToParagraphMap = placeRectanglesInTextBlocks(
          textBlocksWithGlyphs,
          paragraphsWithoutOverlaps,
          "Paragraph"
        )

        // If any text block contains more than one paragraph, we split its lines among the paragraphs.
        val correctedTextBlocks = textBlocksWithGlyphs.map { textBlock =>
          val paragraphs = textBlockToParagraphMap
            .getOrElse(textBlock, Seq.empty)
            .sorted(WithRectangle.VerticalOrdering())

          if (log.isDebugEnabled && paragraphs.nonEmpty) {
            log.debug(f"Paragraphs for text block ${textBlock.rectangle.coordinates}:")
            paragraphs.zipWithIndex.foreach { case (paragraph, i) =>
              log.debug(f"Paragraph $i: ${paragraph.rectangle.coordinates}")
            }
          }
          if (paragraphs.size > 1 && textBlock.textLines.nonEmpty) {
            val (textLineGroups, _, _) = textBlock.textLinesWithRectangles.foldLeft(
              Vector(Vector.empty[(TextLine, Rectangle)]),
              0,
              Some(paragraphs.head): Option[PredictedRectangle]
            ) {
              case ((textLineGroups, paragraphIndex, Some(paragraph)), (textLine, rectangle)) =>
                if (textLine.baseLine.y1 > paragraph.rectangle.bottom) {
                  if (log.isDebugEnabled) {
                    log.debug(
                      f"Found new paragraph, textLine baseline ${textLine.baseLine.y1}, paragraph bottom ${paragraph.rectangle.bottom}"
                    )
                  }
                  if (paragraphIndex + 1 == paragraphs.size) {
                    (textLineGroups :+ Vector(textLine -> rectangle), paragraphIndex + 1, None)
                  } else {
                    (
                      textLineGroups :+ Vector(textLine -> rectangle),
                      paragraphIndex + 1,
                      Some(paragraphs(paragraphIndex + 1))
                    )
                  }
                } else {
                  if (log.isDebugEnabled) {
                    log.debug(
                      f"Extending existing paragraph, textLine baseline ${textLine.baseLine.y1}, paragraph bottom ${paragraph.rectangle.bottom}"
                    )
                  }
                  (
                    textLineGroups.init :+ (textLineGroups.last :+ (textLine -> rectangle)),
                    paragraphIndex,
                    Some(paragraph)
                  )
                }
              case ((textLineGroups, paragraphIndex, None), (textLine, rectangle)) =>
                if (log.isDebugEnabled) {
                  log.debug(
                    f"Extending existing paragraph, textLine baseline ${textLine.baseLine.y1}, no paragraphs left"
                  )
                }
                (textLineGroups.init :+ (textLineGroups.last :+ (textLine -> rectangle)), paragraphIndex, None)
            }

            if (textLineGroups.size <= 1) {
              textBlock
            } else {
              val (childTextBlocks, _) = textLineGroups.foldLeft(Seq.empty[TextBlock], textBlock.rectangle.top) {
                case ((childBlocks, top), textLineGroup) =>
                  val bottom = textLineGroup.last._2.bottom
                  val textLines = textLineGroup.map(_._1)
                  val childBlock = TextBlock(
                    rectangle = textBlock.rectangle.copy(top = top, height = bottom - top),
                    textLines = textLines
                  )
                  (childBlocks :+ childBlock, bottom)
              }
              val lastChild = childTextBlocks.last
              val fixedChildren = childTextBlocks.init :+ lastChild.copy(rectangle =
                lastChild.rectangle.copy(height = textBlock.bottom - lastChild.top)
              )
              ComposedBlock(rectangle = textBlock.rectangle, textBlocks = fixedChildren)
            }
          } else {
            textBlock
          }
        }

        val newBlocks = BlockSorter
          .sort(correctedTextBlocks ++ illustrations, leftToRight)
          .collect { case b: Block =>
            b
          }

        val page = Page(
          id = fileName,
          height = mat.rows(),
          width = mat.cols(),
          physicalPageNumber = 1,
          rotation = 0,
          language = language,
          confidence = 1.0,
          blocks = newBlocks
        ).withCleanIds.withDefaultLanguage

        val duration = (Instant.now.toEpochMilli - startTime.toEpochMilli).toDouble / 1000.0
        log.info(f"Finished full segmentation for page $fileName in $duration%.2f seconds")
        page
      }
    } yield page
  }

  private def placeRectanglesInTextBlocks(
      textBlocks: Seq[TextBlock],
      rectangles: Seq[PredictedRectangle],
      itemType: String,
      minIntersection: Double = 0.5,
      splitHorizontally: Boolean = false
  ): Map[TextBlock, Seq[PredictedRectangle]] = {
    val textBlocksByLeft =
      textBlocks.sortBy(t => (t.rectangle.left, t.rectangle.width, t.rectangle.top, t.rectangle.height))
    val textBlocksByRight = textBlocks
      .sortBy(t =>
        (
          t.rectangle.right,
          t.rectangle.width,
          t.rectangle.top,
          t.rectangle.height
        )
      )
      .reverse
    val textBlocksByTop =
      textBlocks.sortBy(t => (t.rectangle.top, t.rectangle.height, t.rectangle.left, t.rectangle.width))
    val textBlocksByBottom = textBlocks
      .sortBy(t =>
        (
          t.rectangle.bottom,
          t.rectangle.height,
          t.rectangle.left,
          t.rectangle.width
        )
      )
      .reverse

    rectangles.foldLeft(Map.empty[TextBlock, Seq[PredictedRectangle]]) { case (textBlockMap, rect) =>
      // Create 4 ordered sequences of textblocks: by left, top, right, bottom
      // Perform a binary search in each sequence to find the boundary textblock - everything inside the boundary is possible
      // As soon as the inside is == 1, we have a single candidate
      // Otherwise, we take the intersection of the sets until we reach 1 or 0.
      if (log.isDebugEnabled)
        log.debug(
          f"Trying to find textBlock for $itemType ${rect.rectangle.coordinates}"
        )

      val candidates = if (textBlocks.nonEmpty) {
        getIntersectingBlocks(
          rect,
          textBlocksByTop,
          textBlocksByLeft,
          textBlocksByBottom,
          textBlocksByRight
        )
      } else {
        Set.empty
      }

      if (log.isDebugEnabled) {
        log.debug(
          f"Final candidates: ${candidates.map(_.rectangle.coordinates).mkString(", ")}"
        )
      }
      val candidatesWithIntersection = candidates
        .map { candidate =>
          val areaOfIntersection =
            candidate.rectangle.areaOfIntersection(rect.rectangle)
          val percentageIntersection =
            areaOfIntersection / rect.rectangle.area.toDouble
          if (log.isDebugEnabled)
            log.debug(
              f"Candidate: ${candidate.rectangle.coordinates}. Percentage intersection: $percentageIntersection%.2f"
            )
          candidate -> percentageIntersection
        }
        .toSeq
        .sortBy(0 - _._2)

      if (splitHorizontally) {
        val validCandidates = candidatesWithIntersection
          .filter { case (_, percentageIntersection) =>
            percentageIntersection > minIntersection
          }
          .map(_._1)

        val updatedContainers = validCandidates.map { container =>
          val currentRectangles = textBlockMap.getOrElse(container, Seq.empty)
          val newLeft =
            Math.max(rect.rectangle.left, container.rectangle.left)
          val newRight =
            Math.min(rect.rectangle.right, container.rectangle.right)
          val newRectangle = rect.copy(rectangle =
            Rectangle(
              left = newLeft,
              top = rect.rectangle.top,
              width = newRight - newLeft,
              height = rect.rectangle.height
            )
          )
          val newRectangles = currentRectangles :+ newRectangle
          container -> newRectangles
        }.toMap

        textBlockMap ++ updatedContainers

      } else {
        val mainCandidate = candidatesWithIntersection.headOption
        val container = mainCandidate.flatMap { case (mainCandidate, percentageIntersection) =>
          Option.when(percentageIntersection > minIntersection)(
            mainCandidate
          )
        }
        if (log.isDebugEnabled)
          log.debug(
            f"Found container block ${container.map(_.rectangle.coordinates)}"
          )
        if (container.isEmpty) {
          if (log.isDebugEnabled)
            log.debug(
              f"Couldn't find container block for ${rect.rectangle.coordinates}"
            )
        }
        container
          .map { container =>
            val currentRectangles =
              textBlockMap.getOrElse(container, Seq.empty)
            val newRectangles = currentRectangles :+ rect
            textBlockMap + (container -> newRectangles)
          }
          .getOrElse(textBlockMap)
      }
    }
  }

  private def getIntersectingBlocks[R <: WithRectangle](
      rect: WithRectangle,
      topOrdered: Seq[R],
      leftOrdered: Seq[R],
      bottomOrdered: Seq[R],
      rightOrdered: Seq[R]
  ): Set[R] = {
    val candidates = {
      if (log.isTraceEnabled) log.trace("Checking top")
      val topLimit = findLimit(
        rect.rectangle,
        topOrdered,
        (candidate, contained) => contained.bottom > candidate.top,
        0,
        topOrdered.size - 1,
        -1
      )
      val topCandidates = topOrdered.take(topLimit + 1).toSet
      if (log.isTraceEnabled)
        log.trace(
          f"Found top candidates: ${topCandidates.map(_.rectangle.coordinates).mkString(", ")}"
        )
      if (topCandidates.size > 1) {
        if (log.isTraceEnabled) log.trace("Checking bottom")
        val bottomLimit = findLimit(
          rect.rectangle,
          bottomOrdered,
          (candidate, contained) => contained.top < candidate.bottom,
          0,
          bottomOrdered.size - 1,
          -1
        )
        val bottomCandidates = bottomOrdered.take(bottomLimit + 1).toSet
        if (log.isTraceEnabled)
          log.trace(
            f"Found bottom candidates: ${bottomCandidates.map(_.rectangle.coordinates).mkString(", ")}"
          )
        val intersection1 = topCandidates.intersect(bottomCandidates)
        if (intersection1.size > 1) {
          if (log.isTraceEnabled) log.trace("Checking left")
          val leftLimit = findLimit(
            rect.rectangle,
            leftOrdered,
            (candidate, contained) => contained.right > candidate.left,
            0,
            leftOrdered.size - 1,
            -1
          )
          val leftCandidates = leftOrdered.take(leftLimit + 1).toSet
          if (log.isTraceEnabled)
            log.trace(
              f"Found left candidates: ${leftCandidates.map(_.rectangle.coordinates).mkString(", ")}"
            )
          val intersection2 = intersection1.intersect(leftCandidates)
          if (intersection2.size > 1) {
            if (log.isTraceEnabled) log.trace("Checking right")
            val rightLimit = findLimit(
              rect.rectangle,
              rightOrdered,
              (candidate, contained) => contained.left < candidate.right,
              0,
              rightOrdered.size - 1,
              -1
            )
            val rightCandidates = rightOrdered.take(rightLimit + 1).toSet
            if (log.isTraceEnabled)
              log.trace(
                f"Found right candidates: ${rightCandidates.map(_.rectangle.coordinates).mkString(", ")}"
              )
            val intersection3 = intersection2.intersect(rightCandidates)
            if (intersection3.nonEmpty) {
              intersection3
            } else {
              Set.empty[R]
            }
          } else {
            intersection2
          }
        } else {
          intersection1
        }
      } else {
        topCandidates
      }
    }

    candidates
  }

  private def placeRectanglesInTextLines(
      textBlock: TextBlock,
      rectangles: Seq[PredictedRectangle],
      itemType: String
  ): Map[TextLine, Seq[PredictedRectangle]] = {
    val textLinesWithRectangles = textBlock.textLinesWithRectangles

    val textLineMap =
      rectangles.foldLeft(Map.empty[TextLine, Seq[PredictedRectangle]]) { case (textLineMap, rect) =>
        if (log.isDebugEnabled)
          log.debug(
            f"Trying to find textLine for $itemType ${rect.rectangle.coordinates}"
          )
        val containerIndex = findContainer(
          rect.rectangle,
          textLinesWithRectangles.map(_._2),
          _.testVerticalOverlap(_),
          0,
          textLinesWithRectangles.size - 1
        )
        val container = Option.when(containerIndex >= 0)(
          textLinesWithRectangles(containerIndex)._1
        )
        if (log.isDebugEnabled) log.debug(f"Found container line $container")
        if (container.isEmpty) {
          if (log.isDebugEnabled)
            log.debug(
              f"Couldn't find container line for ${rect.rectangle.coordinates}"
            )
        }
        container
          .map { container =>
            val currentRectangles =
              textLineMap.getOrElse(container, Seq.empty)
            val newRectangles = currentRectangles :+ rect
            textLineMap + (container -> newRectangles)
          }
          .getOrElse(textLineMap)
      }

    textLineMap.view
      .mapValues(_.sorted(WithRectangle.HorizontalOrdering(leftToRight)))
      .mapValues(removeOverlaps)
      .toMap
  }

  private def placeRectanglesInWords(
      textLine: TextLine,
      rectangles: Seq[PredictedRectangle]
  ): Map[Word, Seq[PredictedRectangle]] = {
    val (wordMap, _) = rectangles.foldLeft(
      Map.empty[Word, Seq[PredictedRectangle]] -> Option.empty[Word]
    ) { case ((wordMap, lastWord), rect) =>
      if (log.isDebugEnabled)
        log.debug(
          f"Trying to find word for glyph ${rect.rectangle.coordinates}"
        )
      // Before binary search, check if glyph is in last recognized container (since glyphs are ordered)
      val container = lastWord
        .filter(
          _.rectangle.testHorizontalOverlap(rect.rectangle, leftToRight) == 0
        )
        .map(Some(_))
        .getOrElse {
          // Run the binary search
          val containerIndex = findContainer(
            rect.rectangle,
            textLine.words.map(_.rectangle),
            _.testHorizontalOverlap(_, leftToRight),
            0,
            textLine.words.size - 1
          )
          Option.when(containerIndex >= 0)(textLine.words(containerIndex))
        }

      if (log.isDebugEnabled) log.debug(f"Found container word $container")
      if (container.isEmpty) {
        if (log.isDebugEnabled)
          log.debug(
            f"Couldn't find container word for glyph ${rect.rectangle.coordinates}"
          )
      }

      val newWordMap = container
        .map { container =>
          val currentRectangles = wordMap.getOrElse(container, Seq.empty)
          val newRectangles = currentRectangles :+ rect
          wordMap + (container -> newRectangles)
        }
        .getOrElse(wordMap)

      newWordMap -> container
    }

    wordMap
  }

  /** Find the maximum index which can contain the rectangle, for an ordered sequence guaranteeing that if item i cannot
    * contain the rectangle, item i+1 cannot contain it either. Similarly, if item i can contain the rectangle, item i-1
    * can contain it as well.
    */
  @tailrec
  private def findLimit[R <: WithRectangle](
      contained: Rectangle,
      candidates: Seq[R],
      canContainTest: (Rectangle, Rectangle) => Boolean,
      startIndex: Int,
      endIndex: Int,
      maxLimit: Int
  ): Int = {
    val testIndex = (startIndex + endIndex) / 2
    val candidate = candidates(testIndex)
    val canContain = canContainTest(candidate.rectangle, contained)

    if (log.isTraceEnabled)
      log.trace(
        f"startIndex $startIndex, testIndex $testIndex, endIndex $endIndex, minLimit $maxLimit. Can ${candidate.rectangle.coordinates} contain ${contained.coordinates}? $canContain"
      )

    val newLimit = if (canContain && testIndex > maxLimit) {
      testIndex
    } else {
      maxLimit
    }

    if (startIndex >= endIndex) {
      newLimit
    } else {
      if (canContain) {
        findLimit(
          contained,
          candidates,
          canContainTest,
          testIndex + 1,
          endIndex,
          newLimit
        )
      } else {
        findLimit(
          contained,
          candidates,
          canContainTest,
          startIndex,
          testIndex - 1,
          newLimit
        )
      }
    }
  }

  @tailrec
  private def findContainer(
      contained: Rectangle,
      candidates: Seq[Rectangle],
      testOverlap: (Rectangle, Rectangle) => Int,
      startIndex: Int,
      endIndex: Int
  ): Int = {
    if (candidates.isEmpty) {
      return -1
    }
    val testIndex = (startIndex + endIndex) / 2
    val candidate = candidates(testIndex)
    val overlapResult = testOverlap(candidate, contained)
    if (log.isDebugEnabled)
      log.debug(
        s"startIndex: $startIndex, testIndex $testIndex, endIndex: $endIndex. Does ${candidate.coordinates} contain ${contained.coordinates}? $overlapResult"
      )
    if (overlapResult == 0) {
      testIndex
    } else {
      if (startIndex >= endIndex) {
        -1
      } else if (overlapResult < 0) {
        findContainer(
          contained,
          candidates,
          testOverlap,
          testIndex + 1,
          endIndex
        )
      } else {
        findContainer(
          contained,
          candidates,
          testOverlap,
          startIndex,
          testIndex - 1
        )
      }
    }
  }

  private def removeOverlaps(
      rects: Seq[PredictedRectangle]
  ): Seq[PredictedRectangle] = rects match {
    case Nil => rects
    case head +: tail =>
      val (remove, remainder) = tail.span { other =>
        val areaOfIntersection =
          head.rectangle.areaOfIntersection(other.rectangle)
        if (log.isTraceEnabled)
          log.trace(
            f"Head: ${head.rectangle.coordinates}. Other: ${other.rectangle.coordinates}. areaOfIntersection: $areaOfIntersection. head %%: ${areaOfIntersection / head.rectangle.area}. other %%: ${areaOfIntersection / other.rectangle.area}"
          )
        areaOfIntersection / head.rectangle.area > 0.25 || areaOfIntersection / other.rectangle.area > 0.25
      }
      if (remove.nonEmpty) {
        val group = head +: remove
        val maxConfidenceRect =
          MathUtils.argMaxFirst(group)(_.confidence).getOrElse(head)
        if (log.isDebugEnabled) {
          val removed = group.diff(Seq(maxConfidenceRect))
          log.debug(
            f"Keeping ${maxConfidenceRect.rectangle.coordinates} with confidence ${maxConfidenceRect.confidence}"
          )
          log.debug(
            f"Removing ${removed.map(r => f"${r.rectangle.coordinates} conf ${r.confidence}").mkString(", ")}"
          )
        }
        if (head == maxConfidenceRect) {
          maxConfidenceRect +: removeOverlaps(remainder)
        } else {
          removeOverlaps(maxConfidenceRect +: remainder)
        }
      } else {
        head +: removeOverlaps(remainder)
      }
  }

  private def removeOverlapsUnordered(
      rects: Seq[PredictedRectangle]
  ): Seq[PredictedRectangle] = {
    val leftOrdered =
      rects.sortBy(t => (t.rectangle.left, t.rectangle.width, t.rectangle.top, t.rectangle.height))
    val rightOrdered = rects
      .sortBy(t =>
        (
          t.rectangle.right,
          t.rectangle.width,
          t.rectangle.top,
          t.rectangle.height
        )
      )
      .reverse
    val topOrdered =
      rects.sortBy(t => (t.rectangle.top, t.rectangle.height, t.rectangle.left, t.rectangle.width))
    val bottomOrdered = rects
      .sortBy(t =>
        (
          t.rectangle.bottom,
          t.rectangle.height,
          t.rectangle.left,
          t.rectangle.width
        )
      )
      .reverse

    val rectangleOverlaps =
      rects.foldLeft(Map.empty[PredictedRectangle, Set[PredictedRectangle]]) { case (overlaps, rect) =>
        val candidates = getIntersectingBlocks(
          rect,
          topOrdered,
          leftOrdered,
          bottomOrdered,
          rightOrdered
        ) - rect
        val myOverlaps = candidates.filter { candidate =>
          val candidateIntersection =
            candidate.rectangle.percentageIntersection(rect.rectangle)
          val rectangleIntersection =
            rect.rectangle.percentageIntersection(candidate.rectangle)
          candidateIntersection > 0.2 || rectangleIntersection > 0.2
        }

        if (log.isTraceEnabled) {
          if (candidates.size > 1) {
            log.trace(
              f"For ${rect.rectangle.coordinates} found overlaps with ${myOverlaps.map(_.rectangle.coordinates).mkString(", ")}"
            )
          }
        }
        overlaps + (rect -> myOverlaps)
      }

    // merge rectangles if both are above a certain confidence and they overlap
    val mergeGroups = rects.foldLeft(Seq.empty[Set[PredictedRectangle]]) { case (mergeGroups, rect) =>
      val mergeGroupIndex = mergeGroups.zipWithIndex
        .find { case (group, _) => group.contains(rect) }
        .map(_._2)

      if (rect.confidence >= alwaysRetainBlockThreshold) {
        val overlaps = rectangleOverlaps(rect).filter(
          _.confidence >= alwaysRetainBlockThreshold
        )
        if (overlaps.nonEmpty) {
          mergeGroupIndex
            .map { mergeGroupIndex =>
              val newMergeGroup = mergeGroups(mergeGroupIndex) ++ overlaps
              if (log.isDebugEnabled) {
                log.debug(f"Grow merge group: ${newMergeGroup
                  .map(r => f"${r.rectangle.coordinates} (${r.confidence}%.2f)")
                  .mkString(", ")}")
              }
              (mergeGroups.take(
                mergeGroupIndex
              ) :+ newMergeGroup) ++ mergeGroups.drop(mergeGroupIndex + 1)
            }
            .getOrElse {
              val newMergeGroup = overlaps + rect
              if (log.isDebugEnabled) {
                log.debug(f"New merge group: ${newMergeGroup
                  .map(r => f"${r.rectangle.coordinates} (${r.confidence}%.2f)")
                  .mkString(", ")}")
              }
              mergeGroups :+ newMergeGroup
            }
        } else {
          mergeGroups
        }
      } else {
        mergeGroups
      }
    }

    val unmerged = rects.filterNot { rect =>
      mergeGroups.exists(_.contains(rect))
    }
    val mergedRects = unmerged ++ mergeGroups.map { mergeGroup =>
      val confidence = Math.exp(mergeGroup.foldLeft(0.0) { case (sum, r) =>
        sum + Math.log(r.confidence)
      } / mergeGroup.size.toDouble)
      val rectangle = mergeGroup.map(_.rectangle).reduce(_.union(_))
      val merged =
        PredictedRectangle(mergeGroup.head.label, rectangle, confidence)
      if (log.isDebugEnabled) {
        log.debug(
          f"Created merged rectangle: ${merged.rectangle.coordinates} with confidence ${merged.confidence}%.2f from rectangles ${mergeGroup
            .map(r => f"${r.rectangle.coordinates} (${r.confidence}%.2f)")
            .mkString(", ")}"
        )
      }
      merged
    }

    val mergedOverlaps = if (mergeGroups.nonEmpty) {
      val leftOrdered = mergedRects.sortBy(t =>
        (
          t.rectangle.left,
          t.rectangle.width,
          t.rectangle.top,
          t.rectangle.height
        )
      )
      val rightOrdered = mergedRects
        .sortBy(t =>
          (
            t.rectangle.right,
            t.rectangle.width,
            t.rectangle.top,
            t.rectangle.height
          )
        )
        .reverse
      val topOrdered = mergedRects.sortBy(t =>
        (
          t.rectangle.top,
          t.rectangle.height,
          t.rectangle.left,
          t.rectangle.width
        )
      )
      val bottomOrdered = mergedRects
        .sortBy(t =>
          (
            t.rectangle.bottom,
            t.rectangle.height,
            t.rectangle.left,
            t.rectangle.width
          )
        )
        .reverse

      mergedRects.foldLeft(
        Map.empty[PredictedRectangle, Set[PredictedRectangle]]
      ) { case (overlaps, rect) =>
        val candidates = getIntersectingBlocks(
          rect,
          topOrdered,
          leftOrdered,
          bottomOrdered,
          rightOrdered
        ) - rect
        val myOverlaps = candidates.filter { candidate =>
          val candidateIntersection =
            candidate.rectangle.percentageIntersection(rect.rectangle)
          val rectangleIntesection =
            rect.rectangle.percentageIntersection(candidate.rectangle)
          candidateIntersection > 0.2 || rectangleIntesection > 0.2
        }

        if (log.isTraceEnabled) {
          if (candidates.size > 1) {
            log.trace(
              f"For ${rect.rectangle.coordinates} found overlaps with ${myOverlaps.map(_.rectangle.coordinates).mkString(", ")}"
            )
          }
        }
        overlaps + (rect -> myOverlaps)
      }
    } else {
      rectangleOverlaps
    }

    val toRemove = mergedRects.foldLeft(Set.empty[PredictedRectangle]) { case (toRemove, rect) =>
      if (toRemove.contains(rect)) {
        if (log.isDebugEnabled) {
          log.debug(
            f"Rectangle already removed: ${rect.rectangle.coordinates}"
          )
        }
        toRemove
      } else {
        if (log.isDebugEnabled) {
          log.debug(
            f"Checking overlaps for rectangle ${rect.rectangle.coordinates}"
          )
        }
        val candidates = mergedOverlaps(rect)
        if (log.isTraceEnabled) {
          if (candidates.size > 1) {
            log.trace(
              f"For ${rect.rectangle.coordinates} found overlaps (including already removed) with ${candidates
                .map(_.rectangle.coordinates)
                .mkString(", ")}"
            )
          }
        }
        val notYetRemoved = candidates.diff(toRemove)
        if (log.isDebugEnabled) {
          if (notYetRemoved.size > 1) {
            log.debug(
              f"For ${rect.rectangle.coordinates} (${rect.confidence}%.2f) found overlaps with ${notYetRemoved
                .map(r => f"${r.rectangle.coordinates} (${r.confidence}%.2f)")
                .mkString(", ")}"
            )
          }
        }
        val (higherConfidence, lowerConfidence) =
          notYetRemoved.partition(_.confidence > rect.confidence)

        if (higherConfidence.nonEmpty) {
          if (log.isDebugEnabled) {
            log.debug(f"Removing current: ${rect.rectangle.coordinates}")
          }
          toRemove + rect
        } else if (lowerConfidence.nonEmpty) {
          if (log.isDebugEnabled) {
            log.debug(
              f"Removing others: ${lowerConfidence.map(_.rectangle.coordinates).mkString(", ")}"
            )
          }
          toRemove ++ lowerConfidence
        } else {
          toRemove
        }
      }
    }

    if (log.isDebugEnabled) {
      log.debug(
        f"Removing overlapping rectangles: ${toRemove.map(_.rectangle.coordinates).mkString(", ")}"
      )
    }

    mergedRects.filter(!toRemove.contains(_))
  }
}
