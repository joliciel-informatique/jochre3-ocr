package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.graphics.{BlockSorter, Line, PredictedRectangle, Rectangle, WithRectangle}
import com.joliciel.jochre.ocr.core.model.{Block, Glyph, Illustration, Page, Space, TextBlock, TextLine, Word}
import com.joliciel.jochre.ocr.core.utils.{ImageUtils, MathUtils, OutputLocation, StringUtils}
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import zio.{Task, ZIO, ZLayer}

import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import scala.util.Using

object FullYoloSegmenterService {
  val live: ZLayer[YoloPredictorService, Nothing, SegmenterService] = ZLayer.fromFunction(FullYoloSegmenterServiceImpl(_))
}

private[segmentation] case class FullYoloSegmenterServiceImpl(yoloPredictorService: YoloPredictorService) extends SegmenterService {
  def getSegmenter(): Task[FullYoloSegmenter] = {
    ZIO.attempt(new FullYoloSegmenter(yoloPredictorService))
  }
}

private[segmentation] class FullYoloSegmenter(yoloPredictorService: YoloPredictorService) extends Segmenter with ImageUtils {
  private val log = LoggerFactory.getLogger(getClass)

  private val config = ConfigFactory.load().getConfig("jochre.ocr.yolo")
  private val cropToPrintArea = config.getBoolean("crop-to-print-area")
  private val cropMargin = config.getDouble("crop-margin")
  private val glyphImageTileCount = config.getInt("glyph-image-tile-count")
  private val tileMargin = config.getDouble("tile-margin")

  private val language = ConfigFactory.load().getConfig("jochre.ocr").getString("language")
  private val leftToRight = StringUtils.isLeftToRight(language)

  /** Transform an image into a segmented [[Page]] structure.
   * The page might only be segmented down to a given level (e.g. blocks, lines, strings, or glyphs) */
  override def segment(mat: Mat, fileName: String, debugLocation: Option[OutputLocation], testRectangle: Option[Rectangle] = None): Task[Page] = {
    for {
      blockPredictor <- yoloPredictorService.getYoloPredictor(YoloPredictionType.Blocks, mat, fileName, debugLocation, Some(0.20))
      blockPredictions <- blockPredictor.predict()
      pageWithBlocks <- ZIO.attempt {
        val sortedBlockPredictions = BlockSorter.sort(blockPredictions, leftToRight)
          .collect{
            case p: PredictedRectangle => p
          }

        // For now, we simply remove overlaps
        val sortedWithoutOverlaps = removeOverlapsUnordered(sortedBlockPredictions)

        val blocks = sortedWithoutOverlaps.flatMap {
          case PredictedRectangle(label, rect, _) =>
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
          language = language,
          confidence = 1.0,
          blocks = blocks
        ).withCleanIds.withDefaultLanguage
      }
      croppedPrintArea <- ZIO.attempt{
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
      croppedRectangle = Rectangle(0, 0, printAreaMat.cols(), printAreaMat.rows())
      linePredictor <- yoloPredictorService.getYoloPredictor(YoloPredictionType.Lines, printAreaMat, fileName, debugLocation, Some(0.05))
      linePredictions <- linePredictor.predict()
      wordPredictor <- yoloPredictorService.getYoloPredictor(YoloPredictionType.Words, printAreaMat, fileName, debugLocation, Some(0.05))
      wordPredictions <- wordPredictor.predict()
      glyphPredictions <- {
        // Get glyph predictions for overlapping tiles
        // The assumption is that if the same glyph is predicted by two tiles, one of the two will be eliminated downstream
        val tiles = croppedRectangle.tile(glyphImageTileCount, glyphImageTileCount, tileMargin)
        ZIO.foreach(tiles) { tile =>
          val tileMat = crop(printAreaMat, tile)
          for {
            glyphPredictor <- yoloPredictorService.getYoloPredictor(YoloPredictionType.Glyphs, tileMat, fileName, debugLocation, Some(0.10))
            glyphPredictions <- glyphPredictor.predict()
          } yield glyphPredictions.map{ prediction =>
            prediction.copy(rectangle = prediction.rectangle.translate(0 - tile.left, 0 - tile.top))
          }
        }.map(_.flatten)
      }
      page <- ZIO.attempt {
        val textBlocks = pageWithBlocks.textBlocks
        val textBlocksToConsider = testRectangle.map(testRect => textBlocks.filter{ block =>
          block.rectangle.areaOfIntersection(testRect) / block.rectangle.area.toDouble > 0
        })
          .getOrElse(textBlocks)

        debugLocation.foreach{ debugLocation =>
          val rightLeftCoordsFile = debugLocation.resolve("_right-left.txt")
          Using(new OutputStreamWriter(new FileOutputStream(rightLeftCoordsFile.toFile), StandardCharsets.UTF_8)) { writer =>
            val leftCoords = textBlocksToConsider.map(_.rectangle.left).mkString(",")
            val rightCoords = textBlocksToConsider.map(_.rectangle.right).mkString(",")
            writer.write(leftCoords)
            writer.write("\n")
            writer.write(rightCoords)
            writer.write("\n")
            writer.flush()
          }
        }
        val illustrations = pageWithBlocks.illustrations

        // Place lines inside blocks
        val translatedLinePredictions = linePredictions.map(p => p.copy(rectangle = p.rectangle.translate(croppedPrintArea.left, croppedPrintArea.top)))
        // lines are rectangles predicted vertically centered around the baseline - we want to move them upwards to place them as much as possible inside the text box
        val linesBumpedUp = translatedLinePredictions
          .filter(line => line.rectangle.bottom <= pageWithBlocks.height)
          .map(p => p.copy(rectangle = p.rectangle.translate(0, 0 - (p.rectangle.height / 2))))
        val linesToPlace = testRectangle.map(testRect => linesBumpedUp.filter(line => line.rectangle.areaOfIntersection(testRect) / line.rectangle.area.toDouble > 0))
          .getOrElse(linesBumpedUp)

        val textBlockToLineMap = placeRectanglesInTextBlocks(textBlocksToConsider, linesToPlace, "TextLine", minIntersection = 0.01, splitHorizontally = true)

        val textBlocksWithLines = textBlocksToConsider.map{ textBlock =>
          val myLineRects = textBlockToLineMap.get(textBlock).getOrElse(Seq.empty).sorted(WithRectangle.VerticalOrdering())
          val myLineRectsWithoutOverlaps = removeOverlaps(myLineRects)
          val myLines = myLineRectsWithoutOverlaps.map(lineRect => TextLine(Line(textBlock.rectangle.left, lineRect.rectangle.bottom, textBlock.rectangle.right, lineRect.rectangle.bottom), Seq.empty))
          textBlock.copy(textLines = myLines)
        }

        // Place words inside blocks
        val translatedWordPredictions = wordPredictions.map(p => p.copy(rectangle = p.rectangle.translate(croppedPrintArea.left, croppedPrintArea.top)))
          // Avoid words that overlap the bottom of the page
          .filter(word => word.rectangle.bottom < pageWithBlocks.height - 1)

        val wordsToPlace = testRectangle.map(testRect => translatedWordPredictions.filter(word => word.rectangle.areaOfIntersection(testRect) / word.rectangle.area.toDouble > 0.8))
          .getOrElse(translatedWordPredictions)
        val textBlockToWordMap = placeRectanglesInTextBlocks(textBlocksWithLines, wordsToPlace, "Word")

        val textBlocksWithWords = textBlocksWithLines.map{ textBlock =>
          val wordsToInclude = textBlockToWordMap.get(textBlock).getOrElse(Seq.empty)
          val textLineToWordMap = placeRectanglesInTextLines(textBlock, wordsToInclude, "Word")

          textBlock.copy(textLines = textBlock.textLines.map{ textLine =>
            val myWordRects = textLineToWordMap.get(textLine).getOrElse(Seq.empty)
            val myWords = myWordRects.map(rect => Word("", rect.rectangle, Seq.empty, Seq.empty, 1.0))

            textLine.copy(wordsAndSpaces = myWords)
          })
        }

        // Place glyphs inside words
        val translatedGlyphPredictions = glyphPredictions.map(p => p.copy(rectangle = p.rectangle.translate(croppedPrintArea.left, croppedPrintArea.top)))
        val glyphsToPlace = testRectangle.map(testRect => translatedGlyphPredictions.filter(glyph => glyph.rectangle.areaOfIntersection(testRect) / glyph.rectangle.area.toDouble > 0.8))
          .getOrElse(translatedGlyphPredictions)

        val textBlockToGlyphMap = placeRectanglesInTextBlocks(textBlocksWithWords, glyphsToPlace, "Glyph")
        val textBlocksWithGlyphs = textBlocksWithWords.map{ textBlock =>
          val glyphsToInclude = textBlockToGlyphMap.get(textBlock).getOrElse(Seq.empty)
          val textLineToGlyphMap = placeRectanglesInTextLines(textBlock, glyphsToInclude, "Glyph")
          val textLinesWithGlyphs = textBlock.textLines.map { textLine =>
            val textLineGlyphs = textLineToGlyphMap.get(textLine).getOrElse(Seq.empty)
            if (log.isDebugEnabled) log.debug(f"In TextLine ${textLine.baseLine} with ${textLine.words.size} words, found glyphs: ${textLineGlyphs.map(_.rectangle.coordinates).mkString(", ")}")
            if (!textLineGlyphs.isEmpty) {
              val wordToGlyphMap = placeRectanglesInWords(textLine, textLineGlyphs)
              val nonEmptyWords = textLine.words.flatMap { word =>
                  val myGlyphRects = wordToGlyphMap.get(word).getOrElse(Seq.empty)
                  Option.when(!myGlyphRects.isEmpty) {
                    // Take average border between two sequential glyphs as their border
                    val borders = myGlyphRects.zip(myGlyphRects.tail).map {
                      case (firstRect, secondRect) => (firstRect.rectangle.left + secondRect.rectangle.right) / 2
                    }
                    val rightLeftPairs = (myGlyphRects.head.rectangle.right +: borders).zip(borders :+ myGlyphRects.last.rectangle.left)

                    val myGlyphs = rightLeftPairs.map{
                      case (right, left) => Glyph("", Rectangle( left = left, top = word.rectangle.top, width = right-left, height = word.rectangle.height), 1.0)
                    }
                    word.copy(glyphs = myGlyphs)
                  }
              }

              val wordsAndSpaces = Option.when(nonEmptyWords.size > 1)(nonEmptyWords.zip(nonEmptyWords.tail).flatMap { case (word, nextWord) =>
                if (leftToRight) {
                  val spaceWidth = nextWord.rectangle.left - word.rectangle.right
                  if (spaceWidth > 0) {
                    Seq(word, Space(Rectangle(word.rectangle.right, word.rectangle.top, spaceWidth, word.rectangle.height)))
                  } else {
                    Seq(word)
                  }
                } else {
                  val spaceWidth = word.rectangle.left - nextWord.rectangle.right
                  if (spaceWidth > 0) {
                    Seq(word, Space(Rectangle(nextWord.rectangle.right, word.rectangle.top, spaceWidth, word.rectangle.height)))
                  } else {
                    Seq(word)
                  }
                }
              } :+ nonEmptyWords.last).getOrElse(nonEmptyWords)

              textLine.copy(wordsAndSpaces = wordsAndSpaces)
            } else {
              textLine
            }
          }
          val nonEmptyLines = textLinesWithGlyphs.filter(_.words.size > 0)
          textBlock.copy(textLines = nonEmptyLines)
        }.filter(_.textLines.size > 0)

        val newBlocks = BlockSorter.sort(textBlocksWithGlyphs ++ illustrations, leftToRight)
          .collect{
            case b: Block => b
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

        page
      }
    } yield page
  }

  private def placeRectanglesInTextBlocks(textBlocks: Seq[TextBlock], rectangles: Seq[PredictedRectangle], itemType: String, minIntersection: Double = 0.5, splitHorizontally: Boolean = false): Map[TextBlock, Seq[PredictedRectangle]] = {
    val textBlocksByLeft = textBlocks.sortBy(t => (t.rectangle.left, t.rectangle.width, t.rectangle.top, t.rectangle.height))
    val textBlocksByRight = textBlocks.sortBy(t => (t.rectangle.right, t.rectangle.width, t.rectangle.top, t.rectangle.height)).reverse
    val textBlocksByTop = textBlocks.sortBy(t => (t.rectangle.top, t.rectangle.height, t.rectangle.left, t.rectangle.width))
    val textBlocksByBottom = textBlocks.sortBy(t => (t.rectangle.bottom, t.rectangle.height, t.rectangle.left, t.rectangle.width)).reverse

    rectangles.foldLeft(Map.empty[TextBlock, Seq[PredictedRectangle]]) { case (textBlockMap, rect) =>
      // Create 4 ordered sequences of textblocks: by left, top, right, bottom
      // Perform a binary search in each sequence to find the boundary textblock - everything inside the boundary is possible
      // As soon as the inside is == 1, we have a single candidate
      // Otherwise, we take the intersection of the sets until we reach 1 or 0.
      if (log.isDebugEnabled) log.debug(f"Trying to find textBlock for $itemType ${rect.rectangle.coordinates}")

      val candidates = if (textBlocks.size > 0) {
        getIntersectingBlocks(rect, textBlocksByTop, textBlocksByLeft, textBlocksByBottom, textBlocksByRight)
      } else {
        Set.empty
      }

      if (log.isDebugEnabled) {
        log.debug(f"Final candidates: ${candidates.map(_.rectangle.coordinates).mkString(", ")}")
      }
      val candidatesWithIntersection = candidates.map{ candidate =>
        val areaOfIntersection = candidate.rectangle.areaOfIntersection(rect.rectangle)
        val percentageIntersection = areaOfIntersection / rect.rectangle.area.toDouble
        if (log.isDebugEnabled) log.debug(f"Candidate: ${candidate.rectangle.coordinates}. Percentage intersection: $percentageIntersection%.2f")
        candidate -> percentageIntersection
      }.toSeq.sortBy(0 - _._2)

      if (splitHorizontally) {
        val validCandidates = candidatesWithIntersection.filter{
          case (_, percentageIntersection) => percentageIntersection > minIntersection
        }.map(_._1)

        val updatedContainers = validCandidates.map{ container =>
          val currentRectangles = textBlockMap.get(container).getOrElse(Seq.empty)
          val newLeft = Math.max(rect.rectangle.left, container.rectangle.left)
          val newRight = Math.min(rect.rectangle.right, container.rectangle.right)
          val newRectangle = rect.copy(rectangle = Rectangle(
            left = newLeft,
            top = rect.rectangle.top,
            width = newRight - newLeft,
            height = rect.rectangle.height
          ))
          val newRectangles = currentRectangles :+ newRectangle
          container -> newRectangles
        }.toMap

        textBlockMap ++ updatedContainers

      } else {
        val mainCandidate = candidatesWithIntersection.headOption
        val container = mainCandidate.flatMap { case (mainCandidate, percentageIntersection) =>
          Option.when(percentageIntersection > minIntersection)(mainCandidate)
        }
        if (log.isDebugEnabled) log.debug(f"Found container block ${container.map(_.rectangle.coordinates)}")
        if (container.isEmpty) {
          if (log.isDebugEnabled) log.debug(f"Couldn't find container block for ${rect.rectangle.coordinates}")
        }
        container.map { container =>
          val currentRectangles = textBlockMap.get(container).getOrElse(Seq.empty)
          val newRectangles = currentRectangles :+ rect
          textBlockMap + (container -> newRectangles)
        }.getOrElse(textBlockMap)
      }
    }
  }

  private def getIntersectingBlocks[R <: WithRectangle](rect: WithRectangle, topOrdered: Seq[R], leftOrdered: Seq[R], bottomOrdered: Seq[R], rightOrdered: Seq[R]): Set[R] = {
    val candidates = {
      if (log.isTraceEnabled) log.trace("Checking top")
      val topLimit = findLimit(rect.rectangle, topOrdered, ((candidate, contained) => contained.bottom > candidate.top), 0, topOrdered.size - 1, -1)
      val topCandidates = topOrdered.take(topLimit + 1).toSet
      if (log.isTraceEnabled) log.trace(f"Found top candidates: ${topCandidates.map(_.rectangle.coordinates).mkString(", ")}")
      if (topCandidates.size > 1) {
        if (log.isTraceEnabled) log.trace("Checking bottom")
        val bottomLimit = findLimit(rect.rectangle, bottomOrdered, ((candidate, contained) => contained.top < candidate.bottom), 0, bottomOrdered.size - 1, -1)
        val bottomCandidates = bottomOrdered.take(bottomLimit + 1).toSet
        if (log.isTraceEnabled) log.trace(f"Found bottom candidates: ${bottomCandidates.map(_.rectangle.coordinates).mkString(", ")}")
        val intersection1 = topCandidates.intersect(bottomCandidates)
        if (intersection1.size > 1) {
          if (log.isTraceEnabled) log.trace("Checking left")
          val leftLimit = findLimit(rect.rectangle, leftOrdered, ((candidate, contained) => contained.right > candidate.left), 0, leftOrdered.size - 1, -1)
          val leftCandidates = leftOrdered.take(leftLimit + 1).toSet
          if (log.isTraceEnabled) log.trace(f"Found left candidates: ${leftCandidates.map(_.rectangle.coordinates).mkString(", ")}")
          val intersection2 = intersection1.intersect(leftCandidates)
          if (intersection2.size > 1) {
            if (log.isTraceEnabled) log.trace("Checking right")
            val rightLimit = findLimit(rect.rectangle, rightOrdered, ((candidate, contained) => contained.left < candidate.right), 0, rightOrdered.size - 1, -1)
            val rightCandidates = rightOrdered.take(rightLimit + 1).toSet
            if (log.isTraceEnabled) log.trace(f"Found right candidates: ${rightCandidates.map(_.rectangle.coordinates).mkString(", ")}")
            val intersection3 = intersection2.intersect(rightCandidates)
            if (intersection3.size > 0) {
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

  private def placeRectanglesInTextLines(textBlock: TextBlock, rectangles: Seq[PredictedRectangle], itemType: String): Map[TextLine, Seq[PredictedRectangle]] = {
    val textLinesWithRectangles = textBlock.textLinesWithRectangles

    val textLineMap = rectangles.foldLeft(Map.empty[TextLine, Seq[PredictedRectangle]]) { case (textLineMap, rect) =>
      if (log.isDebugEnabled) log.debug(f"Trying to find textLine for $itemType ${rect.rectangle.coordinates}")
      val containerIndex = findContainer(rect.rectangle, textLinesWithRectangles.map(_._2), _.testVerticalOverlap(_), 0, textLinesWithRectangles.size-1)
      val container = Option.when(containerIndex >= 0)(textLinesWithRectangles(containerIndex)._1)
      if (log.isDebugEnabled) log.debug(f"Found container line $container")
      if (container.isEmpty) {
        if (log.isDebugEnabled) log.debug(f"Couldn't find container line for ${rect.rectangle.coordinates}")
      }
      container.map { container =>
        val currentRectangles = textLineMap.get(container).getOrElse(Seq.empty)
        val newRectangles = currentRectangles :+ rect
        textLineMap + (container -> newRectangles)
      }.getOrElse(textLineMap)
    }

    textLineMap.view
      .mapValues(_.sorted(WithRectangle.HorizontalOrdering(leftToRight)))
      .mapValues(removeOverlaps(_))
      .toMap
  }

  private def placeRectanglesInWords(textLine: TextLine, rectangles: Seq[PredictedRectangle]): Map[Word, Seq[PredictedRectangle]] = {
    val (wordMap, _) = rectangles.foldLeft(Map.empty[Word, Seq[PredictedRectangle]] -> Option.empty[Word]) { case ((wordMap, lastWord), rect) =>
      if (log.isDebugEnabled) log.debug(f"Trying to find word for glyph ${rect.rectangle.coordinates}")
      // Before binary search, check if glyph is in last recognized container (since glyphs are ordered)
      val container = lastWord.filter(_.rectangle.testHorizontalOverlap(rect.rectangle, leftToRight)==0).map(Some(_))
        .getOrElse {
          // Run the binary search
          val containerIndex = findContainer(rect.rectangle, textLine.words.map(_.rectangle), _.testHorizontalOverlap(_, leftToRight), 0, textLine.words.size-1)
          Option.when(containerIndex >= 0)(textLine.words(containerIndex))
        }

      if (log.isDebugEnabled) log.debug(f"Found container word $container")
      if (container.isEmpty) {
        if (log.isDebugEnabled) log.debug(f"Couldn't find container word for glyph ${rect.rectangle.coordinates}")
      }

      val newWordMap = container.map { container =>
        val currentRectangles = wordMap.get(container).getOrElse(Seq.empty)
        val newRectangles = currentRectangles :+ rect
        wordMap + (container -> newRectangles)
      }.getOrElse(wordMap)

      newWordMap -> container
    }

    wordMap
  }

  /**
   * Find the maximum index which can contain the rectangle, for an ordered sequence guaranteeing that if item i cannot contain the rectangle,
   * item i+1 cannot contain it either. Similarly, if item i can contain the rectangle, item i-1 can contain it as well.
   */
  private def findLimit[R <: WithRectangle](contained: Rectangle, candidates: Seq[R], canContainTest: (Rectangle, Rectangle) => Boolean, startIndex: Int, endIndex: Int, maxLimit: Int): Int = {
    val testIndex = (startIndex + endIndex) / 2
    val candidate = candidates(testIndex)
    val canContain = canContainTest(candidate.rectangle, contained)

    if (log.isTraceEnabled) log.trace(f"startIndex $startIndex, testIndex $testIndex, endIndex $endIndex, minLimit $maxLimit. Can ${candidate.rectangle.coordinates} contain ${contained.coordinates}? ${canContain}")

    val newLimit = if (canContain && testIndex > maxLimit) {
      testIndex
    } else {
      maxLimit
    }

    if (startIndex>=endIndex) {
      newLimit
    } else {
      if (canContain) {
        findLimit(contained, candidates, canContainTest, testIndex + 1, endIndex, newLimit)
      } else {
        findLimit(contained, candidates, canContainTest, startIndex, testIndex - 1, newLimit)
      }
    }
  }

  private def findContainer(contained: Rectangle, candidates: Seq[Rectangle], testOverlap: (Rectangle, Rectangle) => Int, startIndex: Int, endIndex: Int): Int = {
    if (candidates.size==0) {
      return -1
    }
    val testIndex = (startIndex + endIndex) / 2
    val candidate = candidates(testIndex)
    val overlapResult = testOverlap(candidate, contained)
    if (log.isDebugEnabled) log.debug(s"startIndex: $startIndex, testIndex $testIndex, endIndex: $endIndex. Does ${candidate.coordinates} contain ${contained.coordinates}? ${overlapResult}")
    if (overlapResult==0) {
      testIndex
    } else {
      if (startIndex>=endIndex) {
        -1
      } else if (overlapResult<0) {
        findContainer(contained, candidates, testOverlap, testIndex+1, endIndex)
      } else {
        findContainer(contained, candidates, testOverlap, startIndex, testIndex-1)
      }
    }
  }

  private def removeOverlaps(rects: Seq[PredictedRectangle]): Seq[PredictedRectangle] = rects match {
    case Nil => rects
    case head +: tail =>
      val (remove, remainder) = tail.span{ other =>
        val areaOfIntersection = head.rectangle.areaOfIntersection(other.rectangle)
        if (log.isTraceEnabled) log.trace(f"Head: ${head.rectangle.coordinates}. Other: ${other.rectangle.coordinates}. areaOfIntersection: $areaOfIntersection. head %%: ${areaOfIntersection / head.rectangle.area}. other %%: ${areaOfIntersection / other.rectangle.area}")
        areaOfIntersection / head.rectangle.area > 0.25 || areaOfIntersection / other.rectangle.area > 0.25
      }
      if (!remove.isEmpty) {
        val group = head +: remove
        val maxConfidenceRect = MathUtils.argMaxFirst(group)(_.confidence).getOrElse(head)
        if (log.isDebugEnabled) {
          val removed = group.diff(Seq(maxConfidenceRect))
          log.debug(f"Keeping ${maxConfidenceRect.rectangle.coordinates} with confidence ${maxConfidenceRect.confidence}")
          log.debug(f"Removing ${removed.map(r => f"${r.rectangle.coordinates} conf ${r.confidence}").mkString(", ")}")
        }
        if (head==maxConfidenceRect) {
          maxConfidenceRect +: removeOverlaps(remainder)
        } else {
          removeOverlaps(maxConfidenceRect +: remainder)
        }
      } else {
        head +: removeOverlaps(remainder)
      }
  }

  private def removeOverlapsUnordered(rects: Seq[PredictedRectangle]): Seq[PredictedRectangle] = {
    val leftOrdered = rects.sortBy(t => (t.rectangle.left, t.rectangle.width, t.rectangle.top, t.rectangle.height))
    val rightOrdered = rects.sortBy(t => (t.rectangle.right, t.rectangle.width, t.rectangle.top, t.rectangle.height)).reverse
    val topOrdered = rects.sortBy(t => (t.rectangle.top, t.rectangle.height, t.rectangle.left, t.rectangle.width))
    val bottomOrdered = rects.sortBy(t => (t.rectangle.bottom, t.rectangle.height, t.rectangle.left, t.rectangle.width)).reverse

    val toRemove = rects.foldLeft(Set.empty[PredictedRectangle]) { case (toRemove, rect) =>
      val candidates = getIntersectingBlocks(rect, topOrdered, leftOrdered, bottomOrdered, rightOrdered) - rect
      val candidatesWithHighIntersection = candidates.filter{ candidate =>
        val candidateIntersection = candidate.rectangle.percentageIntersection(rect.rectangle)
        val rectangleIntesection = rect.rectangle.percentageIntersection(candidate.rectangle)
        candidateIntersection > 0.2 || rectangleIntesection > 0.2
      }
      if (log.isTraceEnabled) {
        if (candidatesWithHighIntersection.size>1) {
          log.trace(f"For ${rect.rectangle.coordinates} found overlaps (including already removed) with ${candidatesWithHighIntersection.map(_.rectangle.coordinates).mkString(", ")}")
        }
      }
      val notYetRemoved = candidatesWithHighIntersection.diff(toRemove)
      if (log.isDebugEnabled) {
        if (notYetRemoved.size > 1) {
          log.debug(f"For ${rect.rectangle.coordinates} (${rect.confidence}%.2f) found overlaps with ${notYetRemoved.map(r => f"${r.rectangle.coordinates} (${r.confidence}%.2f)").mkString(", ")}")
        }
      }
      val (higherConfidence, lowerConfidence) = notYetRemoved.partition(_.confidence > rect.confidence)


      if (higherConfidence.size > 0) {
        if (log.isDebugEnabled) {
          log.debug(f"Removing current: ${rect.rectangle.coordinates}")
        }
        toRemove + rect
      } else if (lowerConfidence.size > 0) {
        if (log.isDebugEnabled) {
          log.debug(f"Removing others: ${lowerConfidence.map(_.rectangle.coordinates).mkString(", ")}")
        }
        toRemove ++ lowerConfidence
      } else {
        toRemove
      }
    }

    if (log.isDebugEnabled) {
      log.debug(f"Removing overlapping rectangles: ${toRemove.map(_.rectangle.coordinates).mkString(", ")}")
    }

    rects.filter(!toRemove.contains(_))
  }
}
