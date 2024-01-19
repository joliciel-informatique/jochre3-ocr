package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.model.ImageLabel.{Line, PredictedRectangle, Rectangle}
import com.joliciel.jochre.ocr.core.model.{Glyph, Illustration, Page, Space, TextBlock, TextLine, Word}
import com.joliciel.jochre.ocr.core.utils.{ImageUtils, OutputLocation}
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import zio.{Task, ZIO, ZLayer}

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

  /** Transform an image into a segmented [[Page]] structure.
   * The page might only be segmented down to a given level (e.g. blocks, lines, strings, or glyphs) */
  override def segment(mat: Mat, fileName: String, debugLocation: Option[OutputLocation]): Task[Page] = {
    for {
      blockPredictor <- yoloPredictorService.getYoloPredictor(YoloPredictionType.Blocks, mat, fileName, debugLocation, Some(0.20))
      blockPredictions <- blockPredictor.predict()
      pageWithBlocks <- ZIO.attempt {
        val blocks = blockPredictions.flatMap {
          case PredictedRectangle(rect@Rectangle(label, _, _, _, _), _) =>
            val blockType = BlockType.withName(label)
            blockType match {
              case BlockType.TextBox => Some(TextBlock(rect, Seq.empty))
              case BlockType.Paragraph => Some(TextBlock(rect, Seq.empty))
              case BlockType.Image => Some(Illustration(rect))
              case BlockType.Table => None
            }
        }.sortBy(_.rectangle)

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
      linePredictor <- yoloPredictorService.getYoloPredictor(YoloPredictionType.Lines, printAreaMat, fileName, debugLocation, Some(0.05))
      linePredictions <- linePredictor.predict()
      wordPredictor <- yoloPredictorService.getYoloPredictor(YoloPredictionType.Words, printAreaMat, fileName, debugLocation, Some(0.05))
      wordPredictions <- wordPredictor.predict()
      glyphPredictor <- yoloPredictorService.getYoloPredictor(YoloPredictionType.Glyphs, printAreaMat, fileName, debugLocation, Some(0.10))
      glyphPredictions <- glyphPredictor.predict()
      page <- ZIO.attempt {
        //TODO: handle case of overlapping text blocks
        val textBlocks = pageWithBlocks.textBlocks
        val illustrations = pageWithBlocks.illustrations

        // Place lines inside blocks
        val translatedLinePredictions = linePredictions.map(p => p.copy(rectangle = p.rectangle.translate(croppedPrintArea.left, croppedPrintArea.top)))
        val textBlockToLineMap = placeRectanglesInTextBlocks(textBlocks, translatedLinePredictions, "TextLine")

        val textBlocksWithLines = textBlocks.map{ textBlock =>
          val myLineRects = textBlockToLineMap.get(textBlock).getOrElse(Seq.empty).sortBy(_.rectangle)(Rectangle.VerticalOrdering)
          val myLineRectsWithoutOverlaps = removeOverlaps(myLineRects)
          val myLines = myLineRectsWithoutOverlaps.map(lineRect => TextLine(Line("", textBlock.rectangle.left, lineRect.rectangle.yCenter, textBlock.rectangle.right, lineRect.rectangle.yCenter), Seq.empty))
          textBlock.copy(textLines = myLines)
        }

        // Place words inside blocks
        val translatedWordPredictions = wordPredictions.map(p => p.copy(rectangle = p.rectangle.translate(croppedPrintArea.left, croppedPrintArea.top)))
        val textBlockToWordMap = placeRectanglesInTextBlocks(textBlocksWithLines, translatedWordPredictions, "Word")

        val textBlocksWithWords = textBlocksWithLines.map{ textBlock =>
          val wordsToInclude = textBlockToWordMap.get(textBlock).getOrElse(Seq.empty)
          val textLineToWordMap = placeRectanglesInTextLines(textBlock, wordsToInclude, "Word")

          textBlock.copy(textLines = textBlock.textLines.map{ textLine =>
            val myWordRects = textLineToWordMap.get(textLine).getOrElse(Seq.empty)
            val myWords = myWordRects.map(rect => Word(rect.rectangle.copy(label=""), Seq.empty, 1.0))
            val wordsAndSpaces = Option.when(myWords.size > 1)(myWords.zip(myWords.tail).flatMap{ case (word, nextWord) =>
              Seq(word, Space(Rectangle("", nextWord.rectangle.right, word.rectangle.top, word.rectangle.left - nextWord.rectangle.right, word.rectangle.height)))
            } :+ myWords.last).getOrElse(myWords)
            textLine.copy(wordsAndSpaces = wordsAndSpaces)
          })
        }

        // Place glyphs inside words
        val translatedGlyphPredictions = glyphPredictions.map(p => p.copy(rectangle = p.rectangle.translate(croppedPrintArea.left, croppedPrintArea.top)))
        //val glyphTestRectangle = Some(Rectangle("", 603, 2175, 2505, 540))
        val glyphTestRectangle = None
        val glyphsToPlace = glyphTestRectangle.map(testRect => translatedGlyphPredictions.filter(glyph => glyph.rectangle.areaOfIntersection(testRect) / glyph.rectangle.area.toDouble > 0.8))
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
              textLine.copy(wordsAndSpaces = textLine.wordsAndSpaces.map {
                case word: Word =>
                  val myGlyphRects = wordToGlyphMap.get(word).getOrElse(Seq.empty)
                  if (!myGlyphRects.isEmpty) {
                    // Take average border between two sequential glyphs as their border
                    val borders = myGlyphRects.zip(myGlyphRects.tail).map {
                      case (firstRect, secondRect) => (firstRect.rectangle.left + secondRect.rectangle.right) / 2
                    }
                    val rightLeftPairs = (myGlyphRects.head.rectangle.right +: borders).zip(borders :+ myGlyphRects.last.rectangle.left)

                    val myGlyphs = rightLeftPairs.map{
                      case (right, left) => Glyph(Rectangle("", left = left, top = word.rectangle.top, width = right-left, height = word.rectangle.height), 1.0)
                    }
                    word.copy(glyphs = myGlyphs)
                  } else {
                    word
                  }
                case space: Space => space
              })
            } else {
              textLine
            }
          }
          textBlock.copy(textLines = textLinesWithGlyphs)
        }

        val newBlocks = (textBlocksWithGlyphs ++ illustrations).sorted

        val page = Page(
          id = fileName,
          height = mat.rows(),
          width = mat.cols(),
          physicalPageNumber = 1,
          rotation = 0,
          language = "yi",
          confidence = 1.0,
          blocks = newBlocks
        )

        page
      }
    } yield page
  }

  private def placeRectanglesInTextBlocks(textBlocks: Seq[TextBlock], rectangles: Seq[PredictedRectangle], itemType: String): Map[TextBlock, Seq[PredictedRectangle]] = {
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

      val candidates = {
        if (log.isDebugEnabled) log.debug("Checking top")
        val topLimit = findLimit(rect.rectangle, textBlocksByTop, ((candidate, contained) => contained.bottom > candidate.top), 0, textBlocks.size-1, -1)
        val topCandidates = textBlocksByTop.take(topLimit+1).toSet
        if (log.isDebugEnabled) log.debug(f"Found top candidates: ${topCandidates.map(_.rectangle.coordinates).mkString(", ")}")
        if (topCandidates.size > 1) {
          if (log.isDebugEnabled) log.debug("Checking bottom")
          val bottomLimit = findLimit(rect.rectangle, textBlocksByBottom, ((candidate, contained) => contained.top < candidate.bottom), 0, textBlocks.size-1, -1)
          val bottomCandidates = textBlocksByBottom.take(bottomLimit+1).toSet
          if (log.isDebugEnabled) log.debug(f"Found bottom candidates: ${bottomCandidates.map(_.rectangle.coordinates).mkString(", ")}")
          val intersection1 = topCandidates.intersect(bottomCandidates)
          if (intersection1.size > 1) {
            if (log.isDebugEnabled) log.debug("Checking left")
            val leftLimit = findLimit(rect.rectangle, textBlocksByLeft, ((candidate, contained) => contained.right > candidate.left), 0, textBlocks.size-1, -1)
            val leftCandidates = textBlocksByLeft.take(leftLimit+1).toSet
            if (log.isDebugEnabled) log.debug(f"Found left candidates: ${leftCandidates.map(_.rectangle.coordinates).mkString(", ")}")
            val intersection2 = intersection1.intersect(leftCandidates)
            if (intersection2.size > 1) {
              if (log.isDebugEnabled) log.debug("Checking right")
              val rightLimit = findLimit(rect.rectangle, textBlocksByRight, ((candidate, contained) => contained.left < candidate.right), 0, textBlocks.size-1, -1)
              val rightCandidates = textBlocksByRight.take(rightLimit+1).toSet
              if (log.isDebugEnabled) log.debug(f"Found right candidates: ${rightCandidates.map(_.rectangle.coordinates).mkString(", ")}")
              val intersection3 = intersection2.intersect(rightCandidates)
              if (intersection3.size > 0) {
                intersection3
              } else {
                Set.empty[TextBlock]
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

      if (log.isDebugEnabled) {
        log.debug(f"Final candidates: ${candidates.map(_.rectangle.coordinates).mkString(", ")}")
      }
      val candidatesWithIntersection = candidates.map{ candidate =>
        val areaOfIntersection = candidate.rectangle.areaOfIntersection(rect.rectangle)
        val percentageIntersection = areaOfIntersection / rect.rectangle.area.toDouble
        if (log.isDebugEnabled) log.debug(f"Candidate: ${candidate.rectangle.coordinates}. Percentage intersection: $percentageIntersection")
        candidate -> percentageIntersection
      }.toSeq.sortBy(0 - _._2)

      val mainCandidate = candidatesWithIntersection.headOption
      val container = mainCandidate.flatMap{ case (mainCandidate, percentageIntersection) =>
        Option.when(percentageIntersection > 0.5)(mainCandidate)
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
      .mapValues(_.sortBy(_.rectangle)(Rectangle.HorizontalOrdering))
      .mapValues(removeOverlaps(_))
      .toMap
  }

  private def placeRectanglesInWords(textLine: TextLine, rectangles: Seq[PredictedRectangle]): Map[Word, Seq[PredictedRectangle]] = {
    val (wordMap, _) = rectangles.foldLeft(Map.empty[Word, Seq[PredictedRectangle]] -> Option.empty[Word]) { case ((wordMap, lastWord), rect) =>
      if (log.isDebugEnabled) log.debug(f"Trying to find word for glyph ${rect.rectangle.coordinates}")
      // Before binary search, check if glyph is in last recognized container (since glyphs are ordered)
      val container = lastWord.filter(_.rectangle.testHorizontalOverlap(rect.rectangle)==0).map(Some(_))
        .getOrElse {
          // Run the binary search
          val containerIndex = findContainer(rect.rectangle, textLine.words.map(_.rectangle), _.testHorizontalOverlap(_), 0, textLine.words.size-1)
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
  private def findLimit(contained: Rectangle, candidates: Seq[TextBlock], canContainTest: (Rectangle, Rectangle) => Boolean, startIndex: Int, endIndex: Int, maxLimit: Int): Int = {
    val testIndex = (startIndex + endIndex) / 2
    val candidate = candidates(testIndex)
    val canContain = canContainTest(candidate.rectangle, contained)

    if (log.isDebugEnabled) log.debug(s"startIndex $startIndex, testIndex $testIndex, endIndex $endIndex, minLimit $maxLimit. Can ${candidate.rectangle.coordinates} contain ${contained.coordinates}? ${canContain}")

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
        val maxConfidenceRect = argMax(group)(_.confidence).headOption.getOrElse(head)
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

  private def argMax[A, B](c: Iterable[A])(f: A => B)(implicit o: Ordering[B]): Iterable[A] = {
    val max = (c map f).max(o)
    c filter {
      f(_) == max
    }
  }
}
