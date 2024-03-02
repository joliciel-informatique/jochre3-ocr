package com.joliciel.jochre.ocr.core.graphics

import org.slf4j.LoggerFactory

/**
 * Block sorting algorithm.
 *
 * For example:
 * -------------------
 * |     |  2  |  1  |
 * |     |-----------|
 * |  6  |    3      |
 * |     |-----------|
 * |     |  5  |     |
 * |-----------|     |
 * |     7     |  4  |
 * |-----------|     |
 * |  9  |  8  |     |
 * -------------------
 *
 * We call a "vertical break", a block which overlaps two or more blocks horizontally.
 * Above, 3 and 7 are vertical breaks.
 * We order the blocks such that: if a vertical break separates two blocks vertically and overlaps them horizontally, we order top-down:
 * e.g. 2 comes before 4, 6 comes before 8.
 * If on the other hand, there is no such vertical break then:
 * If there is horizontal overlap, we use top-down ordering:
 * e.g. 3 comes before 4.
 * If there is no horizontal overlap, we use right-to-left ordering:
 * e.g. 3 comes before 6, 4 comes before 7, 4 comes before 5, 4 comes before 6, 5 comes before 6
 */
case class BlockSorter(blocks: Seq[WithRectangle], leftToRight: Boolean) extends Ordering[WithRectangle] {
  private val log = LoggerFactory.getLogger(getClass)
  private val topOrdered = blocks.sortBy(_.top)

  def compare(a: WithRectangle, b: WithRectangle): Int = {
    val (topBlock, bottomBlock) = if (a.top < b.top) {
      (a, b)
    } else {
      (b, a)
    }

    val belowTop = topOrdered
      .dropWhile(rect => rect.top < topBlock.bottom)

    val betweenBlocks = belowTop
      .takeWhile(rect => rect.top < bottomBlock.top)

    val verticalBreak = betweenBlocks.find(block => block.horizontalOverlap(topBlock) > 0 && block.horizontalOverlap(bottomBlock) > 0 &&
      block.verticalOverlap(topBlock) == 0 && block.verticalOverlap(bottomBlock) == 0
    )

    if (log.isTraceEnabled) {
      log.trace("Comparing")
      log.trace(f"${a.rectangle.coordinates}. ${if (a.top < b.top) {"top"} else {"bottom"}}")
      log.trace(f"${b.rectangle.coordinates}. ${if (a.top < b.top) {"bottom"} else {"top"}}")
      log.trace(f"Between blocks: ${betweenBlocks.size}")
      log.trace(f"Vertical break: ${verticalBreak.map(_.rectangle.coordinates)}")
    }
    if (verticalBreak.isDefined) {
      val result = a.rectangle.verticalCompare(b.rectangle)
      if (log.isTraceEnabled) {
        val comparator = if (result < 0) { "<" } else if (result > 0) { ">" } else { "=" }
        log.trace(f"With vertical break: vertical compare. ${a.rectangle.coordinates} $comparator ${b.rectangle.coordinates}")
      }
      result
    } else {
      if (a.horizontalOverlap(b) > 0) {
        val result = a.rectangle.verticalCompare(b.rectangle)
        if (log.isTraceEnabled) {
          val comparator = if (result < 0) { "<" } else if (result > 0) { ">" } else { "=" }
          log.trace(f"With horizontal overlap: vertical compare. ${a.rectangle.coordinates} $comparator ${b.rectangle.coordinates}")
        }
        result
      } else {
        val result = a.rectangle.horizontalCompare(b.rectangle, leftToRight)
        if (log.isTraceEnabled) {
          val comparator = if (result < 0) { "<" } else if (result > 0) { ">" } else { "=" }
          log.trace(f"No horizontal overlap: horizontal compare. ${a.rectangle.coordinates} $comparator ${b.rectangle.coordinates}")
        }
        result
      }
    }
  }
}

object BlockSorter {
  def sort(blocks: Seq[WithRectangle], leftToRight: Boolean): Seq[WithRectangle] = {
    blocks.sorted(BlockSorter(blocks, leftToRight))
  }
}