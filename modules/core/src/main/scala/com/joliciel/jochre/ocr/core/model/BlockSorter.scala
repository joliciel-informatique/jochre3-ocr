package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle

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
  private val topOrdered = blocks.sortBy(_.top)

  def compare(a: WithRectangle, b: WithRectangle): Int = {
    val (topBlock, bottomBlock) = if (a.top < b.top) {
      (a, b)
    } else {
      (b, a)
    }
    val betweenBlocks = topOrdered.dropWhile(_.top < topBlock.bottom).takeWhile(_.bottom > bottomBlock.top)
    val verticalBreak = betweenBlocks.find(block => block.horizontalOverlap(topBlock) > 0 && block.horizontalOverlap(bottomBlock) > 0)
    if (verticalBreak.isDefined) {
      a.rectangle.verticalCompare(b.rectangle)
    } else {
      if (a.horizontalOverlap(b) > 0) {
        a.rectangle.verticalCompare(b.rectangle)
      } else {
        a.rectangle.horizontalCompare(b.rectangle, leftToRight)
      }
    }
  }
}

object BlockSorter {
  def sort(blocks: Seq[WithRectangle], leftToRight: Boolean): Seq[WithRectangle] = {
    try {
      // The simple sort should work in most simple cases
      blocks.sorted(WithRectangle.SimplePageLayoutOrdering(leftToRight))
    } catch {
      case _: IllegalArgumentException =>
        // In complex cases as shown in the class definition above, we have to take vertical breaks into account
        // We use the slower sort algorithm
        blocks.sorted(BlockSorter(blocks, leftToRight))
    }
  }
}