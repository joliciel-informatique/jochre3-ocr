package com.joliciel.jochre.ocr.core.segmentation

import com.joliciel.jochre.ocr.core.model.Page
import com.joliciel.jochre.ocr.core.utils.OutputLocation
import org.bytedeco.opencv.opencv_core.Mat
import zio.Task

trait Segmenter {
  /** Transform an image into a segmented [[Page]] structure.
   * The page might only be segmented down to a given level (e.g. blocks, lines, strings, or glyphs) */
  def segment(mat: Mat, fileName: String, debugLocation: Option[OutputLocation] = None): Task[Page]
}

trait SegmenterService {
  def getSegmenter(): Task[Segmenter]
}