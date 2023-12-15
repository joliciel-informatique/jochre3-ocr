package com.joliciel.jochre.ocr.core.text

import com.joliciel.jochre.ocr.core.model.Page
import com.joliciel.jochre.ocr.core.utils.OutputLocation
import org.bytedeco.opencv.opencv_core.Mat
import zio.Task

trait TextGuesser {
  /**
   * Given an image and a pre-segmented [[Page]] structure, attempt to guess the text within the page
   * by assigning content to the resulting page.
   */
  def guess(page: Page, mat: Mat, fileName: String, outputLocation: Option[OutputLocation] = None): Task[Page]
}

trait TextGuesserService {
  def getTextGuesser(): Task[TextGuesser]
}