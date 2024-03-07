package com.joliciel.jochre.ocr.core.transform

import com.joliciel.jochre.ocr.core.utils.ImageUtils
import org.bytedeco.opencv.opencv_core.Mat

class BrightnessAndContrastTransform(contrast: Double, brightness: Int)
    extends ImageTransformer[Unit]
    with ImageUtils {
  override def transform(path: String, mat: Mat): (Mat, Unit) = {
    changeContrastAndBrightness(mat, contrast, brightness) -> ()
  }
}
