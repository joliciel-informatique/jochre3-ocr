package com.joliciel.jochre.ocr.core.transform
import com.joliciel.jochre.ocr.core.utils.OpenCvUtils
import org.bytedeco.opencv.opencv_core.Mat

class GrayscaleTransform extends ImageTransformer[Unit] with OpenCvUtils {
  override def transform(path: String, mat: Mat): (Mat, Unit) = {
    toGrayscale(mat) -> ()
  }
}
