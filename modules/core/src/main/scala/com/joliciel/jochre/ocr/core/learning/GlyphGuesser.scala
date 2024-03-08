package com.joliciel.jochre.ocr.core.learning

import ai.djl.Model
import ai.djl.modality.Classifications
import ai.djl.modality.Classifications.Classification
import ai.djl.modality.cv.transform.{Resize, ToTensor}
import ai.djl.modality.cv.translator.ImageClassificationTranslator
import ai.djl.modality.cv.{Image, ImageFactory}
import com.joliciel.jochre.ocr.core.model.Glyph
import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.utils.{FileUtils, ImageUtils}
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory

import java.awt.image.BufferedImage
import java.nio.file.Path
import scala.io.Source
import scala.jdk.CollectionConverters._

case class GlyphGuesser(
    modelDir: Path,
    modelName: String,
    modelType: ModelBuilder.ModelType,
    imageSize: Int = 28
) extends FileUtils
    with ImageUtils {
  private val log = LoggerFactory.getLogger(getClass)

  private val classesPath = modelDir.resolve(f"${modelName}_classes.txt")
  private val alphabet = {
    val source = Source.fromFile(classesPath.toFile)
    try {
      source.getLines().toSeq
    } finally {
      source.close()
    }
  }
  private val model = Model.newInstance(modelName)
  model.setBlock(modelType.getModel(alphabet.size, imageSize))
  model.load(modelDir, modelName)
  private val translator = ImageClassificationTranslator.builder
    .addTransform(new Resize(imageSize, imageSize))
    .addTransform(new ToTensor())
    .optSynset(alphabet.asJava)
    .optApplySoftmax(true)
    .optFlag(Image.Flag.GRAYSCALE)
    .build

  private val predictor = model.newPredictor(translator)

  def guess(mat: Mat, glyph: Glyph, k: Int = 5): Seq[Prediction] = {
    val height = (glyph.rectangle.height * 1.1).toInt
    val leftMargin = (height - glyph.rectangle.width) / 2
    val topMargin = (height - glyph.rectangle.height) / 2
    val rectangle = Rectangle(
      glyph.rectangle.left - leftMargin,
      glyph.rectangle.top - topMargin,
      height,
      height
    )

    val image = crop(mat, rectangle)
    val bufferedImage = toBufferedImage(image)
    val classifications = this.predict(bufferedImage)
    if (log.isDebugEnabled) {
      log.debug(f"expected: ${glyph.content}, $classifications")
    }

    val seq = classifications.topK[Classification](k).asScala.toSeq
    seq
      .map { classification =>
        Prediction(classification.getClassName, classification.getProbability)
      }
      .sortBy(0 - _.confidence)
  }

  private def predict(image: BufferedImage): Classifications = {
    val img = ImageFactory.getInstance.fromImage(image)
    predictor.predict(img)
  }
}
