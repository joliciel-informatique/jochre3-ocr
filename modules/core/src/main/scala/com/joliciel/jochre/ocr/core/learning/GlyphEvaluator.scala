package com.joliciel.jochre.ocr.core.learning

import com.joliciel.jochre.ocr.core.corpus.{AltoFinder, TextSimplifier}
import com.joliciel.jochre.ocr.core.utils.{FileUtils, ImageUtils}
import org.slf4j.LoggerFactory

import java.nio.file.Path

case class GlyphEvaluator(
    corpusDir: Path,
    outputDir: Path,
    modelDir: Path,
    modelName: String,
    modelType: ModelBuilder.ModelType,
    imageSize: Int = 28,
    altoFinder: AltoFinder = AltoFinder.default,
    textSimplifier: TextSimplifier = TextSimplifier.default
) extends FileUtils
    with ImageUtils {
  private val log = LoggerFactory.getLogger(getClass)

  private val images = listImages(corpusDir)
  private val altoDocuments =
    images.map(image => altoFinder.getAlto(image.toPath))

  private val guesser = GlyphGuesser(modelDir, modelName, modelType, imageSize)

  def evaluate(): Double = {
    val results =
      images.zip(altoDocuments).flatMap { case (imageFile, altoDocument) =>
        val mat = loadImage(imageFile.toPath)
        val page = altoDocument.pages.head
        val (transformedMat, transformedAlto) =
          transforms.foldLeft(mat -> page) { case ((mat, alto), transformer) =>
            val (newMat, newAlto, _) =
              transformer.transform(imageFile.getPath, mat, alto)
            newMat -> newAlto
          }

        val glyphs = transformedAlto.allGlyphs
        val count = glyphs.size

        glyphs.zipWithIndex.map { case (glyph, i) =>
          if (i % 100 == 0) {
            log.info(s"Image ${imageFile.getPath}, glyph $i/$count")
          }
          val predictions = guesser.guess(transformedMat, glyph, 1)
          val expected = textSimplifier.simplify(glyph.content)
          expected == predictions.head.outcome
        }
      }

    results.count(r => r).toDouble / results.size.toDouble
  }
}
