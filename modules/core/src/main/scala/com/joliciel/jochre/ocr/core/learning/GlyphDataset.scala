package com.joliciel.jochre.ocr.core.learning

import ai.djl.basicdataset.cv.ImageDataset.BaseBuilder
import ai.djl.engine.Engine
import ai.djl.modality.cv.transform.{Crop, Resize}
import ai.djl.modality.cv.{Image, ImageFactory}
import ai.djl.ndarray.{NDArray, NDList, NDManager}
import ai.djl.training.dataset
import ai.djl.training.dataset.RandomAccessDataset
import ai.djl.util.Progress
import com.joliciel.jochre.ocr.core.corpus.{AltoFinder, TextSimplifier}
import com.joliciel.jochre.ocr.core.utils.{FileUtils, ImageUtils}
import org.slf4j.LoggerFactory

import java.nio.file.Path
import scala.collection.SortedSet
import scala.util.matching.Regex

case class GlyphDataset(builder: GlyphDatasetBuilder)
    extends RandomAccessDataset(builder)
    with ImageUtils
    with FileUtils {
  private val log = LoggerFactory.getLogger(getClass)

  private val textSimplifier = builder.textSimplifier

  private val images = listImages(builder.corpusDir)
  private val altoDocuments =
    images.map(image => builder.altoFinder.getAlto(image.toPath))

  private val pages = altoDocuments.flatMap(_.pages)
  private val allGlyphs = pages.flatMap {
    _.allWords
      .filter { word =>
        builder.wordSelectionRegex.map(_.matches(word.content)).getOrElse(true)
      }
      .flatMap(_.glyphs)
  }
  private val totalSize = allGlyphs.size
  private val alphabet = SortedSet(
    allGlyphs
      .map(_.content)
      .map(builder.textSimplifier.simplify)*
  )

  private val alphabetToIndex = alphabet.zipWithIndex.toMap

  val classes: Seq[String] = alphabet.toSeq :+ ""

  private def getImages(manager: NDManager): Seq[Array[Float]] =
    images.zip(pages).flatMap { case (imageFile, page) =>
      log.info(f"Loading image ${imageFile.getPath}")

      val mat = loadImage(imageFile.toPath)

      val (transformedMat, transformedAlto) =
        transforms.foldLeft(mat -> page) { case ((mat, alto), transformer) =>
          val (newMat, newAlto, _) =
            transformer.transform(imageFile.getPath, mat, alto)
          newMat -> newAlto
        }

      val bufferedImage = toBufferedImage(transformedMat)
      val array = ImageFactory
        .getInstance()
        .fromImage(bufferedImage)
        .toNDArray(manager, Image.Flag.GRAYSCALE)

      val allGlyphs = for {
        word <- transformedAlto.allWords.filter { word =>
          builder.wordSelectionRegex
            .map(_.matches(word.content))
            .getOrElse(true)
        }
        glyph <- word.glyphs
      } yield glyph

      allGlyphs.map { glyph =>
        val height = (glyph.rectangle.height * 1.1).toInt
        val leftMargin = (height - glyph.rectangle.width) / 2
        val topMargin = (height - glyph.rectangle.height) / 2
        val crop = new Crop(
          glyph.rectangle.left - leftMargin,
          glyph.rectangle.top - topMargin,
          height,
          height
        )
        val cropped = crop.transform(array)
        val resize = new Resize(builder.targetWidth, builder.targetWidth)
        val resized = resize.transform(cropped)
        val normalized = resized.div(255)
        normalized.toFloatArray
      }
    }

  private val manager = Engine.getInstance().newBaseManager()
  private val allImages: Seq[Array[Float]] = getImages(manager)

  override def get(manager: NDManager, index: Long): dataset.Record = {
    val array: NDArray = manager
      .create(allImages(index.toInt))
      .reshape(1, 28, 28)
    val data: NDList = new NDList(array)
    val label: NDList = new NDList(
      manager.create(getClassNumber(index).toFloat)
    )
    new dataset.Record(data, label)
  }

  private def getClassNumber(index: Long): Long = {
    val content = textSimplifier.simplify(allGlyphs(index.toInt).content)
    alphabetToIndex.getOrElse(content, alphabet.size).toLong
  }

  override def availableSize(): Long = totalSize

  override def prepare(progress: Progress): Unit = {}
}

case class GlyphDatasetBuilder(
    corpusDir: Path,
    targetWidth: Int,
    altoFinder: AltoFinder = AltoFinder.default,
    textSimplifier: TextSimplifier = TextSimplifier.default,
    wordSelectionRegex: Option[Regex] = None
) extends BaseBuilder[GlyphDatasetBuilder] {
  override def self(): GlyphDatasetBuilder = this

  def build(): GlyphDataset = GlyphDataset(this)
}
