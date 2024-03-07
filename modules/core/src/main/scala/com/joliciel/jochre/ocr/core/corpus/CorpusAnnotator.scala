package com.joliciel.jochre.ocr.core.corpus

import com.joliciel.jochre.ocr.core.model.{Alto, Page}
import com.joliciel.jochre.ocr.core.utils.{FileUtils, ImageUtils}
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory

import java.io.File
import java.nio.file.Path

/** Given a corpus of images pre-annotated with Alto, such that the AltoFinder makes it possible to
  * find the equivalent Alto file, transforms this corpus into another annotated format. This could
  * involve splitting images (e.g. into individual text lines), and writing the associated
  * annotation into a file of a given format, as required by a particular downstream tool (e.g.
  * YOLO, TrOCR, etc.).
  */
trait CorpusAnnotator extends FileUtils with ImageUtils {
  def altoFinder: AltoFinder
  def corpusDir: Path
  def outDir: Path
  def maxFiles: Option[Int]
  def fileList: Option[Set[String]]
  def keepStructure: Boolean = false

  private val log = LoggerFactory.getLogger(getClass)

  val initialTransforms: Seq[AnnotatedImageTransformer[_]] = Seq[AnnotatedImageTransformer[_]](
    RotationTransformer
  )

  def annotate(): Unit = {
    try {
      val corpusFiles = recursiveListImages(corpusDir.toFile)

      val locations = corpusFiles
        .filter(location => fileList.map(_.contains(location.getName)).getOrElse(true))
        .take(maxFiles.getOrElse(corpusFiles.size))

      locations.zipWithIndex.foreach { case (location, i) =>
        log.info(f"About to annotate ${location.getPath}")
        val mat = loadImage(location.toPath)
        val alto = altoFinder.getAlto(location.toPath)
        val page = alto.pages.head

        val (transformedMat, transformedPage) =
          initialTransforms.foldLeft(mat -> page) { case ((mat, page), transformer) =>
            val (newMat, newPage, _) =
              transformer.transform(location.getPath, mat, page)
            newMat -> newPage
          }

        val filePath = location.toPath
        val fileName = if (keepStructure) {
          corpusDir.relativize(filePath).toString
        } else {
          filePath.toFile.getName
        }

        val parentDir = outDir.toFile
        parentDir.mkdirs()

        val baseName = FileUtils.removeFileExtension(fileName)

        val transformedAlto = alto.copy(pages = Seq(transformedPage))
        this.annotateOneFile(
          transformedMat,
          transformedAlto,
          parentDir,
          baseName,
          i
        )
      }
    } finally {
      this.cleanUp()
    }
  }

  def annotateOneFile(
      mat: Mat,
      alto: Alto,
      parentDir: File,
      baseName: String,
      index: Int
  ): Unit

  def cleanUp(): Unit = {}
}
