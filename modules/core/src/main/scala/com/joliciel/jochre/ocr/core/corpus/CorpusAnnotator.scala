package com.joliciel.jochre.ocr.core.corpus

import com.joliciel.jochre.ocr.core.model.Page
import com.joliciel.jochre.ocr.core.utils.{FileUtils, OpenCvUtils}
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory

import java.io.File
import java.nio.file.Path

trait CorpusAnnotator extends FileUtils with OpenCvUtils {
  def altoFinder: AltoFinder
  def corpusDir: Path
  def outDir: Path
  def keepStructure: Boolean
  def maxFiles: Option[Int]
  def fileList: Option[Set[String]]

  private val log = LoggerFactory.getLogger(getClass)

  def annotate() = {
    try {
      val corpusFiles = recursiveListImages(corpusDir.toFile)

      val locations = corpusFiles
        .filter(location => fileList.map(_.contains(Path.of(location).getFileName.toString)).getOrElse(true))
        .take(maxFiles.getOrElse(corpusFiles.size))

      val initialTransforms = List[AnnotatedImageTransformer[_]](
        RotationTransformer
      )

      locations.map { location =>
        log.info(f"About to annotate ${location.getPath}")
        val mat = loadImage(location.getPath)
        val alto = altoFinder.getAltoPage(Path.of(location))

        val (transformedMat, transformedAlto) = initialTransforms.foldLeft(mat -> alto) {
          case ((mat, alto), transformer) =>
            val (newMat, newAlto, _) = transformer.transform(location.getPath, mat, alto)
            newMat -> newAlto
        }

        val filePath = Path.of(location)
        val fileName = if (keepStructure) {
          corpusDir.relativize(filePath).toString
        } else {
          filePath.toFile.getName
        }

        val outFile = new File(outDir.toFile, fileName)

        val parentDir = outFile.getParentFile
        parentDir.mkdirs()

        val baseName = FileUtils.removeFileExtension(outFile.getName)

        this.annotateOneFile(transformedMat, transformedAlto, parentDir, baseName)
      }
    } finally {
      this.cleanUp()
    }
  }

  def annotateOneFile(mat: Mat, alto: Page, parentDir: File, baseName: String): Unit

  def cleanUp(): Unit
}
