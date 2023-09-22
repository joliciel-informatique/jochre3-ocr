package com.joliciel.jochre.ocr.core.corpus

import com.joliciel.jochre.ocr.core.utils.FileUtils.recursiveListImages
import com.joliciel.jochre.ocr.core.utils.{FileUtils, OpenCvUtils}
import org.rogach.scallop.{ScallopConf, ScallopOption}
import org.slf4j.LoggerFactory

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

case class TextLineExtractor(altoFinder: AltoFinder = AltoFinder.default) extends OpenCvUtils {
  private val log = LoggerFactory.getLogger(getClass)

  def transform(corpusDir: Path, outDir: Path, debugDir: Option[Path], keepStructure: Boolean, maxFiles: Option[Int], extension: String, fileList: Option[Set[String]]) = {
    val corpusFiles = recursiveListImages(corpusDir.toFile)

    val locations = corpusFiles
      .filter(location => fileList.map(_.contains(Path.of(location).getFileName.toString)).getOrElse(true))
      .take(maxFiles.getOrElse(corpusFiles.size))

    val initialTransforms = List[AnnotatedImageTransformer[_]](
      RotationTransformer
    )

    locations.map{ location =>
      val mat = loadImage(location.getPath)
      val alto = altoFinder.getAltoPage(Path.of(location))

      val (transformedMat, transformedAlto)= initialTransforms.foldLeft(mat -> alto) {
        case ((mat, alto), transformer) =>
          val (newMat, newAlto, _) = transformer.transform(location.getPath, mat, alto)
          newMat -> newAlto
      }

      val textLineWithRectangles = transformedAlto.textBlocks.flatMap(_.textLinesWithRectangles) ++
        transformedAlto.composedBlocks.flatMap(_.textBlocks.flatMap(_.textLinesWithRectangles))

      val filePath = Path.of(location)
      val outFile =
        if (keepStructure) {
          outDir.resolve(corpusDir.relativize(filePath)).toFile
        } else {
          new File(outDir.toFile, filePath.toFile.getName)
        }

      val parentDir = outFile.getParentFile
      parentDir.mkdirs()

      val baseName = FileUtils.removeFileExtension(outFile.getName)

      debugDir.foreach(_.toFile.mkdirs())
      debugDir.foreach(debugDir => saveImage(transformedMat, new File(debugDir.toFile, f"${baseName}_rotated.png").getPath))

      textLineWithRectangles.zipWithIndex.map{ case ((textLine, rectangle), i) =>
        log.debug(f"Next textLine: $rectangle")
        val cropped = crop(transformedMat, rectangle)
        val content = textLine.content

        val fileNameBase = f"${baseName}_${"%03d".format(i)}"
        val imageFileName = f"${fileNameBase}.${extension}"
        val textFileName = f"${fileNameBase}.txt"

        val imageFile = new File(parentDir, imageFileName)
        val textFile = new File(parentDir, textFileName)

        saveImage(cropped, imageFile.getPath)
        Files.write(Paths.get(textFile.getPath), content.getBytes(StandardCharsets.UTF_8))
      }
    }
  }
}

object TextLineExtractor {
  class TextLineExtractorCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val corpusDir: ScallopOption[String] = opt[String](required = true, descr = "The directory containing original images and labels in Alto4 format")
    val outDir: ScallopOption[String] = opt[String](required = true, descr = "The directory where the processed images will be placed")
    val debugDir: ScallopOption[String] = opt[String](required = false, descr = "A directory where to write debug images")
    val keepStructure: ScallopOption[Boolean] = opt[Boolean](descr = "If present, keep the sub-directory structure within the out-dir")
    val maxFiles = opt[Int](descr = "If present, only transform this many files at most")
    val extension: ScallopOption[String] = choice(Seq("png", "jpg"), default = Some("png"))
    val fileList: ScallopOption[String] = opt[String](required = false, descr = "If present, limit the files to this list only")
    verify()
  }

  def main(args: Array[String]): Unit = {
    val options = new TextLineExtractorCLI(args.toIndexedSeq)

    val corpusDir = new File(options.corpusDir())
    val corpusPath = corpusDir.toPath
    val outDir = new File(options.outDir())
    outDir.mkdirs()
    val outPath = outDir.toPath

    val debugDir = options.debugDir.toOption.map(debugDir => (new File(debugDir)).toPath)

    val fileList = options.fileList.toOption.map(FileUtils.readFile(_).toSet)

    val extension = options.extension()

    val textLineExtractor = TextLineExtractor()
    textLineExtractor.transform(corpusPath, outPath, debugDir, options.keepStructure(), options.maxFiles.toOption, extension, fileList)
  }
}
