package com.joliciel.jochre.ocr.api.analysis

import com.joliciel.jochre.ocr.api.Types.Requirements
import com.joliciel.jochre.ocr.api.{HttpError, HttpErrorMapper, UnknownOutputFormatException}
import com.joliciel.jochre.ocr.core.Jochre
import com.joliciel.jochre.ocr.core.alto.AltoTransformerOptions
import com.joliciel.jochre.ocr.core.lexicon.Lexicon
import com.joliciel.jochre.ocr.core.model.Alto
import com.joliciel.jochre.ocr.core.output.OutputFormat
import com.joliciel.jochre.ocr.core.utils.{FileUtils, ImageUtils}
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.FilenameUtils
import org.slf4j.LoggerFactory
import sttp.model.Part
import zio._
import zio.stream.{ZPipeline, ZStream}

import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.zip.{ZipEntry, ZipOutputStream}
import javax.imageio.ImageIO
import scala.util.Try

trait AnalysisLogic extends HttpErrorMapper with ImageUtils with FileUtils {
  private val log = LoggerFactory.getLogger(getClass)
  private val config = ConfigFactory.load().getConfig("jochre.ocr.api")
  private val savePdfImages = config.getBoolean("save-pdf-images")

  def postAnalyzeFileLogic(
      fileForm: FileForm
  ): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] = {
    val fileName = fileForm.image.fileName.getOrElse("Unknown")
    val altoTransformerOptions = AltoTransformerOptions().withRemoveGlyphs(fileForm.removeGlyphs)

    val fileGetter: () => Try[File] = () => getFileFromPart(fileForm.image)
    val imageGetter: () => Try[BufferedImage] = () => getImageFromPart(fileForm.image)

    (for {
      alto <- getAlto(
        fileGetter,
        imageGetter,
        fileForm.start,
        fileForm.end,
        fileForm.dpi,
        fileName,
        altoTransformerOptions
      )
      altoXml <- ZIO.attempt(OutputFormat.Alto4.apply(alto))
    } yield {
      log.info(f"Analyzed $fileName")
      ZStream(altoXml)
        .via(ZPipeline.utf8Encode)
    })
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to analyze file", error))
      .mapError(mapToHttpError)
  }

  def postAnalyzeFileWithOutputFormatsLogic(
      fileForm: FileFormWithOutputFormats
  ): ZIO[Requirements, HttpError, (ZStream[Any, Throwable, Byte], String)] = {
    val fileName = fileForm.image.fileName.getOrElse("Unknown")
    val baseName = FileUtils.removeFileExtension(fileName)
    val altoTransformerOptions = AltoTransformerOptions().withRemoveGlyphs(fileForm.removeGlyphs)

    val fileGetter: () => Try[File] = () => getFileFromPart(fileForm.image)
    val imageGetter: () => Try[BufferedImage] = () => getImageFromPart(fileForm.image)

    (for {
      outputFormats <- ZIO.attempt {
        stringToOutputFormats(fileForm.outputFormats)
      }
      alto <- getAlto(
        fileGetter,
        imageGetter,
        fileForm.start,
        fileForm.end,
        fileForm.dpi,
        fileName,
        altoTransformerOptions
      )
      zipFile <- ZIO.attempt {
        toZipByteArray(alto, baseName, outputFormats)
      }
    } yield {
      log.info(f"Analyzed $fileName")
      ZStream.fromIterable(zipFile) -> f"""attachment; filename="$baseName.zip""""
    })
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to analyze file", error))
      .mapError(mapToHttpError)
  }

  private def getFileFromPart(filePart: Part[File]): Try[File] = Try {
    filePart match {
      case Part(_, body, _, _) =>
        body
    }
  }

  private def getImageFromPart(filePart: Part[File]): Try[BufferedImage] = Try {
    filePart match {
      case Part(_, body, _, _) =>
        try {
          ImageIO.read(body)
        } finally {
          body.delete()
        }
    }
  }

  def postAnalyzeURLLogic(
      request: AnalyseURLRequest
  ): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] = {
    val fileName = request.fileName.getOrElse(FilenameUtils.getName(request.url))
    val altoTransformerOptions = AltoTransformerOptions().withRemoveGlyphs(request.removeGlyphs)

    val fileGetter: () => Try[File] = () => getFileFromUrl(request.url)
    val imageGetter: () => Try[BufferedImage] = () => getImageFromUrl(request.url)

    (for {
      alto <- getAlto(
        fileGetter,
        imageGetter,
        request.start,
        request.end,
        request.dpi,
        fileName,
        altoTransformerOptions
      )
      altoXml <- ZIO.attempt(OutputFormat.Alto4.apply(alto))
    } yield {
      log.info(f"Analyzed $fileName")
      ZStream(altoXml)
        .via(ZPipeline.utf8Encode)
    })
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to analyze URL", error))
      .mapError(mapToHttpError)
  }

  def postAnalyzeURLWithOutputFormatsLogic(
      request: AnalyseURLRequestWithOutputFormats
  ): ZIO[Requirements, HttpError, (ZStream[Any, Throwable, Byte], String)] = {
    val fileName = request.fileName.getOrElse(FilenameUtils.getName(request.url))
    val baseName = FileUtils.removeFileExtension(fileName)
    val altoTransformerOptions = AltoTransformerOptions().withRemoveGlyphs(request.removeGlyphs)

    val fileGetter: () => Try[File] = () => getFileFromUrl(request.url)
    val imageGetter: () => Try[BufferedImage] = () => getImageFromUrl(request.url)

    (for {
      outputFormats <- ZIO.attempt {
        stringToOutputFormats(request.outputFormats)
      }
      alto <- getAlto(
        fileGetter,
        imageGetter,
        request.start,
        request.end,
        request.dpi,
        fileName,
        altoTransformerOptions
      )
      zipFile <- ZIO.attempt {
        toZipByteArray(alto, baseName, outputFormats)
      }
    } yield {
      log.info(f"Analyzed $fileName")
      ZStream.fromIterable(zipFile) -> f"""attachment; filename="$baseName.zip""""
    })
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to analyze URL", error))
      .mapError(mapToHttpError)
  }

  private def stringToOutputFormats(outputFormatStr: String): Seq[OutputFormat] = {
    outputFormatStr.split(",").map { outputFormatStr =>
      try {
        OutputFormat.withName(outputFormatStr.strip())
      } catch {
        case ex: NoSuchElementException => throw new UnknownOutputFormatException(ex.getMessage)
      }
    }
  }

  private def getAlto(
      fileGetter: () => Try[File],
      imageGetter: () => Try[BufferedImage],
      start: Option[Int] = None,
      end: Option[Int] = None,
      dpi: Option[Int] = None,
      fileName: String,
      altoTransformerOptions: AltoTransformerOptions
  ): ZIO[Requirements, Throwable, Alto] = {
    if (fileName.endsWith(".pdf")) {
      val outputDir = Option.when(savePdfImages) {
        val imageParentDirectory = Path.of(config.getString("image-parent-directory"))
        imageParentDirectory.toFile.mkdirs()
        val baseName = FileUtils.removeFileExtension(fileName)
        val pdfImageDirectory = imageParentDirectory.resolve(baseName)
        pdfImageDirectory.toFile.mkdirs()
        pdfImageDirectory
      }

      for {
        pdfFile <- ZIO.fromTry(fileGetter())
        jochre <- ZIO.service[Jochre]
        alto <- jochre.processPdf(
          pdfFile.toPath,
          fileName = Some(fileName),
          startPage = start,
          endPage = end,
          dpi = dpi,
          outputDir = outputDir,
          writeImages = savePdfImages,
          altoTransformerOptions = altoTransformerOptions
        )
        _ <- ZIO.attempt {
          pdfFile.delete()
        }
      } yield alto
    } else {
      for {
        image <- ZIO.fromTry(imageGetter())
        jochre <- ZIO.service[Jochre]
        alto <- jochre.processImage(image, fileName, altoTransformerOptions = altoTransformerOptions)
      } yield alto
    }
  }

  private def toZipByteArray(alto: Alto, baseName: String, outputFormats: Seq[OutputFormat]): Array[Byte] = {
    val bos = new ByteArrayOutputStream()
    val zos = new ZipOutputStream(bos)

    outputFormats.foreach { outputFormat =>
      val outputFileName = baseName + outputFormat.suffix
      val output = outputFormat.apply(alto)
      zos.putNextEntry(new ZipEntry(outputFileName))
      zos.write(output.getBytes(StandardCharsets.UTF_8))
      zos.flush()
    }
    zos.finish()
    bos.toByteArray
  }

  def getWordInLexiconLogic(word: String): ZIO[Requirements, HttpError, WordInLexiconResponse] = {
    (for {
      lexicon <- ZIO.service[Lexicon]
      frequency <- ZIO.attempt(lexicon.getFrequency(word))
    } yield {
      WordInLexiconResponse(frequency)
    })
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to check word in lexicon", error))
      .mapError(mapToHttpError)
  }

}
