package com.joliciel.jochre.ocr.api.analysis

import com.joliciel.jochre.ocr.api.Types.Requirements
import com.joliciel.jochre.ocr.api.{HttpError, HttpErrorMapper}
import com.joliciel.jochre.ocr.core.Jochre
import com.joliciel.jochre.ocr.core.output.OutputFormat
import com.joliciel.jochre.ocr.core.utils.{FileUtils, ImageUtils}
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.FilenameUtils
import org.slf4j.LoggerFactory
import sttp.model.Part
import zio._
import zio.stream.{ZPipeline, ZStream}

import java.nio.file.Path
import javax.imageio.ImageIO

trait AnalysisLogic extends HttpErrorMapper with ImageUtils with FileUtils {
  private val log = LoggerFactory.getLogger(getClass)
  private val config = ConfigFactory.load().getConfig("jochre.ocr.api")
  private val savePdfImages = config.getBoolean("save-pdf-images")

  def postAnalyzeFileLogic(
      fileForm: FileForm
  ): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] = {
    val fileName = fileForm.image.fileName.getOrElse("Unknown")
    val getAlto = if (fileName.endsWith(".pdf")) {
      val outputDir = Option.when(savePdfImages) {
        val imageParentDirectory = Path.of(config.getString("image-parent-directory"))
        imageParentDirectory.toFile.mkdirs()
        val baseName = FileUtils.removeFileExtension(fileName)
        val pdfImageDirectory = imageParentDirectory.resolve(baseName)
        pdfImageDirectory.toFile.mkdirs()
        pdfImageDirectory
      }

      for {
        pdfFile <- ZIO.attempt {
          fileForm match {
            case FileForm(Part(_, body, _, _), _, _, _) =>
              body
          }
        }
        jochre <- ZIO.service[Jochre]
        alto <- jochre.processPdf(
          pdfFile.toPath,
          fileName = Some(fileName),
          startPage = fileForm.start,
          endPage = fileForm.end,
          dpi = fileForm.dpi,
          outputDir = outputDir,
          writeImages = savePdfImages
        )
        _ <- ZIO.attempt {
          pdfFile.delete()
        }
      } yield alto
    } else {
      for {
        image <- ZIO.attempt {
          fileForm match {
            case FileForm(Part(_, body, _, _), _, _, _) =>
              try {
                ImageIO.read(body)
              } finally {
                body.delete()
              }
          }
        }
        jochre <- ZIO.service[Jochre]
        alto <- jochre.processImage(image, fileName)
      } yield alto
    }

    (for {
      alto <- getAlto
      altoXml <- ZIO.attempt(OutputFormat.Alto4.apply(alto))
    } yield {
      log.info(f"Analyzed $fileName")
      ZStream(altoXml)
        .via(ZPipeline.utf8Encode)
    })
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to analyze file", error))
      .mapError(mapToHttpError)
  }

  def postAnalyzeURLLogic(
      body: AnalyseURLRequest
  ): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] = {
    val fileName = body.fileName.getOrElse(FilenameUtils.getName(body.url))
    val getAlto = if (fileName.endsWith(".pdf")) {
      val outputDir = Option.when(savePdfImages) {
        val imageParentDirectory = Path.of(config.getString("image-parent-directory"))
        imageParentDirectory.toFile.mkdirs()
        val baseName = FileUtils.removeFileExtension(fileName)
        val pdfImageDirectory = imageParentDirectory.resolve(baseName)
        pdfImageDirectory.toFile.mkdirs()
        pdfImageDirectory
      }

      for {
        pdfFile <- ZIO.fromTry {
          getFileFromUrl(body.url)
        }
        jochre <- ZIO.service[Jochre]
        alto <- jochre.processPdf(
          pdfFile.toPath,
          fileName = Some(fileName),
          startPage = body.start,
          endPage = body.end,
          dpi = body.dpi,
          outputDir = outputDir,
          writeImages = savePdfImages
        )
        _ <- ZIO.attempt(pdfFile.delete())
      } yield alto
    } else {
      for {
        image <- ZIO.fromTry {
          getImageFromUrl(body.url)
        }
        jochre <- ZIO.service[Jochre]
        alto <- jochre.processImage(image, fileName)
      } yield alto
    }

    (for {
      alto <- getAlto
      altoXml <- ZIO.attempt(OutputFormat.Alto4.apply(alto))
    } yield {
      log.info(f"Analyzed $fileName")
      ZStream(altoXml)
        .via(ZPipeline.utf8Encode)
    })
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to analyze file", error))
      .mapError(mapToHttpError)
  }
}
