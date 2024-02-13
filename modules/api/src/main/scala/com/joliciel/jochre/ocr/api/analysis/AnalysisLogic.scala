package com.joliciel.jochre.ocr.api.analysis

import org.apache.commons.io.FilenameUtils
import com.joliciel.jochre.ocr.api.Types.Requirements
import com.joliciel.jochre.ocr.api.{HttpError, HttpErrorMapper}
import com.joliciel.jochre.ocr.core.Jochre
import com.joliciel.jochre.ocr.core.output.OutputFormat
import com.joliciel.jochre.ocr.core.utils.{FileUtils, ImageUtils}
import org.slf4j.LoggerFactory
import sttp.model.Part
import zio._
import zio.stream.{ZPipeline, ZStream}

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import scala.xml.PrettyPrinter

trait AnalysisLogic extends HttpErrorMapper with ImageUtils with FileUtils {
  private val log = LoggerFactory.getLogger(getClass)

  def postAnalyzeFileLogic(fileForm: FileForm): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] = {
    val fileName = fileForm.image.fileName.getOrElse("Unknown")
    val getAlto = if (fileName.endsWith(".pdf")) {
      (for {
        pdfFile <- ZIO.attempt {
          fileForm match {
            case FileForm(Part(_, body, _, _), _, _, _) =>
              body
          }
        }
        jochre <- ZIO.service[Jochre]
        pages <- jochre.processPdf(
          pdfFile.toPath,
          fileName = Some(fileName),
          startPage = fileForm.start,
          endPage = fileForm.end,
          dpi = fileForm.dpi
        )
      } yield pages)
    } else {
      (for {
        image <- ZIO.attempt {
          fileForm match {
            case FileForm(Part(_, body, _, _), _, _, _) =>
              ImageIO.read(body)
          }
        }
        jochre <- ZIO.service[Jochre]
        page <- jochre.processImage(image, fileName)
      } yield Seq(page))
    }

    (for {
      pages <- getAlto
      altoXml <- ZIO.attempt(OutputFormat.Alto4.apply(fileName, pages))
    } yield {
      log.info(f"Analyzed ${fileName}")
      ZStream(altoXml)
        .via(ZPipeline.utf8Encode)
    })
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to analyze file", error))
      .mapError(mapToHttpError(_))
  }

  def postAnalyzeURLLogic(body: AnalyseURLRequest): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] = {
    val fileName = body.fileName.getOrElse(FilenameUtils.getName(body.url))
    val getAlto = if (fileName.endsWith(".pdf")) {
      (for {
        pdfFile <- ZIO.fromTry {
          getFileFromUrl(body.url)
        }
        jochre <- ZIO.service[Jochre]
        pages <- jochre.processPdf(
          pdfFile.toPath,
          fileName = Some(fileName),
          startPage = body.start,
          endPage = body.end,
          dpi = body.dpi
        )
      } yield pages)
    } else {
      (for {
        image <- ZIO.fromTry {
          getImageFromUrl(body.url)
        }
        jochre <- ZIO.service[Jochre]
        page <- jochre.processImage(image, fileName)
      } yield Seq(page))
    }

    (for {
      pages <- getAlto
      altoXml <- ZIO.attempt(OutputFormat.Alto4.apply(fileName, pages))
    } yield {
      log.info(f"Analyzed ${fileName}")
      ZStream(altoXml)
        .via(ZPipeline.utf8Encode)
    })
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to analyze file", error))
      .mapError(mapToHttpError(_))
  }
}
