package com.joliciel.jochre.ocr.api.analysis

import org.apache.commons.io.FilenameUtils
import com.joliciel.jochre.ocr.api.Types.Requirements
import com.joliciel.jochre.ocr.api.{HttpError, HttpErrorMapper}
import com.joliciel.jochre.ocr.core.Jochre
import com.joliciel.jochre.ocr.core.utils.ImageUtils
import org.slf4j.LoggerFactory
import sttp.model.Part
import zio._
import zio.stream.{ZPipeline, ZStream}

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import scala.xml.PrettyPrinter

trait AnalysisLogic extends HttpErrorMapper with ImageUtils {
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
        altoXml <- jochre.processPdf(
          pdfFile.toPath,
          fileName = Some(fileName),
          startPage = fileForm.start,
          endPage = fileForm.end,
          dpi = fileForm.dpi
        )
      } yield altoXml)
    } else {
      (for {
        image <- ZIO.attempt {
          fileForm match {
            case FileForm(Part(_, body, _, _), _, _, _) =>
              ImageIO.read(body)
          }
        }
        jochre <- ZIO.service[Jochre]
        altoXml <- jochre.processImage(image, fileName)
      } yield altoXml)
    }

    (for {
      altoXml <- getAlto
      alto <- ZIO.attempt{
        val prettyPrinter = new PrettyPrinter(80, 2)
        prettyPrinter.format(altoXml)
      }
    } yield {
      log.info(f"Analyzed ${fileName}")
      ZStream(alto)
        .via(ZPipeline.utf8Encode)
    })
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to analyze file", error))
      .mapError(mapToHttpError(_))
  }

  def postAnalyzeURLLogic(body: AnalyseURLRequest): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] = {
    val fileName = body.fileName.getOrElse(FilenameUtils.getName(body.url))
    (for {
      image <- ZIO.fromTry {
        getImageFromUrl(body.url)
      }
      jochre <- ZIO.service[Jochre]
      altoXml <- jochre.processImage(image, fileName)
      alto <- ZIO.attempt {
        val prettyPrinter = new PrettyPrinter(80, 2)
        prettyPrinter.format(altoXml)
      }
    } yield {
      log.info(f"Analyzed ${fileName}")
      ZStream(alto)
        .via(ZPipeline.utf8Encode)
    })
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to analyze URL", error))
      .mapError(mapToHttpError(_))
  }
}
