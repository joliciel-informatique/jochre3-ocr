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

  private case class ImageWithName(image: BufferedImage, name: String)

  def postAnalyzeFileLogic(fileForm: FileForm): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] =
    (for {
      image <- ZIO.attempt{
        fileForm match {
          case fileForm@FileForm(Part(_, body, _, _)) =>
            val image = ImageIO.read(body)
            ImageWithName(image, fileForm.image.fileName.getOrElse("Unknown"))
        }
      }
      jochre <- ZIO.service[Jochre]
      altoXml <- jochre.processImage(image.image, None, None, image.name, None)
      alto <- ZIO.attempt{
        val prettyPrinter = new PrettyPrinter(80, 2)
        prettyPrinter.format(altoXml)
      }
    } yield {
      log.info(f"Analyzed ${image.name}")
      ZStream(alto)
        .via(ZPipeline.utf8Encode)
    })
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to analyze file", error))
      .mapError(mapToHttpError(_))

  def postAnalyzeURLLogic(body: AnalyseURLRequest): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] =
    (for {
      image <- ZIO.fromTry {
        getImageFromUrl(body.url)
      }
      imageWithName <- ZIO.attempt {
        val fileName = body.fileName.getOrElse(FilenameUtils.getName(body.url))
        val finalFileName = if (fileName.isEmpty) { "Unknown" } else { fileName }
        ImageWithName(image, finalFileName)
      }
      jochre <- ZIO.service[Jochre]
      altoXml <- jochre.processImage(imageWithName.image, None, None, imageWithName.name, None)
      alto <- ZIO.attempt {
        val prettyPrinter = new PrettyPrinter(80, 2)
        prettyPrinter.format(altoXml)
      }
    } yield {
      log.info(f"Analyzed ${imageWithName.name}")
      ZStream(alto)
        .via(ZPipeline.utf8Encode)
    })
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to analyze URL", error))
      .mapError(mapToHttpError(_))
}
