package com.joliciel.jochre.ocr.api.analysis

import com.joliciel.jochre.ocr.api.Types.Requirements
import com.joliciel.jochre.ocr.api.{HttpError, HttpErrorMapper}
import com.joliciel.jochre.ocr.core.Jochre
import org.slf4j.LoggerFactory
import sttp.model.Part
import zio._
import zio.stream.{ZPipeline, ZStream}

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import scala.xml.PrettyPrinter

trait AnalysisLogic extends HttpErrorMapper {
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
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to post file to analyze", error))
      .mapError(mapToHttpError(_))
}
