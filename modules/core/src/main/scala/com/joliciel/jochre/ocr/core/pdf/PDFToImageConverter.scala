package com.joliciel.jochre.ocr.core.pdf

import org.apache.pdfbox.Loader
import org.apache.pdfbox.io.RandomAccessReadBuffer
import org.apache.pdfbox.rendering.{ImageType, PDFRenderer}
import org.slf4j.LoggerFactory
import zio.stream.ZStream
import zio.{Task, ZIO}

import java.awt.image.BufferedImage
import java.io.InputStream

case class PDFToImageConverter(source: () => InputStream, startPage: Option[Int] = None, endPage: Option[Int] = None, dpi: Option[Int] = None) {
  private val log = LoggerFactory.getLogger(getClass)


  def process[T](convert: (BufferedImage, Int) => Task[T]): ZStream[Any, Throwable, T] = {
    ZStream.acquireReleaseWith(ZIO.attempt {
      val inputStream = source()
      val pdfStream = new RandomAccessReadBuffer(inputStream)
      val document = Loader.loadPDF(pdfStream)
      val pdfRenderer = new PDFRenderer(document)
      (inputStream, document, pdfRenderer)
    }) { case (inputStream, document, _) =>
      ZIO.attempt{
        document.close()
        inputStream.close()
      }.orDieWith { ex =>
          log.error("Cannot close document", ex)
          ex
        }
    }.flatMap {
      case (_, document, pdfRenderer) =>
        val pageCount = document.getNumberOfPages
        val start = startPage.getOrElse(1)
        val end = endPage.map(endPage => if (pageCount < endPage) { pageCount } else { endPage }).getOrElse(pageCount)
        ZStream.fromIterable(start to end)
          .mapZIO { i =>
            ZIO.attempt {
              log.info(f"Extracting image $i")
              val image = pdfRenderer.renderImageWithDPI(i-1, dpi.getOrElse(300).toFloat, ImageType.RGB)
              image -> i
            }
          }
          .mapZIO { case (image, i) =>
            log.info(f"Converting image $i")
            convert(image, i)
          }
    }
  }
}