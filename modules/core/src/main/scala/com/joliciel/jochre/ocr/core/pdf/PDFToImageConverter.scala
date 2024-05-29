package com.joliciel.jochre.ocr.core.pdf

import com.joliciel.jochre.ocr.core.utils.FileUtils
import org.apache.pdfbox.Loader
import org.apache.pdfbox.io.RandomAccessReadBuffer
import org.apache.pdfbox.rendering.{ImageType, PDFRenderer}
import org.rogach.scallop.{ScallopConf, ScallopOption}
import org.slf4j.LoggerFactory
import zio.stream.ZStream
import zio.{Task, ZIO}

import java.awt.image.BufferedImage
import java.io.{FileInputStream, InputStream}
import java.nio.file.Path
import javax.imageio.ImageIO
import scala.collection.compat.immutable.ArraySeq

case class PDFToImageConverter(
    source: () => InputStream,
    startPage: Option[Int] = None,
    endPage: Option[Int] = None,
    dpi: Option[Int] = None
) {
  private val log = LoggerFactory.getLogger(getClass)

  def process[T](
      convert: (BufferedImage, Int) => Task[T]
  ): ZStream[Any, Throwable, T] = {
    ZStream
      .acquireReleaseWith(ZIO.attempt {
        val inputStream = source()
        val pdfStream = new RandomAccessReadBuffer(inputStream)
        val document = Loader.loadPDF(pdfStream)
        val pdfRenderer = new PDFRenderer(document)
        (inputStream, document, pdfRenderer)
      }) { case (inputStream, document, _) =>
        ZIO
          .attempt {
            document.close()
            inputStream.close()
          }
          .orDieWith { ex =>
            log.error("Cannot close document", ex)
            ex
          }
      }
      .flatMap { case (_, document, pdfRenderer) =>
        val pageCount = document.getNumberOfPages
        val start = startPage.getOrElse(1)
        val end = endPage
          .map(endPage =>
            if (pageCount < endPage) { pageCount }
            else { endPage }
          )
          .getOrElse(pageCount)
        ZStream
          .fromIterable(start to end)
          .mapZIO { i =>
            ZIO.attempt {
              log.info(f"Extracting image $i of $pageCount")
              val image = pdfRenderer.renderImageWithDPI(
                i - 1,
                dpi.getOrElse(300).toFloat,
                ImageType.RGB
              )
              image -> i
            }
          }
          .mapZIO { case (image, i) =>
            log.info(f"Converting image $i of $pageCount")
            convert(image, i)
          }
      }
  }
}

object PDFToImageConverter {
  private class CLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val inFile: ScallopOption[String] = opt[String](required = true)
    val outDir: ScallopOption[String] = opt[String](required = true)
    val startPage: ScallopOption[Int] = opt[Int]()
    val endPage: ScallopOption[Int] = opt[Int]()
    val dpi: ScallopOption[Int] = opt[Int]()
    verify()
  }

  def main(args: Array[String]): Unit = {
    val cli = new CLI(ArraySeq.unsafeWrapArray(args))
    val baseName = FileUtils.removeFileExtension(cli.inFile())
    val inFile = Path.of(cli.inFile())
    val outDir = Path.of(cli.outDir())
    val startPage = cli.startPage.toOption
    val endPage = cli.endPage.toOption
    val dpi = cli.dpi.toOption

    val inputSource = () => new FileInputStream(inFile.toFile)

    val converter = PDFToImageConverter(inputSource, startPage, endPage, dpi)

    converter.process((image: BufferedImage, page: Int) => {
      ZIO.attempt {
        val imageFileName = f"${baseName}_$page%04d.png"
        ImageIO.write(image, "png", outDir.resolve(imageFileName).toFile)
      }
    })
  }
}
