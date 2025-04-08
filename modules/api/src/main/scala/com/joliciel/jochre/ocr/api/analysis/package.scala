package com.joliciel.jochre.ocr.api

import com.joliciel.jochre.ocr.core.output.OutputFormat
import sttp.model.Part

import java.io.File

package object analysis {
  case class FileForm(
      image: Part[File],
      start: Option[Int] = None,
      end: Option[Int] = None,
      dpi: Option[Int] = None,
      removeGlyphs: Option[Boolean] = None
  )

  case class FileFormWithOutputFormats(
      image: Part[File],
      outputFormats: String,
      start: Option[Int] = None,
      end: Option[Int] = None,
      dpi: Option[Int] = None,
      removeGlyphs: Option[Boolean] = None
  )

  case class AnalyseURLRequest(
      url: String,
      fileName: Option[String],
      start: Option[Int] = None,
      end: Option[Int] = None,
      dpi: Option[Int] = None,
      removeGlyphs: Option[Boolean] = None
  )

  case class AnalyseURLRequestWithOutputFormats(
      url: String,
      fileName: Option[String],
      outputFormats: String,
      start: Option[Int] = None,
      end: Option[Int] = None,
      dpi: Option[Int] = None,
      removeGlyphs: Option[Boolean] = None
  )

  case class WordInLexiconResponse(
      frequency: Int
  )

  case class WordFrequency(word: String, frequency: Int)
  case class WordsInLexiconResponse(
      frequencies: Seq[WordFrequency]
  )

  case class StandardizedWordsResponse(
      words: Seq[String]
  )

  case class TextFileForm(
      file: Part[File]
  )

  object AnalysisHelper {
    val analyzeURLRequestExample: AnalyseURLRequest = AnalyseURLRequest(
      url = "https://iiif.archive.org/iiif/nybc200058$8/full/1800,/0/default.jpg",
      fileName = Some("nybc200058_0008.jpg")
    )
    val analyzeURLRequestWithOutputFormatsExample: AnalyseURLRequestWithOutputFormats =
      AnalyseURLRequestWithOutputFormats(
        url = "https://iiif.archive.org/iiif/nybc200058$8/full/1800,/0/default.jpg",
        fileName = Some("nybc200058_0008.jpg"),
        outputFormats = OutputFormat.values.map(_.entryName).mkString(",")
      )
  }
}
