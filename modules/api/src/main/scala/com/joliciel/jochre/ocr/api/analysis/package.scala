package com.joliciel.jochre.ocr.api

import sttp.model.Part

import java.io.File

package object analysis {
  case class FileForm(
      image: Part[File],
      start: Option[Int] = None,
      end: Option[Int] = None,
      dpi: Option[Int] = None
  )

  case class AnalyseURLRequest(
      url: String,
      fileName: Option[String],
      start: Option[Int] = None,
      end: Option[Int] = None,
      dpi: Option[Int] = None
  )

  object AnalysisHelper {
    val analyzeURLRequestExample: AnalyseURLRequest = AnalyseURLRequest(
      url = "https://iiif.archive.org/iiif/nybc200058$8/full/1800,/0/default.jpg",
      fileName = Some("nybc200058_0008.jpg")
    )
  }
}
