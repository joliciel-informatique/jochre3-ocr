package com.joliciel.jochre.ocr.api

import sttp.model.Part

import java.io.File

package object analysis {
  case class FileForm(image: Part[File])
}
