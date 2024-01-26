package com.joliciel.jochre.ocr.core

import java.io.File

package object evaluation {
  case class EvaluationResult(file: File, results: Map[String, Double])
}
