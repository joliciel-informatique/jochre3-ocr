package com.joliciel.jochre.ocr.core

import java.io.File
import java.net.URI
import scala.io.Source
import scala.util.matching.Regex

package object transform {
  case class SkewAngle(value: Double)
  case class Scale(value: Double)
}
