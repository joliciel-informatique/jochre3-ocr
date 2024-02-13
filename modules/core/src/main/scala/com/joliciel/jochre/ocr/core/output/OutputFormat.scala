package com.joliciel.jochre.ocr.core.output

import com.joliciel.jochre.ocr.core.model.Page
import enumeratum._

import scala.xml.PrettyPrinter

sealed trait OutputFormat extends EnumEntry {
  def suffix: String

  def apply(fileName: String, pages: Seq[Page]): String
}

object OutputFormat extends Enum[OutputFormat] {
  val values = findValues

  case object Alto4 extends OutputFormat {
    override val suffix: String = "_alto4.xml"

    override def apply(fileName: String, pages: Seq[Page]): String = {
      val altoWriter = Alto4Writer(pages, fileName)
      val alto = altoWriter.alto
      val prettyPrinter = new PrettyPrinter(80, 2)
      prettyPrinter.format(alto)
    }
  }

  case object Text extends OutputFormat {
    override val suffix: String = ".txt"

    override def apply(fileName: String, pages: Seq[Page]): String = {
      pages.map {
        _.content
      }.mkString("\n")
    }
  }
}