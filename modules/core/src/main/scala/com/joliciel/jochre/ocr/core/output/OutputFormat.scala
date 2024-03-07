package com.joliciel.jochre.ocr.core.output

import com.joliciel.jochre.ocr.core.model.Alto
import enumeratum._

import scala.xml.PrettyPrinter

sealed trait OutputFormat extends EnumEntry {
  def suffix: String

  def apply(altoXml: Alto): String
}

object OutputFormat extends Enum[OutputFormat] {
  val values: IndexedSeq[OutputFormat] = findValues

  case object Alto4 extends OutputFormat {
    override val suffix: String = "_alto4.xml"

    override def apply(altoXml: Alto): String = {
      val xml = altoXml.toXml
      val prettyPrinter = new PrettyPrinter(120, 2)
      prettyPrinter.format(xml)
    }
  }

  case object Text extends OutputFormat {
    override val suffix: String = ".txt"

    override def apply(altoXml: Alto): String = {
      altoXml.pages
        .map {
          _.content
        }
        .mkString("\n")
    }
  }
}
