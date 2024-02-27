package com.joliciel.jochre.ocr.core.model

import scala.xml.{Elem, Node}

case class TextStyle(id: String, fontFamily: Option[String] = None, fontType: Option[String] = None, fontWidth: Option[String] = None, fontSize: Option[Double] = None) {
  def toXml: Elem =
      <TextStyle ID={id} FONTFAMILY={fontFamily.orNull} FONTTYPE={fontType.orNull} FONTWIDTH={fontWidth.orNull} FONTSIZE={fontSize.map(size => f"$size%.1f").orNull}></TextStyle>
}

object TextStyle {
  def fromXML(node: Node): TextStyle = {
    val id = node \@ "ID"
    val fontFamily = node \@ "FONTFAMILY"
    val fontFamilyOption = Option.when(fontFamily.nonEmpty)(fontFamily)
    val fontType = node \@ "FONTTYPE"
    val fontTypeOption = Option.when(fontType.nonEmpty)(fontType)
    val fontWidth = node \@ "FONTWIDTH"
    val fontWidthOption = Option.when(fontWidth.nonEmpty)(fontWidth)
    val fontSize = node \@ "FONTSIZE"
    val fontSizeOption = Option.when(fontSize.nonEmpty)(fontSize.toDoubleOption.getOrElse(12.0))
    TextStyle(id, fontFamily=fontFamilyOption, fontType=fontTypeOption, fontWidth=fontWidthOption, fontSize=fontSizeOption)
  }
}
