package com.joliciel.jochre.ocr.core.model

import scala.xml.{Elem, Node}

case class LayoutTag(id: String, label: String) extends Tag {
  def toXml: Elem =
    <LayoutTag ID={id} LABEL={label}></LayoutTag>
}

object LayoutTag {
  def fromXML(node: Node): LayoutTag = {
    val id = node \@ "ID"
    val label = node \@ "LABEL"
    LayoutTag(id, label)
  }
}
