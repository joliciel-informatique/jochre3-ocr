package com.joliciel.jochre.ocr.core.model

import scala.xml.{Elem, Node}

case class StructureTag(id: String, label: String) extends Tag {
  def toXml: Elem =
    <StructureTag ID={id} LABEL={label}></StructureTag>
}

object StructureTag {
  def fromXML(node: Node): StructureTag = {
    val id = node \@ "ID"
    val label = node \@ "LABEL"
    StructureTag(id, label)
  }
}
