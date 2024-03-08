package com.joliciel.jochre.ocr.core.model

import scala.xml.{Elem, Node}

case class SpellingAlternative(purpose: String, content: String) extends AltoElement {
  def toXml: Elem =
    <ALTERNATIVE PURPOSE={purpose}>{content}</ALTERNATIVE>

  override def transform(
      partialFunction: PartialFunction[AltoElement, AltoElement]
  ): SpellingAlternative = {
    val transformed = if (partialFunction.isDefinedAt(this)) {
      partialFunction(this).asInstanceOf[SpellingAlternative]
    } else { this }
    transformed
  }
}

object SpellingAlternative {
  def fromXML(node: Node): SpellingAlternative = {
    import com.joliciel.jochre.ocr.core.utils.XmlImplicits._
    val purpose = node \@ "PURPOSE"
    val content = node.textContent
    SpellingAlternative(purpose, content)
  }
}
