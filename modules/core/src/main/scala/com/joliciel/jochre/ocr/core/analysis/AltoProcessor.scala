package com.joliciel.jochre.ocr.core.analysis

import com.typesafe.config.ConfigFactory

import java.io.{File, Reader}
import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, Node, Text, XML}

trait AltoProcessor {
  private val config = ConfigFactory.load().getConfig("jochre.ocr")
  val ocrVersion = config.getString("ocr-version")

  def removeGlyphs: Boolean = false

  def process(altoFile: File, fileName: String): Elem = {
    val elem = XML.loadFile(altoFile)
    process(elem, fileName)
  }

  def process(altoFile: Reader, fileName: String): Elem = {
    val elem = XML.load(altoFile)
    process(elem, fileName)
  }

  def process(alto: Elem, fileName: String): Elem = {
    val addAlternativesRule = new RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case elem: Elem if elem.label == "String" =>
          addStringAlternatives(elem)
        case node => node
      }
    }

    val basicRule = new RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case elem: Elem if elem.label == "softwareVersion" =>
          val newChildren = elem.child.map{
            case _: Text => Text(ocrVersion)
            case other => other
          }

          elem.copy(child = newChildren)
        case elem: Elem if elem.label == "fileName" =>
          val newChildren = elem.child.map {
            case _: Text => Text(fileName)
            case other => other
          }

          elem.copy(child = newChildren)
        case node => node
      }
    }

    val allRules = basicRule +: getSpecificRules :+ addAlternativesRule
    val transform = new RuleTransformer(allRules: _*)
    transform(alto).asInstanceOf[Elem]
  }

  def getSpecificRules: Seq[RewriteRule] = Seq.empty

  def addStringAlternatives(altoString: Elem): Elem = altoString match {
    case Elem(_, _, _, _, child @ _*) =>
      val alternatives = (altoString \ "ALTERNATIVE")
        .collect{
          case elem:Elem => AltoAlternative(elem \@ "PURPOSE", elem.text)
        }.toSet

      val shapeNode = (altoString \ "Shape").headOption

      val glyphNodes = if (removeGlyphs) {
        Seq.empty
      } else {
        child.filter{
          case Elem(_, label, _, _, _ @ _*) => label == "Glyph"
          case _ => false
        }.map(Some(_))
      }

      val content = altoString \@ "CONTENT"
      val newAlternatives = getAlternatives(content)
      val allAlternatives = (newAlternatives ++ alternatives).toSeq.sortBy(a => (a.purpose, a.content))

      val alternativesAsNodes = allAlternatives.map {case AltoAlternative(purpose, alternative) =>
          <ALTERNATIVE PURPOSE={purpose}>{alternative}</ALTERNATIVE>
        }.map(Some(_))
      altoString.copy(child = (shapeNode ++ alternativesAsNodes ++ glyphNodes).flatten.toSeq)
  }

  def getAlternatives(content: String): Set[AltoAlternative]
}
