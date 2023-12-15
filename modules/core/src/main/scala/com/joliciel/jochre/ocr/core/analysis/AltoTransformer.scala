package com.joliciel.jochre.ocr.core.analysis

import com.joliciel.jochre.ocr.core.corpus.TextSimplifier
import com.joliciel.jochre.ocr.core.utils.XmlImplicits

import java.io.{File, Reader}
import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Attribute, Elem, Node, Text, XML}

/**
 * Given Alto XML as input, transforms it to produce appropriate output, including in particular the possibility of adding String alternatives.
 */
trait AltoTransformer extends XmlImplicits {
  val ocrVersion = sys.env.get("JOCHRE3_OCR_VERSION").getOrElse("0.0.1-SNAPSHOT")

  def removeGlyphs: Boolean = false
  def textSimplifier: Option[TextSimplifier] = None

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
          val newChildren = Text(ocrVersion)

          elem.copy(child = newChildren)
        case elem: Elem if elem.label == "fileName" =>
          val newChildren = Text(fileName)

          elem.copy(child = newChildren)
        case withContent: Elem if withContent.label == "String" || withContent.label == "Glyph" || withContent.label == "HYP" =>
          textSimplifier.map{ textSimplifier =>
            val content = withContent \@ "CONTENT"
            val simplifiedContent = textSimplifier.simplify(content)
            val newAttributes = for (attr <- withContent.attributes) yield attr match {
              case attr@Attribute("CONTENT", _, _) =>
                attr.goodCopy(value = simplifiedContent)
              case other => other
            }
            withContent.copy(attributes = newAttributes)
          }.getOrElse(withContent)
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
