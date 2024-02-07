package com.joliciel.jochre.ocr.core.alto

import com.joliciel.jochre.ocr.core.corpus.TextSimplifier
import com.joliciel.jochre.ocr.core.model.{SpellingAlternative, AltoElement, ComposedBlock, Glyph, Hyphen, Page, TextBlock, Word}
import com.joliciel.jochre.ocr.core.utils.XmlImplicits

import java.io.{File, Reader}
import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Attribute, Elem, Node, Text, XML}

/**
 * Given Alto XML as input, transforms it to produce appropriate output, including in particular the possibility of adding String alternatives.
 */
trait AltoTransformer extends XmlImplicits {
  def removeGlyphs: Boolean = false
  def textSimplifier: Option[TextSimplifier] = None

  def process(altoFile: File): Seq[Page] = {
    val elem = XML.loadFile(altoFile)
    val pageElements = elem \\ "Page"
    val pages = pageElements.map(Page.fromXML(_))
    pages.map(process(_))
  }

  def process(altoFile: Reader): Seq[Page] = {
    val elem = XML.load(altoFile)
    val pageElements = elem \\ "Page"
    val pages = pageElements.map(Page.fromXML(_))
    pages.map(process(_))
  }

  def process(alto: Page): Page = {
    val simplified = textSimplifier.map(textSimplifier => alto.transform(simplifyContent(textSimplifier))).getOrElse(alto)

    val withSpecificRulesApplied = getSpecificRules.foldLeft(simplified){ case (alto, rule) =>
      alto.transform(rule)
    }

    val withoutGlyphs = if (removeGlyphs) {
      withSpecificRulesApplied.transform(glyphRemover)
    } else {
      withSpecificRulesApplied
    }

    val withAlternatives = withoutGlyphs.transform(addStringAlternatives)

    withAlternatives
  }

  def getSpecificRules: Seq[PartialFunction[AltoElement, AltoElement]] = Seq.empty

  val addStringAlternatives: PartialFunction[AltoElement, AltoElement] = {
    case word: Word =>
      val newAlternatives = getAlternatives(word.content)
      val allAlternatives = (newAlternatives ++ word.alternatives).toSeq.sortBy(a => (a.purpose, a.content))
      word.copy(alternatives = allAlternatives)
  }

  def simplifyContent(textSimplifier: TextSimplifier): PartialFunction[AltoElement, AltoElement] = {
    case word: Word =>
      word.copy(rectangle = word.rectangle.copy(label = textSimplifier.simplify(word.rectangle.label)))
    case glyph: Glyph =>
      glyph.copy(rectangle = glyph.rectangle.copy(label = textSimplifier.simplify(glyph.rectangle.label)))
    case hyphen: Hyphen =>
      hyphen.copy(rectangle = hyphen.rectangle.copy(label = textSimplifier.simplify(hyphen.rectangle.label)))
  }

  def glyphRemover: PartialFunction[AltoElement, AltoElement] = {
    case word: Word =>
      word.copy(glyphs = Seq.empty)
  }

  def getAlternatives(content: String): Set[SpellingAlternative]
}
