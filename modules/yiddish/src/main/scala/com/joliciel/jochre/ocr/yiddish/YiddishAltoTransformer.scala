package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.lexicon.{Lexicon, TextFileLexicon}
import com.joliciel.jochre.ocr.core.analysis.{AltoAlternative, AltoTransformer}
import com.joliciel.jochre.ocr.core.corpus.TextSimplifier
import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle
import com.joliciel.jochre.ocr.core.model.{Glyph, Hyphen}
import com.joliciel.jochre.ocr.core.utils.XmlImplicits
import com.joliciel.jochre.ocr.yiddish.YiddishAltoTransformer.{Purpose, punctuationAndNotRegex}
import com.joliciel.yivoTranscriber.YivoTranscriber
import enumeratum._

import java.io.FileInputStream
import java.util.zip.ZipInputStream
import scala.util.Using
import scala.xml.transform.RewriteRule
import scala.xml.{Attribute, Elem, Node}

case class YiddishAltoTransformer(yiddishConfig: YiddishConfig, override val textSimplifier: Option[TextSimplifier] = Some(YiddishTextSimpifier)) extends AltoTransformer with XmlImplicits {
  private val yivoTranscriber = new YivoTranscriber()
  private val lexicon: Lexicon = {
    Using(new ZipInputStream(new FileInputStream(yiddishConfig.lexiconPath))) { zis =>
      TextFileLexicon.deserialize(zis)
    }
  }.get

  // match an alef if:
  // - it's at the start of word and not immediately followed by a yud, vov, vov yud or tsvey yudn
  // - it's in the middle of word and not immediately followed by a komets or pasekh
  private val shtumerAlef = raw"^א(?![יוײײ ַָ])|(?<!^)א(?![ַָ])".r

  private val nonAbbreviationApostropheRegex = raw"""(?U)['‛](\w\w+)""".r
  override def getAlternatives(content: String): Set[AltoAlternative] = {
    val contentWithoutNonAbbreviationApostrophes = if (punctuationAndNotRegex.findFirstIn(content).isDefined) {
      nonAbbreviationApostropheRegex.pattern.matcher(content).replaceAll("$1")
    } else {
      content
    }
    val yivo = yivoTranscriber.transcribe(contentWithoutNonAbbreviationApostrophes, false)

    val fixedYivo = {
      if (lexicon.getFrequency(yivo)>0 && yivo!="א") {
        yivo
      } else if (shtumerAlef.findFirstIn(yivo).isEmpty) {
        yivo
      } else {
        val alternatives = Seq(yivo)
        // replace each impossible shtumer alef by both a pasekh alef and a komets alef
        val matcher = shtumerAlef.pattern.matcher(yivo)
        val newAlternatives = Iterator.continually(matcher.find()).takeWhile(matched => matched).foldLeft(alternatives){ case (alternatives, _) =>
          alternatives.flatMap{ alternative =>
            val sb = new StringBuilder()
            sb.append(alternative.substring(0, matcher.start))
            sb.append('A')
            sb.append(alternative.substring(matcher.end))
            val sb2 = new StringBuilder()
            sb2.append(alternative.substring(0, matcher.start))
            sb2.append('O')
            sb2.append(alternative.substring(matcher.end))
            Seq(sb.toString(), sb2.toString())
          }
        }.map(_.replace("A", "אַ").replace("O", "אָ"))
        val realWord = newAlternatives.find(lexicon.getFrequency(_) > 0)
        realWord.getOrElse(newAlternatives(0))
      }
    }
    val yivoAlternative = Option.when(fixedYivo != content)(AltoAlternative(Purpose.YIVO.entryName, fixedYivo))
    val romanized = YivoTransliterator.transliterate(fixedYivo)

    val romanizedAlternative = Option.when(romanized != content)(AltoAlternative(Purpose.Roman.entryName, romanized))
    Set(yivoAlternative, romanizedAlternative).flatten
  }

  override val getSpecificRules: Seq[RewriteRule] = Seq(
    Option.when(yiddishConfig.addHyphenElement)(YiddishAltoTransformer.addHyphenRule(textSimplifier)),
    Some(YiddishAltoTransformer.punctuationSplitRule(textSimplifier)),
    Some(YiddishAltoTransformer.reverseNumberRule),
  ).flatten

  override val removeGlyphs: Boolean = true
}

object YiddishAltoTransformer extends XmlImplicits {
  sealed trait Purpose extends EnumEntry

  object Purpose extends Enum[Purpose] {
    val values = findValues

    case object YIVO extends Purpose

    case object Roman extends Purpose
  }

  private val punctuationAndNotRegex = raw"(?U)\p{Punct}[^\p{Punct}]|[^\p{Punct}]\p{Punct}".r
  private val punctuationRegex = raw"(?U)\p{Punct}+".r
  private val quoteRegex = raw"""(?U)[‛“'"]""".r
  private val abbreviationRegex = raw"""(?U)\w+[‛“'"]\w+""".r

  private val dotRegex = raw"""(?U)[\.]""".r
  private val decimalNumberRegex = raw"""(?U)\d+[\.]\d+""".r

  def punctuationSplitRule(textSimplifier: Option[TextSimplifier] = None) = new RewriteRule {
    override def transform(node: Node): Seq[Node] = node match {
      case altoString: Elem if altoString.label == "String" =>
        val content = altoString \@ "CONTENT"
        val confidence = Option.when(!(altoString \@ "WC").isEmpty)((altoString \@ "WC").toDouble).getOrElse(0.0)
        if (punctuationAndNotRegex.findFirstIn(content).isDefined) {
          // split by punctuation
          val glyphs = altoString \ "Glyph"
          val words = glyphs.foldLeft(Seq[Seq[Node]]()) { case (words, glyph) =>
            if (words.isEmpty) {
              Seq(Seq(glyph))
            } else {
              val lastWord = words.last.map(_ \@ "CONTENT").mkString("")
              val glyphContent = glyph \@ "CONTENT"
              val lastWasPunct = punctuationRegex.matches(lastWord)
              val currentIsPunct = punctuationRegex.matches(glyphContent)
              if (currentIsPunct != lastWasPunct) {
                words :+ Seq(glyph)
              } else {
                words.init :+ (words.last :+ glyph)
              }
            }
          }
          val contentSeq = words.map(nodes => nodes.map(_ \@ "CONTENT").mkString(""))
          val contentTriplets = (contentSeq :+ "" :+ "").lazyZip("" +: contentSeq :+ "").lazyZip("" +: "" +: contentSeq).toSeq

          val abbreviationIndexes = contentTriplets.zipWithIndex.flatMap{
            case ((next, current, prev), i) =>
              Option.when(
                (quoteRegex.matches(current) && abbreviationRegex.matches(f"$prev$current$next")) ||
                  (dotRegex.matches(current) && decimalNumberRegex.matches(f"$prev$current$next"))
              )(i-1)
          }.toSet

          val correctedWords = words.zipWithIndex.flatMap{ case(nodes, i) =>
            if (abbreviationIndexes.contains(i)) {
              Some(words(i-1) ++ nodes ++ words(i+1))
            } else if (abbreviationIndexes.contains(i-1)) {
              None
            } else if (abbreviationIndexes.contains(i+1)) {
              None
            } else {
              Some(nodes)
            }
          }

          val wordNodes = correctedWords.map { glyphSeq =>
            glyphsToString(glyphSeq, confidence, textSimplifier)
          }
          wordNodes
        } else {
          altoString
        }
      case other => other
    }
  }

  private def mean(seq: Seq[Double]): Double = if (seq.isEmpty) 0 else seq.sum / seq.size
  private def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }

  private def glyphsToString(glyphSeq: Seq[Node], confidence: Double, textSimplifier: Option[TextSimplifier]): Node = {
    val content = glyphSeq.map(glyph => (glyph \@ "CONTENT")).mkString("")
    val simplifiedContent = textSimplifier.map(_.simplify(content)).getOrElse(content)
    val vpos = glyphSeq.map(glyph => (glyph \@ "VPOS")).map(s => if (s.isEmpty) {
      0
    } else {
      Integer.parseInt(s)
    })
    val hpos = glyphSeq.map(glyph => (glyph \@ "HPOS")).map(s => if (s.isEmpty) {
      0
    } else {
      Integer.parseInt(s)
    })
    val heights = glyphSeq.map(glyph => (glyph \@ "HEIGHT")).map(s => if (s.isEmpty) {
      0
    } else {
      Integer.parseInt(s)
    })
    val widths = glyphSeq.map(glyph => (glyph \@ "WIDTH")).map(s => if (s.isEmpty) {
      0
    } else {
      Integer.parseInt(s)
    })

    val bottom = vpos.zip(heights).map { case (top, height) => top + height }.max
    val right = hpos.zip(widths).map { case (right, width) => right + width }.max
    val top = vpos.min
    val left = hpos.min
    val height = bottom - top
    val width = right - left

    val glyphConfidence = mean(glyphSeq.map(glyph => (glyph \@ "GC")).map(s => if (s.isEmpty) {
      0
    } else {
      s.toDouble
    }))

    val myConfidence = if (punctuationRegex.matches(content)) {
      glyphConfidence
    } else {
      confidence
    }

    <String HPOS={left.toString} VPOS={top.toString} WIDTH={width.toString} HEIGHT={height.toString} CONTENT={simplifiedContent} WC={roundAt(2)(myConfidence).toString}>
      {glyphSeq}
    </String>
  }

  private val numberRegex = raw"\d+\.?\d+".r

  val reverseNumberRule = new RewriteRule {
    override def transform(node: Node): Seq[Node] = node match {
      case altoString: Elem if altoString.label == "String" =>
        val content = altoString \@ "CONTENT"
        if (numberRegex.matches(content)) {
          val inverseNumber = content.reverse
          val newAttributes = for (attr <- altoString.attributes) yield attr match {
            case attr@Attribute("CONTENT", _, _) =>
              attr.goodCopy(value = inverseNumber)
            case other => other
          }
          altoString.copy(attributes = newAttributes)
        } else {
          altoString
        }
      case other => other
    }
  }

  private val hyphenRegex = raw"^(.+)([-־])$$".r
  def addHyphenRule(textSimplifier: Option[TextSimplifier] = None) = new RewriteRule {
    override def transform(node: Node): Seq[Node] = node match {
      case textLine: Elem if textLine.label == "TextLine" =>
        val children = textLine \ "_"
        val hyphenNodes = children.lastOption match {
          case Some(altoString: Elem) if altoString.label == "String" =>
            val content = altoString \@ "CONTENT"
            val confidence = Option.when(!(altoString \@ "WC").isEmpty)((altoString \@ "WC").toDouble).getOrElse(0.0)
            content match {
              case hyphenRegex(_, _) =>
                val glyphs = altoString \ "Glyph"
                val stringGlyphs = glyphs.init
                val lastGlyph = glyphs.last
                val hyphenGlyphContent = lastGlyph \@ "CONTENT"
                val (letterGlyph, hyphenGlyph) = if (hyphenGlyphContent.length==1) {
                  None -> Glyph.fromXML(lastGlyph)
                } else {
                  // In some cases the last glyph contains several letters
                  // We separate the hyphen from the remaining letters - these will be added to the preceding word
                  val numLetters = hyphenGlyphContent.length
                  val hyphen = Glyph.fromXML(lastGlyph)
                  val width = hyphen.rectangle.width
                  val widthHyphen = width / numLetters
                  val widthLetters = widthHyphen * (numLetters -1)
                  // Note: left here only works for right-to-left text (Yiddish)
                  val letterGlyph = hyphen.copy(rectangle = hyphen.rectangle.copy(
                    label = hyphenGlyphContent.substring(0, numLetters - 1),
                    left = hyphen.rectangle.left + widthHyphen,
                    width = widthLetters
                  ))
                  val hyphenGlyph = hyphen.copy(rectangle = hyphen.rectangle.copy(
                    label = hyphenGlyphContent.substring(numLetters - 1),
                    width = widthHyphen
                  ))
                  Some(letterGlyph) -> hyphenGlyph
                }

                val newStringGlyphs = letterGlyph.map{glyph => stringGlyphs :+ glyph.toXml()}.getOrElse(stringGlyphs)

                if (newStringGlyphs.isEmpty) {
                  Seq.empty
                } else {
                  val stringElemWithoutHyphen = glyphsToString(newStringGlyphs, confidence, textSimplifier)
                  val hyphenAttributes = (for (attr <- hyphenGlyph.toXml().attributes) yield attr match {
                    case Attribute("GC", _, _) =>
                      None
                    case attr@Attribute("CONTENT", _, _) =>
                      Some(attr.goodCopy(value = hyphenGlyph.content))
                    case other => Some(other)
                  }).flatten
                  val hyphenElem = (<HYP></HYP>).copy(attributes = hyphenAttributes)
                  Seq(stringElemWithoutHyphen, hyphenElem)
                }
              case _ =>
                Seq.empty
            }
          case _ =>
            Seq.empty
        }
        if (hyphenNodes.size > 0) {
          val newChildren = children.init ++ hyphenNodes
          val newTextLine = (<TextLine>
            {newChildren}
          </TextLine>).copy(attributes = textLine.attributes)
          newTextLine
        } else {
          textLine
        }
      case other => other
    }
  }

  val yiddishConfig: YiddishConfig = YiddishConfig.fromConfig
  def apply(): YiddishAltoTransformer = YiddishAltoTransformer(yiddishConfig)
}
