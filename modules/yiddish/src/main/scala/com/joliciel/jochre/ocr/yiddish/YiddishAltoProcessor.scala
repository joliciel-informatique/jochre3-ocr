package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.lexicon.{Lexicon, TextFileLexicon}
import com.joliciel.jochre.ocr.core.analysis.{AltoAlternative, AltoProcessor}
import com.joliciel.jochre.ocr.core.utils.XmlImplicits
import com.joliciel.jochre.ocr.yiddish.YiddishAltoProcessor.Purpose
import com.joliciel.yivoTranscriber.YivoTranscriber
import enumeratum._
import org.slf4j.LoggerFactory
import zio._

import java.io.FileInputStream
import java.util.zip.ZipInputStream
import scala.util.Using
import scala.xml.transform.RewriteRule
import scala.xml.{Attribute, Elem, Node}

case class YiddishAltoProcessor(yiddishConfig: YiddishConfig) extends AltoProcessor with XmlImplicits {

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

  override def getAlternatives(content: String): Set[AltoAlternative] = {
    val yivo = yivoTranscriber.transcribe(content, false)

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

  private val numberRegex = raw"\d+\.?\d+".r
  private val punctuationAndNotRegex = raw"(?U)\p{Punct}[^\p{Punct}\d]|[^\p{Punct}\d]\p{Punct}".r
  private val punctuationRegex = raw"(?U)\p{Punct}".r
  override def getSpecificRules: Seq[RewriteRule] = Seq(
    new RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case altoString: Elem if altoString.label == "String" =>
          val content = altoString \@ "CONTENT"
          val confidence = Option.when(!(altoString \@ "WC").isEmpty)((altoString \@ "WC").toDouble).getOrElse(0)
          if (punctuationAndNotRegex.findFirstIn(content).isDefined) {
            // split by punctuation
            val glyphs = altoString \ "Glyph"
            val words = glyphs.foldLeft(Seq[Seq[Node]]()){ case (words, glyph) =>
              if (words.isEmpty) {
                Seq(Seq(glyph))
              } else {
                val currentIsPunct = punctuationRegex.matches(glyph \@ "CONTENT")
                if (currentIsPunct) {
                  words :+ Seq(glyph)
                } else {
                  val lastGlyph = words.last.map(_.last)
                  val lastIsPunct = punctuationRegex.matches(lastGlyph \@ "CONTENT")
                  if (lastIsPunct) {
                    words :+ Seq(glyph)
                  } else {
                    words.init :+ (words.last :+ glyph)
                  }
                }
              }
            }
            val wordNodes = words.map { glyphSeq =>
              val content = glyphSeq.map(glyph => (glyph \@ "CONTENT")).mkString("")
              val vpos = glyphSeq.map(glyph => (glyph \@ "VPOS")).map(s => if (s.isEmpty) {0} else {Integer.parseInt(s)})
              val hpos = glyphSeq.map(glyph => (glyph \@ "HPOS")).map(s => if (s.isEmpty) {0} else {Integer.parseInt(s)})
              val heights = glyphSeq.map(glyph => (glyph \@ "HEIGHT")).map(s => if (s.isEmpty) {0} else {Integer.parseInt(s)})
              val widths = glyphSeq.map(glyph => (glyph \@ "WIDTH")).map(s => if (s.isEmpty) {0} else {Integer.parseInt(s)})

              val bottom = vpos.zip(heights).map{case (top, height) => top + height }.max
              val right = hpos.zip(widths).map{case (right, width) => right + width }.max
              val left = vpos.min
              val top = hpos.min
              val height = bottom - top
              val width = right - left

              val glyphConfidence = glyphSeq.map(glyph => (glyph \@ "GC")).map(s => if (s.isEmpty) {0} else {s.toDouble}).max
              val myConfidence = if (glyphSeq.size>1) { confidence } else { glyphConfidence }
              <String VPOS={left.toString} HPOS={top.toString} HEIGHT={height.toString} WIDTH={width.toString} WC={myConfidence.toString} CONTENT={content}></String>
            }
            wordNodes
          } else {
            altoString
          }
        case other => other
      }
    },
    new RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case altoString: Elem if altoString.label == "String" =>
          val content = altoString \@ "CONTENT"
          if (numberRegex.matches(content)) {
            val inverseNumber = content.reverse
            val newAttributes = for (attr <- altoString.attributes) yield attr match {
              case attr@Attribute("CONTENT", _, _) =>
                attr.goodCopy (value = inverseNumber)
              case other => other
            }
            altoString.copy(attributes = newAttributes)
          } else {
            altoString
          }
        case other => other
      }
    }
  )

  override val removeGlyphs: Boolean = true
}

object YiddishAltoProcessor {
  sealed trait Purpose extends EnumEntry

  object Purpose extends Enum[Purpose] {
    val values = findValues

    case object YIVO extends Purpose
    case object Roman extends Purpose
  }

  val yiddishConfig: YiddishConfig = YiddishConfig.fromConfig
  def apply(): YiddishAltoProcessor = YiddishAltoProcessor(yiddishConfig)
}
