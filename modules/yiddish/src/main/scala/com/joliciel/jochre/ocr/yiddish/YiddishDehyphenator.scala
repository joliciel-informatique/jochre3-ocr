package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.yiddish.lexicon.YivoLexicon
import org.slf4j.LoggerFactory
import scala.io.Source
import java.nio.file.Files
import java.nio.charset.StandardCharsets
import org.rogach.scallop.*
import java.io.File
import com.joliciel.jochre.ocr.core.text.Dehyphenator
import zio.{ZLayer, Task}
import zio.ZIO

/** Given an ocr'd text split on physical line breaks (single line break) and paragraphs (double line breaks), with
  * various words hyphenated across physical line breaks, de-hyphenates the text, so that all physical line breaks are
  * removed, and hyphenated words are put back together with or without a hyphen, depending on which is more probable.
  * Paragraphs will be marked with a single line break.
  */
final case class YiddishDehyphenator(yivoLexicon: YivoLexicon) extends Dehyphenator {
  private val log = LoggerFactory.getLogger(getClass)

  private val whiteSpaceOrPunct = raw"(?U)[\p{Punct}\s]".r

  def dehyphenate(text: String): String = {
    val lines = text.split("\n").toSeq
    if (lines.length <= 1) {
      text
    } else {
      val linesWithNext = lines.zip(lines.tail :+ "")
      val fixedLines = linesWithNext
        .map { case (line, nextLine) =>
          if (line.endsWith("־")) {
            val lastWord = whiteSpaceOrPunct
              .findAllMatchIn(line)
              .toSeq
              .init
              .lastOption match {
              case Some(myMatch) =>
                line.substring(myMatch.start + 1, line.length - 1)
              case None =>
                line.substring(0, line.length - 1)
            }

            if (log.isDebugEnabled()) {
              log.debug(f"Last word before hyphen: $lastWord")
            }
            val firstWordNextLine =
              whiteSpaceOrPunct.findFirstMatchIn(nextLine) match {
                case Some(myMatch) => nextLine.substring(0, myMatch.start)
                case None          => nextLine
              }

            if (log.isDebugEnabled()) {
              log.debug(f"First word on next line $firstWordNextLine")
            }

            val fullWord = f"$lastWord$firstWordNextLine"

            val lastWordYivo = yivoLexicon.toYivo(lastWord, false)
            val firstWordNextLineYivo =
              yivoLexicon.toYivo(firstWordNextLine)
            val fullWordYivo = yivoLexicon.toYivo(fullWord)
            val fullWordWithHyphen = f"$lastWordYivo־$firstWordNextLineYivo"

            val keepHyphen =
              if (lastWordYivo.matches(".+[ךםןףץ]$")) {
                // first word ends with "end" letter, keep hyphen
                if (log.isDebugEnabled()) {
                  log.debug(
                    f"First word $lastWordYivo ends with end letter. Keep hyphen."
                  )
                }
                true
              } else if (yivoLexicon.getFrequency(fullWordYivo, true) > 0) {
                // the combined word without a hyphen exists in the lexicon, remove the hyphen
                if (log.isDebugEnabled()) {
                  log.debug(
                    f"Combined word $fullWordYivo found in lexicon. Remove hyphen."
                  )
                }
                false
              } else if (
                yivoLexicon.getFrequency(
                  lastWordYivo,
                  true
                ) > 0 && yivoLexicon.getFrequency(
                  firstWordNextLineYivo,
                  true
                ) > 0
              ) {
                // both words exist separately in lexicon, keep the hyphen
                if (log.isDebugEnabled()) {
                  log.debug(
                    f"Both words $lastWordYivo and $firstWordNextLineYivo found in lexicon. Keep hyphen."
                  )
                }
                true
              } else if (
                yivoLexicon.getFrequency(
                  fullWordWithHyphen,
                  true
                ) > 0
              ) {
                // the combined word with a hyphen exists in the lexicon, keep the hyphen
                if (log.isDebugEnabled()) {
                  log.debug(
                    f"Combined word $fullWordWithHyphen found in lexicon. Keep hyphen."
                  )
                }
                true
              } else {
                // remove the hyphen by default
                log.debug(
                  f"Either $lastWordYivo or $firstWordNextLineYivo not found in lexicon. Remove hyphen."
                )
                false
              }

            if (!keepHyphen) {
              line.substring(0, line.length() - 1)
            } else if (nextLine.length == 0) {
              f"$line\n"
            } else {
              line
            }
          } else if (nextLine.length == 0) {
            f"$line\n"
          } else if (line.length == 0) {
            line
          } else {
            f"$line "
          }
        }

      fixedLines.mkString("")
    }
  }
}

object YiddishDehyphenator {
  private val log = LoggerFactory.getLogger(getClass)

  def apply(yiddishConfig: YiddishConfig): YiddishDehyphenator = {
    val yivoLexicon = YivoLexicon.fromYiddishConfig(yiddishConfig)
    YiddishDehyphenator(yivoLexicon)

  }

  def apply(): YiddishDehyphenator = {
    val yiddishConfig = YiddishConfig.fromConfig
    val yivoLexicon = YivoLexicon.fromYiddishConfig(yiddishConfig)
    YiddishDehyphenator(yivoLexicon)
  }

  val live: ZLayer[YiddishConfig, Nothing, YiddishDehyphenator] =
    ZLayer.fromZIO {
      for {
        yiddishConfig <- ZIO.service[YiddishConfig]
      } yield YiddishDehyphenator(yiddishConfig)
    }

  private class CLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val inFile: ScallopOption[String] = opt[String](
      required = true,
      descr = "The file to dehyphenate"
    )
    val outFile: ScallopOption[String] = opt[String](
      required = true,
      descr = "The output file after dehyphenation"
    )
    verify()
  }

  def main(args: Array[String]): Unit = {
    val options = new CLI(args.toIndexedSeq)

    val inFile = new File(options.inFile())
    val outFile = new File(options.outFile())
    outFile.getParentFile.mkdirs()

    log.info(f"Dehyphenating ${inFile.getName()}")

    val source = Source.fromFile(inFile)
    val text =
      try source.getLines.mkString("\n")
      finally source.close()

    val dehyphenator = YiddishDehyphenator()
    val dehyphenated = dehyphenator.dehyphenate(text)

    Files.write(outFile.toPath, dehyphenated.getBytes(StandardCharsets.UTF_8))
    log.info(f"Dehyphenated ${inFile.getName()}")
  }
}
