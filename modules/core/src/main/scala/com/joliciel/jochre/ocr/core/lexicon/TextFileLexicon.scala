package com.joliciel.jochre.ocr.core.lexicon

import com.joliciel.jochre.ocr.core.corpus.TextSimplifier
import org.rogach.scallop.{ScallopConf, ScallopOption}
import org.slf4j.LoggerFactory

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.file.Path
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}
import scala.io.Source

case class TextFileLexicon(entries: Set[String], textSimplifier: Option[TextSimplifier] = None) extends Lexicon {
  private val log = LoggerFactory.getLogger(getClass)
  override def getFrequency(word: String, presimplified: Boolean = false): Int = {
    val testWord = if (presimplified) {
      word
    } else {
      textSimplifier.map(_.simplify(word)).getOrElse(word)
    }

    if (entries.contains(testWord)) {
      1
    } else if (isImpossible(testWord)) {
      -1
    } else {
      0
    }
  }

  def serialize(file: File): Unit = {
    log.info(f"Serializing ${entries.size} entries to ${file.getPath}")
    val fos = new FileOutputStream(file)
    val zos = new ZipOutputStream(fos)
    try {
      zos.putNextEntry(new ZipEntry("entries.obj"))
      val out = new ObjectOutputStream(zos)
      out.writeObject(this.entries)
    } finally {
      zos.flush()
      fos.flush()
      fos.close()
    }
  }

  override def isImpossible(word: String): Boolean = false
}

object TextFileLexicon {
  private val log = LoggerFactory.getLogger(getClass)
  def deserialize(file: File, textSimplifier: Option[TextSimplifier] = None): TextFileLexicon = {
    val fis = new FileInputStream(file)
    val zis = new ZipInputStream(fis)
    try {
      zis.getNextEntry
      val in = new ObjectInputStream(zis)
      val entries = in.readObject.asInstanceOf[Set[String]]
      TextFileLexicon(entries, textSimplifier)
    } finally {
      zis.close()
      fis.close()
    }
  }

  def loadTextFileLexicon(input: File, textSimplifier: Option[TextSimplifier] = None) : TextFileLexicon =
    load(input, textSimplifier, TextFileLexicon(_, _))

  def load[T <: TextFileLexicon](input: File, textSimplifier: Option[TextSimplifier] = None, constructor: (Set[String], Option[TextSimplifier]) => T): T = {
    if (input.isDirectory) {
      val files = Option(input.listFiles).getOrElse(Array.empty)
      val entries = files.filter(_.getName.endsWith(".txt"))
        .foldLeft(Set.empty[String]){ case (entries, file) =>
          log.info(f"Loaded ${entries.size} entries before file ${file.getPath}")
          val newEntries = loadFile(file, textSimplifier)
          entries ++ newEntries
        }
      log.info(f"Loaded ${entries.size} entries")
      constructor(entries, textSimplifier)
    } else {
      val entries = loadFile(input, textSimplifier).toSet
      constructor(entries, textSimplifier)
    }
  }

  private def loadFile(input: File, textSimplifier: Option[TextSimplifier] = None): Iterator[String] = {
    val fileEntries = Source.fromFile(input).getLines()
    val simplifiedEntries = textSimplifier.map { textSimplifier => fileEntries.map(textSimplifier.simplify(_)) }
      .getOrElse(fileEntries)
    simplifiedEntries
  }

  class LexiconCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val lexiconDir: ScallopOption[String] = opt[String](required = true)
    val outputPath: ScallopOption[String] = opt[String](required = true)
    verify()
  }


  def main(args: Array[String]): Unit = {
    val cli = new LexiconCLI(args)
    val lexiconDir = Path.of(cli.lexiconDir())
    val outputPath = Path.of(cli.outputPath())

    val outputDir = outputPath.getParent.toFile
    outputDir.mkdirs()

    val lexicon = TextFileLexicon.loadTextFileLexicon(lexiconDir.toFile)
    lexicon.serialize(outputPath.toFile)
  }
}
