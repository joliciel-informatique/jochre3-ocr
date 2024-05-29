package com.joliciel.jochre.ocr.core.lexicon

import com.joliciel.jochre.ocr.core.corpus.TextSimplifier
import io.github.classgraph.{ClassGraph, Resource}
import org.rogach.scallop._
import org.slf4j.LoggerFactory

import java.io.{File, FileInputStream, FileOutputStream, InputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}
import scala.collection.immutable.ArraySeq
import scala.io.Source
import scala.util.Using

case class TextFileLexicon(
    entries: Set[String],
    textSimplifier: Option[TextSimplifier] = None
) extends Lexicon {
  private val log = LoggerFactory.getLogger(getClass)
  override def getFrequency(
      word: String,
      presimplified: Boolean = false
  ): Int = {
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
  def deserialize(
      file: File,
      textSimplifier: Option[TextSimplifier] = None
  ): TextFileLexicon = {
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

  private def loadTextFileLexicon(
      input: File,
      textSimplifier: Option[TextSimplifier] = None
  ): TextFileLexicon =
    load(input, textSimplifier, TextFileLexicon(_, _))

  def load[T <: TextFileLexicon](
      input: File,
      textSimplifier: Option[TextSimplifier] = None,
      constructor: (Set[String], Option[TextSimplifier]) => T
  ): T = {
    if (input.isDirectory) {
      val files = Option(input.listFiles).getOrElse(Array.empty[File])
      val entries = files
        .filter(_.getName.endsWith(".txt"))
        .foldLeft(Set.empty[String]) { case (entries, file) =>
          log.info(
            f"Loaded ${entries.size} entries before file ${file.getPath}"
          )
          Using(new FileInputStream(file)) { input =>
            val newEntries = loadFile(input, textSimplifier)
            entries ++ newEntries
          }.get
        }
      log.info(f"Loaded ${entries.size} entries")
      constructor(entries, textSimplifier)
    } else {
      Using(new FileInputStream(input)) { input =>
        val entries = loadFile(input, textSimplifier).toSet
        constructor(entries, textSimplifier)
      }.get
    }
  }

  def loadFromResource[T <: TextFileLexicon](
      resourcePath: String,
      textSimplifier: Option[TextSimplifier] = None,
      constructor: (Set[String], Option[TextSimplifier]) => T
  ): T = {
    var entries = Set.empty[String]
    val scanResult = new ClassGraph().acceptPaths(resourcePath).scan
    try {
      scanResult
        .getResourcesWithExtension("txt")
        .forEachInputStreamThrowingIOException((res: Resource, input: InputStream) => {
          log.info(f"Loading entries from ${res.getPath}")
          val newEntries = loadFile(input, textSimplifier)
          entries = entries ++ newEntries
        })

    } finally { if (scanResult != null) scanResult.close() }
    constructor(entries, textSimplifier)
  }

  private def loadFile(
      input: InputStream,
      textSimplifier: Option[TextSimplifier] = None
  ): Iterator[String] = {
    val fileEntries = Source.fromInputStream(input, "UTF-8").getLines()
    val simplifiedEntries = textSimplifier
      .map { textSimplifier => fileEntries.map(textSimplifier.simplify) }
      .getOrElse(fileEntries)
    simplifiedEntries
  }

  private class LexiconCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val lexiconDir: ScallopOption[String] = opt[String](required = true)
    val outputPath: ScallopOption[String] = opt[String](required = true)
    verify()
  }

  def main(args: Array[String]): Unit = {
    val cli = new LexiconCLI(ArraySeq.unsafeWrapArray(args))
    val lexiconDir = Path.of(cli.lexiconDir())
    val outputPath = Path.of(cli.outputPath())

    val outputDir = outputPath.getParent.toFile
    outputDir.mkdirs()

    val lexicon = TextFileLexicon.loadTextFileLexicon(lexiconDir.toFile)
    lexicon.serialize(outputPath.toFile)
  }
}
