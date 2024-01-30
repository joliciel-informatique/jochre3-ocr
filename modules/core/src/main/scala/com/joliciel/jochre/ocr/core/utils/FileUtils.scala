package com.joliciel.jochre.ocr.core.utils

import java.io.File
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.util.matching.Regex

trait FileUtils {
  def removeFileExtension(filename: String, removeAllExtensions: Boolean = true): String = {
    if (filename == null || filename.isEmpty) return filename
    val extPattern = "(?<!^)[.]" + (if (removeAllExtensions) ".*" else "[^.]*$")
    filename.replaceAll(extPattern, "")
  }

  def readFile(file: File): Seq[String] = {
    val source = Source.fromFile(file)
    try {
      readSource(source)
    } finally {
      source.close()
    }
  }

  def readFile(filename: String): Seq[String] = {
    val source = Source.fromFile(filename)
    try {
      readSource(source)
    } finally {
      source.close()
    }
  }

  private def readSource(source: Source): Seq[String] = {
    (for (line <- source.getLines()) yield line).toSeq
  }

  def writeFile(path: Path, text: String) = {
    Files.write(path, text.getBytes(StandardCharsets.UTF_8))
  }

  def recursiveListFiles(dir: File, regex: Regex): Seq[File] =
    recursiveListFilesInternal(dir, regex).sortBy(_.getPath).toSeq

  private def recursiveListFilesInternal(dir: File, regex: Regex): Array[File] = {
    val these = Option(dir.listFiles).getOrElse(Array.empty)
    val good = these.filter(file => regex.findFirstIn(file.getName).isDefined)
    good ++ these.filter(_.isDirectory).flatMap(recursiveListFilesInternal(_, regex))
  }

  def recursiveListImages(dir: File): Seq[URI] =
    recursiveListFilesInternal(dir, ".*\\.jpg|.*\\.png|.*\\.jpeg".r).map(_.toURI).sortBy(_.getPath).toSeq
}

object FileUtils extends FileUtils