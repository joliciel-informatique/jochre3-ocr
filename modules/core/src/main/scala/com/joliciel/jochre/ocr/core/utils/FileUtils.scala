package com.joliciel.jochre.ocr.core.utils

import org.bytedeco.opencv.opencv_core.Mat

import java.io.File
import java.net.{URI, URL}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.io.Source
import scala.util.{Try, Using}
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

  def recursiveListImages(dir: File): Seq[File] =
    recursiveListFilesInternal(dir, ".*\\.jpg|.*\\.png|.*\\.jpeg".r).sortBy(_.getPath).toSeq

  def getImageFilesFromDir(inputDir: Path, maxImages: Option[Int]): Seq[(File, Mat)] = {
    val allFiles = FileUtils.recursiveListImages(inputDir.toFile)

    allFiles
      .take(maxImages.getOrElse(allFiles.size))
      .map { inputFile =>
        val mat = ImageUtils.loadImage(inputFile.toPath)
        inputFile -> mat
      }
  }

  def getFileFromUrl(urlStr: String): Try[File] = {
    Using.Manager { use =>
      val url = new URL(urlStr)
      val stream = use {
        url.openStream
      }
      val fileName = urlStr.substring(urlStr.lastIndexOf('/') + 1)

      val lastDot = fileName.lastIndexOf('.')

      val fileNameBase =  if (lastDot>0) { fileName.substring(0, lastDot) } else { fileName }
      val extension = if (lastDot > 0) { fileName.substring(lastDot) } else { "" }
      val file = File.createTempFile(fileNameBase, extension)
      Files.copy(stream, file.toPath, StandardCopyOption.REPLACE_EXISTING)
      file
    }
  }
}

object FileUtils extends FileUtils