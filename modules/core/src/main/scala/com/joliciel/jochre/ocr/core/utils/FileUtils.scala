package com.joliciel.jochre.ocr.core.utils

import org.bytedeco.opencv.opencv_core.Mat

import java.io.File
import java.net.{URI, URL}
import java.nio.charset.StandardCharsets
import java.nio.file.{FileSystemNotFoundException, FileSystems, Files, Path, Paths, StandardCopyOption}
import scala.io.Source
import scala.util.{Try, Using}
import scala.util.matching.Regex

trait FileUtils {
  def removeFileExtension(
      filename: String,
      removeAllExtensions: Boolean = true
  ): String = {
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

  def writeFile(path: Path, text: String): Path = {
    Files.write(path, text.getBytes(StandardCharsets.UTF_8))
  }

  def listFiles(dir: Path, regex: Regex, recursive: Boolean = true): Seq[File] =
    listFilesInternal(dir, regex, recursive).sortBy(_.getPath).toSeq

  private def listFilesInternal(
      dir: Path,
      regex: Regex,
      recursive: Boolean = true
  ): Array[File] = {
    val these = Option(dir.toFile.listFiles).getOrElse(Array.empty[File])
    val good = these.filter(file => file.isFile && regex.findFirstIn(file.getName).isDefined)
    if (recursive) {
      good ++ these
        .filter(_.isDirectory)
        .flatMap(f => listFilesInternal(f.toPath, regex))
    } else {
      good
    }

  }

  def listImages(dir: Path, recursive: Boolean = true): Seq[File] =
    listFilesInternal(dir, ".*\\.jpg|.*\\.png|.*\\.jpeg".r, recursive)
      .sortBy(_.getPath)
      .toSeq

  def getImageFilesFromDir(
      inputDir: Path,
      maxImages: Option[Int],
      recursive: Boolean = true
  ): Seq[(File, Mat)] = {
    val allFiles = FileUtils.listImages(inputDir, recursive)

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

      val fileNameBase = if (lastDot > 0) { fileName.substring(0, lastDot) }
      else { fileName }
      val extension = if (lastDot > 0) { fileName.substring(lastDot) }
      else { "" }
      val file = File.createTempFile(fileNameBase, extension)
      Files.copy(stream, file.toPath, StandardCopyOption.REPLACE_EXISTING)
      file
    }
  }
}

object FileUtils extends FileUtils
