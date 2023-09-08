package com.joliciel.jochre.ocr.core

import java.io.File
import java.net.URI
import scala.io.Source
import scala.util.matching.Regex

package object transform {
  def readFile(filename: String): Seq[String] = {
    val bufferedSource = Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
    bufferedSource.close
    lines
  }

  def recursiveListFiles(dir: File, regex: Regex): Array[File] = {
    val these = Option(dir.listFiles).getOrElse(Array.empty)
    val good = these.filter(file => regex.findFirstIn(file.getName).isDefined)
    good ++ these.filter(_.isDirectory).flatMap(recursiveListFiles(_, regex))
  }

  def recursiveListImages(dir: File): Vector[URI] =
    recursiveListFiles(dir, ".*\\.jpg|.*\\.png|.*\\.jpeg".r).map(_.toURI).sortBy(_.getPath).toVector
}
