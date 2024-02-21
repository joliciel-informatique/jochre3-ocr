package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.output.OutputFormat
import org.rogach.scallop.{ScallopConf, ScallopOption}

class JochreCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
  val input: ScallopOption[String] = opt[String](required = true)
  val outputDir: ScallopOption[String] = opt[String](required = true)
  val debugDir: ScallopOption[String] = opt[String]()
  val maxImages: ScallopOption[Int] = opt[Int](default = Some(0), descr = "For directories, the max files to process. 0 means all files.")
  val startPage: ScallopOption[Int] = opt[Int](descr = "For PDF files, the start page, starting at 1.")
  val endPage: ScallopOption[Int] = opt[Int](descr = "For PDF files, the end page, starting at 1. 0 means all pages.")
  val dpi: ScallopOption[Int] = opt[Int](descr = "For PDF files, the DPI at which to export the file before analyzing. Default 300.")
  val outputFormats: ScallopOption[String] = opt[String](default = Some(OutputFormat.Alto4.entryName), descr = f"Comma-separated list of output formats among: ${OutputFormat.values.map(_.entryName).mkString(", ")}")
  val evalDir: ScallopOption[String] = opt[String]()
  val testRectangle: ScallopOption[String] = opt[String]()
  val beamWidth: ScallopOption[Int] = opt[Int](descr = "The beam width to use if the algorithm uses it. If not provided will use value from config.")
  val unknownWordFactor: ScallopOption[Double] = opt[Double](descr = "Factor to apply to unknown words in order to reduce their score. If not provided, will use value from config.")
  val lexiconDir: ScallopOption[String] = opt[String](descr = "If provided, will use lexicon to recognize known words. If not provided, no lexicon will be used.")
  verify()
}