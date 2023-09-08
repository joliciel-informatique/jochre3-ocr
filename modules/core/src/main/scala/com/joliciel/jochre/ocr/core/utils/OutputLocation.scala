package com.joliciel.jochre.ocr.core.utils

import java.io.File
import java.nio.file.Path

case class OutputLocation(outDir: Path, baseName: String) {
  def resolve(extension: String): Path = outDir.resolve(baseName + extension)
}

object OutputLocation {
  def apply(outDir: Path, file: File): OutputLocation = OutputLocation(outDir, FileUtils.removeFileExtension(file.getName))

  def apply(outDir: Path, path: Path): OutputLocation = this.apply(outDir, path.toFile)
}
