package com.joliciel.jochre.ocr.core.utils

object FileUtils {
  def removeFileExtension(filename: String, removeAllExtensions: Boolean = true): String = {
    if (filename == null || filename.isEmpty) return filename
    val extPattern = "(?<!^)[.]" + (if (removeAllExtensions) ".*" else "[^.]*$")
    filename.replaceAll(extPattern, "")
  }
}
