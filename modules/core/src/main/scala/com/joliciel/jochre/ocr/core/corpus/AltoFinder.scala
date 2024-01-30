package com.joliciel.jochre.ocr.core.corpus

import com.joliciel.jochre.ocr.core.model.Page

import java.nio.file.Path
import scala.xml.XML

trait AltoFinder {
  def getAltoPage(imagePath: Path): Page
}

object AltoFinder {
  /**
   * The default alto finder assumes if the image is called "blah/blah.jpg", then an alto file
   * is located in the same directory called "blah/blah_alto4.xml"
   */
  val default: AltoFinder = (imagePath: Path) => {
    val imagePathString = imagePath.toString
    val altoPath = imagePathString.substring(0, imagePathString.lastIndexOf('.')) + "_alto4.xml"
    val file = Path.of(altoPath).toFile
    val altoXml = XML.loadFile(file)
    Page.fromXML(altoXml)
  }
}
