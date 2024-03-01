package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.graphics.{ImageInfo, Rectangle, WithRectangle}
import com.joliciel.jochre.ocr.core.utils.StringUtils
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.opencv_core.Mat

import scala.xml.Elem

package object model {
  trait AltoElement {
    def toXml: Elem

    def transform(partialFunction: PartialFunction[AltoElement, AltoElement]): AltoElement
  }

  trait PageElement extends AltoElement {
    def translate(xDiff: Int, yDiff: Int): PageElement

    def rotate(imageInfo: ImageInfo): PageElement

    def rescale(scale: Double): PageElement

    def draw(mat: Mat): Unit

    def content: String
  }

  trait Block extends PageElement with WithRectangle

  trait TextContainer extends Block

  trait WithLanguage {
    def language: Option[String]
    def defaultLanguage: Option[String]
    def withDefaultLanguage(defaultLanguage: String): WithLanguage

    def languageOrDefault: String = {
      getEffectiveLanguage(this.language, this.defaultLanguage)
    }

    def isLeftToRight: Boolean = {
      StringUtils.isLeftToRight(this.languageOrDefault)
    }

    def getEffectiveLanguage(language: Option[String], defaultLanguage: Option[String]): String = {
      language.getOrElse(
        defaultLanguage.getOrElse {
          ConfigFactory.load().getConfig("jochre.ocr").getString("language")
        }
      )
    }
  }

  trait WordOrSpace extends PageElement with WithRectangle

  trait Tag {
    def toXml: Elem
  }
}
