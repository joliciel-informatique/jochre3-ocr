package com.joliciel.jochre.ocr.core

import com.joliciel.jochre.ocr.core.graphics.{ImageInfo, WithRectangle}
import com.joliciel.jochre.ocr.core.utils.StringUtils
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.opencv_core.Mat
import enumeratum._

import scala.xml.Elem

package object model {
  sealed trait SubsType extends EnumEntry

  object SubsType extends Enum[SubsType] {
    val values: IndexedSeq[SubsType] = findValues

    /** If content is the first part of a hyphenated word, applies only for the last word of a line if it is hyphenated
      */
    case object HypPart1 extends SubsType

    /** If content is the second part of a hyphenated word, applies only for the first word of a line if it is
      * hyphenated
      */
    case object HypPart2 extends SubsType
  }

  trait AltoElement {
    def toXml: Elem

    def transform(
        partialFunction: PartialFunction[AltoElement, AltoElement]
    ): AltoElement
  }

  trait PageElement extends AltoElement {
    def translate(xDiff: Int, yDiff: Int): PageElement

    def rotate(imageInfo: ImageInfo): PageElement

    def rescale(scale: Double): PageElement

    def draw(mat: Mat): Unit

    def content: String
  }

  trait Block extends PageElement with WithRectangle

  trait TextContainer extends Block {
    def processedContent: String
  }

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

    def getEffectiveLanguage(
        language: Option[String],
        defaultLanguage: Option[String]
    ): String = {
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
