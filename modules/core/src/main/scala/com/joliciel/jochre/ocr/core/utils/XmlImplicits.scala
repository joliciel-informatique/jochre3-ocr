package com.joliciel.jochre.ocr.core.utils

import scala.xml.{Attribute, MetaData, Null, Text}

trait XmlImplicits {
  implicit class AddGoodCopyToAttribute(attr: Attribute) {
    def goodCopy(key: String = attr.key, value: Any = attr.value): Attribute =
      Attribute(attr.pre, key, Text(value.toString), attr.next)
  }

  implicit def iterableToMetaData(items: Iterable[MetaData]): MetaData = {
    items match {
      case Nil => Null
      case head :: tail => head.copy(next = iterableToMetaData(tail))
    }
  }
}

object XmlImplicits extends XmlImplicits