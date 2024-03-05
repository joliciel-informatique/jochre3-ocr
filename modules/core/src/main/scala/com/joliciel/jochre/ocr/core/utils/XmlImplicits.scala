package com.joliciel.jochre.ocr.core.utils

import scala.xml.{Atom, Attribute, MetaData, Node, Null, Text}

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

  implicit class EnrichedNode(node: Node) {
    def textContent: String = node.child.collect {
      case textNode: Atom[_] => textNode.text
    }.mkString("")
  }
}

object XmlImplicits extends XmlImplicits