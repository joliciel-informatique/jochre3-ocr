package com.joliciel.jochre.ocr.core.model

import scala.xml.{Elem, Node}

case class Alto(
    fileName: String,
    pages: Seq[Page],
    processingSteps: Seq[ProcessingStep] = Seq.empty,
    textStyles: Seq[TextStyle] = Seq.empty,
    tags: Seq[Tag] = Seq.empty
) {
  lazy val content: String = pages.map(_.content).mkString("\n")

  def toXml: Elem = <alto xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                          xmlns="http://www.loc.gov/standards/alto/ns-v4#"
                          xsi:schemaLocation="http://www.loc.gov/standards/alto/ns-v4# http://www.loc.gov/standards/alto/v4/alto-4-4.xsd"
                          xmlns:xlink="http://www.w3.org/1999/xlink">
    <Description>
      <MeasurementUnit>pixel</MeasurementUnit>
      <sourceImageInformation>
        <fileName>{fileName}</fileName>
      </sourceImageInformation>
      {processingSteps.map(_.toXml)}
    </Description>
    <Layout>{pages.map(_.toXml)}</Layout>
    <Styles>{textStyles.map(_.toXml)}</Styles>
    <Tags>{tags.map(_.toXml)}</Tags>
  </alto>

  def transform(
      partialFunction: PartialFunction[AltoElement, AltoElement]
  ): Alto = {
    val newPages = pages.map(_.transform(partialFunction)).collect { case page: Page =>
      page
    }
    this.copy(pages = newPages)
  }
}

object Alto {
  def fromXML(node: Node): Alto = {
    import com.joliciel.jochre.ocr.core.utils.XmlImplicits._

    val pageNodes = node \\ "Page"
    val pages = pageNodes.map(Page.fromXML)

    val processingStepNodes = node \\ "Processing"
    val processingSteps = processingStepNodes.map(ProcessingStep.fromXML)

    val textStyleNodes = node \\ "TextStyle"
    val textStyles = textStyleNodes.map(TextStyle.fromXML)

    val layoutTagNodes = node \\ "LayoutTag"
    val layoutTags = layoutTagNodes.map(LayoutTag.fromXML)

    val structureTagNodes = node \\ "StructureTag"
    val structureTags = structureTagNodes.map(StructureTag.fromXML)

    val tags = layoutTags ++ structureTags

    val fileNameNode = (node \\ "fileName").headOption
    val fileName = fileNameNode.map(_.textContent).getOrElse("")

    Alto(
      fileName = fileName,
      pages = pages,
      processingSteps = processingSteps,
      textStyles = textStyles,
      tags = tags
    )
  }
}
