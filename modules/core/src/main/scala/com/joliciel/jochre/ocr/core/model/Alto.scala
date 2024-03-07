package com.joliciel.jochre.ocr.core.model

import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{LocalDateTime, ZoneOffset, ZonedDateTime}
import scala.xml.{Elem, Node}

case class Alto(
    fileName: String,
    pages: Seq[Page],
    textStyles: Seq[TextStyle] = Seq.empty,
    tags: Seq[Tag] = Seq.empty,
    processingTime: ZonedDateTime = LocalDateTime.now().atZone(ZoneOffset.UTC)
) {
  import com.joliciel.jochre.ocr.core.model.Alto._

  lazy val content: String = pages.map(_.content).mkString("\n")

  private val ocrVersion =
    sys.env.getOrElse("JOCHRE3_OCR_VERSION", "0.0.1-SNAPSHOT")

  def toXml: Elem = <alto xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                          xmlns="http://www.loc.gov/standards/alto/ns-v4#"
                          xsi:schemaLocation="http://www.loc.gov/standards/alto/ns-v4# http://www.loc.gov/standards/alto/v4/alto-4-4.xsd"
                          xmlns:xlink="http://www.w3.org/1999/xlink">
    <Description>
      <MeasurementUnit>pixel</MeasurementUnit>
      <sourceImageInformation>
        <fileName>{fileName}</fileName>
      </sourceImageInformation>
      <Processing ID="OCR_1">
        <processingDateTime>{
    processingTime.format(dateTimeFormatter)
  }</processingDateTime>
        <processingStepDescription>contentGeneration</processingStepDescription>
        <processingSoftware>
          <softwareCreator>Joliciel Informatique</softwareCreator>
          <softwareName>Jochre</softwareName>
          <softwareVersion>{ocrVersion}</softwareVersion>
          <applicationDescription>Java Optical CHaracter REcognition: https://gitlab.com/jochre/jochre3-ocr/</applicationDescription>
        </processingSoftware>
      </Processing>
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
  private val dateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME

  def fromXML(node: Node): Alto = {
    import com.joliciel.jochre.ocr.core.utils.XmlImplicits._

    val pageNodes = node \\ "Page"
    val pages = pageNodes.map(Page.fromXML)
    val textStyleNodes = node \\ "TextStyle"
    val textStyles = textStyleNodes.map(TextStyle.fromXML)

    val layoutTagNodes = node \\ "LayoutTag"
    val layoutTags = layoutTagNodes.map(LayoutTag.fromXML)

    val structureTagNodes = node \\ "StructureTag"
    val structureTags = structureTagNodes.map(StructureTag.fromXML)

    val tags = layoutTags ++ structureTags

    val fileNameNode = (node \\ "fileName").headOption
    val fileName = fileNameNode.map(_.textContent).getOrElse("")

    val processingTimeNode = (node \\ "processingDateTime").headOption
    val processingTime = processingTimeNode
      .map { node =>
        try {
          ZonedDateTime.parse(node.textContent, dateTimeFormatter)
        } catch {
          case _: DateTimeParseException =>
            LocalDateTime.now().atZone(ZoneOffset.UTC)
        }
      }
      .getOrElse(LocalDateTime.now().atZone(ZoneOffset.UTC))

    Alto(
      fileName = fileName,
      pages = pages,
      textStyles = textStyles,
      tags = tags,
      processingTime = processingTime
    )
  }
}
