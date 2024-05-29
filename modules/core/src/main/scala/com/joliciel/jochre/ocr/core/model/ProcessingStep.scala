package com.joliciel.jochre.ocr.core.model

import java.time.{LocalDateTime, ZoneOffset, ZonedDateTime}
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import scala.xml.{Elem, Node}

case class ProcessingStep(
    id: String,
    processingTime: Option[ZonedDateTime] = None,
    processingAgency: Option[String] = None,
    processingStepDescriptions: Seq[String] = Seq.empty,
    processingStepSettings: Option[String] = None,
    softwareCreator: Option[String] = None,
    softwareName: Option[String] = None,
    softwareVersion: Option[String] = None,
    applicationDescription: Option[String] = None
) {
  import ProcessingStep.dateTimeFormatter

  def toXml: Elem = <Processing ID="OCR_1">
    {
      processingTime.map(time => <processingDateTime>{time.format(dateTimeFormatter)}</processingDateTime>).orNull
    }
    {processingAgency.map(agency => <processingAgency>{agency}</processingAgency>).orNull}
    {
      processingStepDescriptions.map(description =>
        <processingStepDescription>{description}</processingStepDescription>
      )
    }
    {
      processingStepSettings.map(settings => <processingStepSettings>{settings}</processingStepSettings>).orNull
    }
    <processingSoftware>
      {softwareCreator.map(creator => <softwareCreator>{creator}</softwareCreator>).orNull}
      {softwareName.map(name => <softwareName>{name}</softwareName>).orNull}
      {softwareVersion.map(version => <softwareVersion>{version}</softwareVersion>).orNull}
      {
        applicationDescription.map(description => <applicationDescription>{description}</applicationDescription>).orNull
      }
    </processingSoftware>
  </Processing>
}

object ProcessingStep {
  private val dateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME

  private val dateFormatter = DateTimeFormatter.ISO_DATE

  def jochre(): ProcessingStep = ProcessingStep(
    id = "JOCHRE1",
    processingTime = Some(LocalDateTime.now().atZone(ZoneOffset.UTC)),
    processingStepDescriptions = Seq("contentGeneration"),
    softwareCreator = Some("Joliciel Informatique"),
    softwareName = Some("Jochre"),
    softwareVersion = Some(sys.env.getOrElse("JOCHRE3_OCR_VERSION", "0.0.1-SNAPSHOT")),
    applicationDescription = Some("Java Optical CHaracter REcognition: https://gitlab.com/jochre/jochre3-ocr/")
  )

  def fromXML(node: Node): ProcessingStep = {
    import com.joliciel.jochre.ocr.core.utils.XmlImplicits._

    val id = node \@ "ID"

    val processingTimeNode = (node \\ "processingDateTime").headOption
    val processingTime = processingTimeNode
      .flatMap { node =>
        try {
          Some(ZonedDateTime.parse(node.textContent, dateTimeFormatter))
        } catch {
          case _: DateTimeParseException =>
            try {
              Some(ZonedDateTime.parse(node.textContent, dateFormatter))
            } catch {
              case _: DateTimeParseException =>
                None
            }
        }
      }

    val processingAgency = (node \\ "processingAgency").headOption.map(_.textContent)
    val processingStepDescriptions = (node \\ "processingStepDescription").map(_.textContent)
    val processingStepSettings = (node \\ "processingStepSettings").headOption.map(_.textContent)
    val softwareCreator = (node \\ "softwareCreator").headOption.map(_.textContent)
    val softwareName = (node \\ "softwareName").headOption.map(_.textContent)
    val softwareVersion = (node \\ "softwareVersion").headOption.map(_.textContent)
    val applicationDescription = (node \\ "applicationDescription").headOption.map(_.textContent)

    ProcessingStep(
      id = id,
      processingTime = processingTime,
      processingAgency = processingAgency,
      processingStepDescriptions = processingStepDescriptions,
      processingStepSettings = processingStepSettings,
      softwareCreator = softwareCreator,
      softwareName = softwareName,
      softwareVersion = softwareVersion,
      applicationDescription = applicationDescription
    )
  }
}
