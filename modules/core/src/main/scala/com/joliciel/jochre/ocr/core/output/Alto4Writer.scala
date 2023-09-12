package com.joliciel.jochre.ocr.core.output

import com.joliciel.jochre.ocr.core.model.{ComposedBlock, Illustration, Page, TextBlock}

import java.io.{File, FileWriter, Writer}
import java.nio.charset.StandardCharsets
import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneOffset}
import scala.xml.{Elem, PrettyPrinter}

case class Alto4Writer(page: Page, fileName: String) {
  val alto: Elem = <alto xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                         xmlns="http://www.loc.gov/standards/alto/ns-v4#"
                         xsi:schemaLocation="http://www.loc.gov/standards/alto/ns-v4# http://www.loc.gov/standards/alto/v4/alto-4-4.xsd"
                         xmlns:xlink="http://www.w3.org/1999/xlink">
    <Description>
      <MeasurementUnit>pixel</MeasurementUnit>
      <sourceImageInformation>
        <fileName>
          {fileName}
        </fileName>
      </sourceImageInformation>
      <Processing ID="OCR_1">
        <processingDateTime>
          {LocalDateTime.now().atZone(ZoneOffset.UTC).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)}
        </processingDateTime>
        <processingStepDescription>contentGeneration</processingStepDescription>
        <processingSoftware>
          <softwareCreator>Joliciel Informatique</softwareCreator>
          <softwareName>Jochre</softwareName>
          <softwareVersion>3.0.1-SNAPSHOT</softwareVersion>
          <applicationDescription>Java Optical CHaracter REcognition: https://github.com/urieli/jochre</applicationDescription>
        </processingSoftware>
      </Processing>
    </Description>
    <Layout>
      {page.toXml()}
    </Layout>
  </alto>

  def write(file: File): Unit = {
    this.write(new FileWriter(file, StandardCharsets.UTF_8))
  }

  def write(out: Writer): Unit = {
    try {
      val prettyPrinter = new PrettyPrinter(80, 2)
      out.write(prettyPrinter.format(alto))
      out.flush()
    } finally {
      out.close()
    }
  }
}
