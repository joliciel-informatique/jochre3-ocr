package com.joliciel.jochre.ocr.core.output

import com.joliciel.jochre.ocr.core.model.JochreImage

import java.io.{File, FileWriter, Writer}
import java.nio.charset.StandardCharsets
import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneOffset}
import scala.xml.{Elem, PrettyPrinter}

case class Alto4Writer(image: JochreImage, fileName: String) {

  def write(file: File): Unit = {
    this.write(new FileWriter(file, StandardCharsets.UTF_8))
  }

  def write(out: Writer): Unit = {
    try {
      val alto: Elem = <alto xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
              xmlns="http://www.loc.gov/standards/alto/ns-v4#"
              xsi:schemaLocation="http://www.loc.gov/standards/alto/ns-v4# http://www.loc.gov/standards/alto/v4/alto-4-4.xsd"
              xmlns:xlink="http://www.w3.org/1999/xlink">
          <Description>
            <MeasurementUnit>pixel</MeasurementUnit>
            <sourceImageInformation>
              <fileName>{fileName}</fileName>
            </sourceImageInformation>
            <Processing ID="OCR_1">
              <processingDateTime>{LocalDateTime.now().atZone(ZoneOffset.UTC).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)}</processingDateTime>
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
            <Page ID={f"PAGE${image.id}"} HEIGHT={image.height.toString} WIDTH={image.width.toString} PHYSICAL_IMG_NR={image.physicalPageNumber.toString} ROTATION={image.rotation.toString} LANG="en" PC="0.0000">
              <PrintSpace HEIGHT={image.height.toString} WIDTH={image.width.toString} HPOS="0" VPOS="0">
                {image.textBlocks.zipWithIndex.map{ case (textBlock, i) =>
                  <ComposedBlock ID={f"ComposedBlock${image.id}_${"%03d".format(i+1)}"} HEIGHT={textBlock.rectangle.height.toString} WIDTH={textBlock.rectangle.width.toString} HPOS={textBlock.rectangle.left.toString} VPOS={textBlock.rectangle.top.toString}>
                  {textBlock.paragraphs.map { paragraph =>
                    <TextBlock ID={f"TextBlock${image.id}_${"%03d".format(i+1)}"} HEIGHT={paragraph.rectangle.height.toString} WIDTH={paragraph.rectangle.width.toString} HPOS={paragraph.rectangle.left.toString} VPOS={paragraph.rectangle.top.toString}>
                    {paragraph.rows.map { row =>
                      <TextLine HEIGHT={row.baseLine.height.toString} WIDTH={row.baseLine.width.toString} HPOS={row.baseLine.x1.toString} VPOS={row.baseLine.y1.toString} BASELINE={row.baseLine.y1.toString}>
                      </TextLine>
                    }}
                    </TextBlock>
                  }}
                  </ComposedBlock>
                }}
                {image.illustrations.zipWithIndex.map { case (illustration, i) =>
                  <Illustration ID={f"Illustration${image.id}_${"%03d".format(i+1)}"} HEIGHT={illustration.rectangle.height.toString} WIDTH={illustration.rectangle.width.toString} HPOS={illustration.rectangle.left.toString} VPOS={illustration.rectangle.top.toString}>
                  </Illustration>
                }}
              </PrintSpace>
            </Page>
          </Layout>
        </alto>

      val prettyPrinter = new PrettyPrinter(80, 2)
      out.write(prettyPrinter.format(alto))
      out.flush()
    } finally {
      out.close()
    }
  }
}
