package com.joliciel.jochre.ocr.core.alto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.xml.PrettyPrinter

class AltoTransformerTest extends AnyFlatSpec
  with Matchers
{
  "addAlternativesToFile" should "add alternatives to an alto file" in {
    val fileName = "JimiHendrix.png"
    val sillyAltoProcessor = new AltoTransformer {
      override val removeGlyphs: Boolean = true
      override def getAlternatives(content: String): Set[AltoAlternative] = Set(
        AltoAlternative("X", f"X_${content}"),
        AltoAlternative("Y", f"Y_${content}")
      )
    }

    val altoFile = {
      <alto>
        <Description>
          <sourceImageInformation>
            <fileName>jochre15708220328454552598.jpg</fileName>
          </sourceImageInformation>
          <Processing ID="OCR_1">
            <processingSoftware>
              <softwareCreator>Joliciel Informatique</softwareCreator>
              <softwareName>Jochre</softwareName>
              <softwareVersion>unknown</softwareVersion>
              <applicationDescription>Java Optical CHaracter REcognition: https://github.com/urieli/jochre</applicationDescription>
            </processingSoftware>
          </Processing>
        </Description>
        <Page>
          <Paragraph>
            <String CONTENT="Jimi"><Glyph CONTENT="J" /></String><String CONTENT="Hendrix"/>
          </Paragraph>
          <Paragraph>
            <String CONTENT="Janis"><ALTERNATIVE PURPOSE="X">X_Whatever</ALTERNATIVE><ALTERNATIVE PURPOSE="Y">Y_Janis</ALTERNATIVE><ALTERNATIVE PURPOSE="Z">Z_Janis</ALTERNATIVE></String>
          </Paragraph>
        </Page>
      </alto>
    }

    val expected =
      <alto>
        <Description>
          <sourceImageInformation>
            <fileName>{fileName}</fileName>
          </sourceImageInformation>
          <Processing ID="OCR_1">
            <processingSoftware>
              <softwareCreator>Joliciel Informatique</softwareCreator>
              <softwareName>Jochre</softwareName>
              <softwareVersion>{sillyAltoProcessor.ocrVersion}</softwareVersion>
              <applicationDescription>Java Optical CHaracter REcognition: https://github.com/urieli/jochre</applicationDescription>
            </processingSoftware>
          </Processing>
        </Description>
        <Page>
          <Paragraph>
            <String CONTENT="Jimi">
              <ALTERNATIVE PURPOSE="X">X_Jimi</ALTERNATIVE> <ALTERNATIVE PURPOSE="Y">Y_Jimi</ALTERNATIVE>
            </String> <String CONTENT="Hendrix">
            <ALTERNATIVE PURPOSE="X">X_Hendrix</ALTERNATIVE> <ALTERNATIVE PURPOSE="Y">Y_Hendrix</ALTERNATIVE>
          </String>
          </Paragraph>
          <Paragraph>
            <String CONTENT="Janis">
              <ALTERNATIVE PURPOSE="X">X_Janis</ALTERNATIVE> <ALTERNATIVE PURPOSE="X">X_Whatever</ALTERNATIVE> <ALTERNATIVE PURPOSE="Y">Y_Janis</ALTERNATIVE> <ALTERNATIVE PURPOSE="Z">Z_Janis</ALTERNATIVE>
            </String>
          </Paragraph>
        </Page>
      </alto>

    val prettyPrinter = new PrettyPrinter(80, 2)

    prettyPrinter.format(sillyAltoProcessor.process(altoFile, fileName)) shouldEqual prettyPrinter.format(expected)
  }

}
