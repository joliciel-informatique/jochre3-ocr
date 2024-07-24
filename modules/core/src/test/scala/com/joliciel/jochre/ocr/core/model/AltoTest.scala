package com.joliciel.jochre.ocr.core.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AltoTest extends AnyFlatSpec with Matchers {
  "processedText" should "correctly convert Alto to text" in {
    val altoFile = {
      <Page>
        <PrintSpace>
        <TextBlock>
          <TextLine>
            <String CONTENT="Jimi" /><SP /><String CONTENT="Hend" SUBS_CONTENT="Hendrix"/><HYP CONTENT="-" />
          </TextLine>
          <TextLine>
            <String CONTENT="rix" SUBS_CONTENT="Hendrix" /><SP /><String CONTENT="Experience"/>
          </TextLine>
        </TextBlock>
        </PrintSpace>
      </Page>
    }

    val page = Page.fromXML(altoFile)
    page.processedContent shouldEqual "Jimi Hendrix Experience"
  }
}
