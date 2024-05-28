package com.joliciel.jochre.ocr.core.alto

import com.joliciel.jochre.ocr.core.model.{Page, SpellingAlternative, Word}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AltoTransformerTest extends AnyFlatSpec with Matchers {
  "addAlternativesToFile" should "add alternatives to an alto file" in {
    val sillyAltoProcessor = new AltoTransformer {
      override def getAlternatives(word: Word): Set[SpellingAlternative] = Set(
        SpellingAlternative("X", f"X_${word.content}"),
        SpellingAlternative("Y", f"Y_${word.content}")
      )
    }

    val altoFile = {
        <Page>
          <Paragraph>
            <String CONTENT="Jimi"><Glyph CONTENT="J" /></String><String CONTENT="Hendrix"/>
          </Paragraph>
          <Paragraph>
            <String CONTENT="Janis"><ALTERNATIVE PURPOSE="X">X_Whatever</ALTERNATIVE><ALTERNATIVE PURPOSE="Y">Y_Janis</ALTERNATIVE><ALTERNATIVE PURPOSE="Z">Z_Janis</ALTERNATIVE></String>
          </Paragraph>
        </Page>
    }

    val page = Page.fromXML(altoFile)

    val expected =
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

    val expectedPage = Page.fromXML(expected)

    sillyAltoProcessor.processPage(page, AltoTransformerOptions().withRemoveGlyphs(true)) shouldEqual expectedPage
  }

}
