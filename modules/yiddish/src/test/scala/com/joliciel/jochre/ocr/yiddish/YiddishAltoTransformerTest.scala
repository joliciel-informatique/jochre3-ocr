package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.graphics.{ImageInfo, Rectangle}
import com.joliciel.jochre.ocr.core.model.{Page, SpellingAlternative, SubsType, TextLine, Word}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class YiddishAltoTransformerTest extends AnyFlatSpec with Matchers {
  private val yiddishConfig: YiddishConfig = YiddishConfig.fromConfig
  private val yiddishAltoProcessor = YiddishAltoTransformer(yiddishConfig)

  private def toWord(content: String) = Word(
    content = content,
    rectangle = Rectangle(0, 0, 10, 10),
    glyphs = Seq.empty,
    alternatives = Seq.empty,
    confidence = 1.0
  )
  "getAlternatives" should "correctly add alternatives" in {

    yiddishAltoProcessor.getAlternatives(toWord("מעהר")) shouldEqual Set(
      SpellingAlternative(YiddishAltoTransformer.Purpose.YIVO.entryName, "מער"),
      SpellingAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "mer")
    )

    yiddishAltoProcessor.getAlternatives(toWord("בלײ")) shouldEqual Set(
      SpellingAlternative(YiddishAltoTransformer.Purpose.YIVO.entryName, "בלײַ"),
      SpellingAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "blay")
    )
  }

  it should "only add a YIVO alternative if it's different from the original" in {
    yiddishAltoProcessor.getAlternatives(toWord("מער")) shouldEqual Set(
      SpellingAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "mer")
    )
  }

  it should "find first real word if impossible shtumer alef" in {
    yiddishAltoProcessor.getAlternatives(toWord("אָװנט")) shouldEqual Set(
      SpellingAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "ovnt")
    )

    yiddishAltoProcessor.getAlternatives(toWord("אַבי")) shouldEqual Set(
      SpellingAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "abi")
    )

    yiddishAltoProcessor.getAlternatives(toWord("אײראָפּע")) shouldEqual Set(
      SpellingAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "eyrope")
    )

    yiddishAltoProcessor.getAlternatives(toWord("א")) shouldEqual Set(
      SpellingAlternative(YiddishAltoTransformer.Purpose.YIVO.entryName, "אַ"),
      SpellingAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "a")
    )

    yiddishAltoProcessor.getAlternatives(toWord("װאסער")) shouldEqual Set(
      SpellingAlternative(YiddishAltoTransformer.Purpose.YIVO.entryName, "װאַסער"),
      SpellingAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "vaser")
    )

    yiddishAltoProcessor.getAlternatives(toWord("איבערמאכן")) shouldEqual Set(
      SpellingAlternative(YiddishAltoTransformer.Purpose.YIVO.entryName, "איבערמאַכן"),
      SpellingAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "ibermakhn")
    )

    yiddishAltoProcessor.getAlternatives(toWord("אטאם")) shouldEqual Set(
      SpellingAlternative(YiddishAltoTransformer.Purpose.YIVO.entryName, "אַטאָם"),
      SpellingAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "atom")
    )
  }

  it should "add hyphenated equivalents if it's a hyphenated word" in {
    val word = toWord("פלי").copy(subsType = Some(SubsType.HypPart1), subsContent = Some("פליגעל"))
    yiddishAltoProcessor.getAlternatives(word) shouldEqual Set(
      SpellingAlternative(YiddishAltoTransformer.Purpose.YIVO.entryName, "פֿלי"),
      SpellingAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "fli"),
      SpellingAlternative(YiddishAltoTransformer.Purpose.YIVOHyphenated.entryName, "פֿליגל"),
      SpellingAlternative(YiddishAltoTransformer.Purpose.RomanHyphenated.entryName, "fligl")
    )
  }

  "process" should "keep spaces" in {
    val alto = <Page>
      <PrintSpace>
        <TextBlock>
          <TextLine>
            <String HPOS="10" VPOS="10" WIDTH="80" HEIGHT="80" CONTENT="Jimi" WC="0.8"></String>
            <SP HPOS="90" VPOS="10" WIDTH="20" HEIGHT="80"></SP>
            <String  HPOS="110" VPOS="10" WIDTH="80" HEIGHT="80" CONTENT ="Hendrix" WC="0.7"></String>
          </TextLine>
        </TextBlock>
      </PrintSpace>
    </Page>

    val page = Page.fromXML(alto)

    val actualPage = yiddishAltoProcessor.process(page)

    actualPage shouldEqual page
  }

  it should "reverse numbers" in {
    val alto = <Page>
      <PrintSpace>
        <TextBlock>
          <TextLine>
            <String CONTENT="Jimi">
              <Glyph CONTENT="J"/>
              <Glyph CONTENT="i"/>
              <Glyph CONTENT="m"/>
              <Glyph CONTENT="i"/>
            </String>
          </TextLine>
          <TextLine>
            <String CONTENT="24" WC="80">
              <Glyph CONTENT="2"/>
              <Glyph CONTENT="4"/>
            </String>
            <String CONTENT="123.45" WC="50">
              <Glyph CONTENT="1"/>
              <Glyph CONTENT="2"/>
              <Glyph CONTENT="3"/>
              <Glyph CONTENT="."/>
              <Glyph CONTENT="4"/>
              <Glyph CONTENT="5"/>
            </String>
            <String CONTENT="71טן" WC="80">
              <Glyph CONTENT="7"/>
              <Glyph CONTENT="1"/>
              <Glyph CONTENT="ט"/>
              <Glyph CONTENT="ן"/>
            </String>
          </TextLine>
        </TextBlock>
      </PrintSpace>
    </Page>
    val page = Page.fromXML(alto)

    val actualPage = yiddishAltoProcessor.process(page)

    val expected = <Page>
      <PrintSpace>
        <TextBlock>
          <TextLine>
            <String CONTENT="Jimi"></String>
          </TextLine>
          <TextLine>
            <String CONTENT="42" WC="80"></String>
            <String CONTENT="54.321" WC="50"></String>
            <String CONTENT="17טן" WC="80">
              <ALTERNATIVE PURPOSE="Roman">17tn</ALTERNATIVE>
            </String>
          </TextLine>
        </TextBlock>
      </PrintSpace>
    </Page>

    val expectedPage = Page.fromXML(expected)

    actualPage.withoutIds shouldEqual expectedPage.withoutIds
  }

  it should "split on punctuation" in {
    val alto = <Page>
      <PrintSpace>
        <TextBlock>
          <TextLine>
            <String HPOS="10" VPOS="10" WIDTH="80" HEIGHT="80" CONTENT=";a,.bc." WC="0.8">
              <Glyph HPOS="10" VPOS="10" WIDTH="10" HEIGHT="10" CONTENT=";" GC="0.1"/>
              <Glyph HPOS="20" VPOS="20" WIDTH="10" HEIGHT="10" CONTENT="a" GC="0.2"/>
              <Glyph HPOS="30" VPOS="30" WIDTH="10" HEIGHT="10" CONTENT="," GC="0.3"/>
              <Glyph HPOS="40" VPOS="40" WIDTH="10" HEIGHT="10" CONTENT="." GC="0.4"/>
              <Glyph HPOS="50" VPOS="50" WIDTH="10" HEIGHT="10" CONTENT="b" GC="0.5"/>
              <Glyph HPOS="60" VPOS="60" WIDTH="10" HEIGHT="10" CONTENT="c" GC="0.6"/>
              <Glyph HPOS="70" VPOS="70" WIDTH="10" HEIGHT="10" CONTENT="." GC="0.7"/>
            </String>
          </TextLine>
        </TextBlock>
      </PrintSpace>
    </Page>
    val page = Page.fromXML(alto)

    val actualPage = yiddishAltoProcessor.process(page)

    val expected = <Page>
      <PrintSpace>
        <TextBlock>
          <TextLine>
            <String HPOS="10" VPOS="10" WIDTH="10" HEIGHT="10" CONTENT=";" WC="0.1"></String>
            <String HPOS="20" VPOS="20" WIDTH="10" HEIGHT="10" CONTENT="a" WC="0.8"></String>
            <String HPOS="30" VPOS="30" WIDTH="20" HEIGHT="20" CONTENT=",." WC="0.35"></String>
            <String HPOS="50" VPOS="50" WIDTH="20" HEIGHT="20" CONTENT="bc" WC="0.8"></String>
            <String HPOS="70" VPOS="70" WIDTH="10" HEIGHT="10" CONTENT="." WC="0.7"></String>
          </TextLine>
        </TextBlock>
      </PrintSpace>
    </Page>

    val expectedPage = Page.fromXML(expected)

    actualPage.withoutIds shouldEqual expectedPage.withoutIds
  }

  it should "correctly remove non-abbreviation apostrophes from words" in {
    val alto = {
      <Page>
        <PrintSpace>
          <TextBlock>
            <TextLine>
              <String HPOS="662" VPOS="2992" WIDTH="521" HEIGHT="118" CONTENT="נאָך-פּסח'דיגען" WC="0.87">
                <Glyph HPOS="1148" VPOS="2993" WIDTH="40" HEIGHT="117" CONTENT="נ" GC="0.7">
                </Glyph>
                <Glyph HPOS="1099" VPOS="2993" WIDTH="49" HEIGHT="117" CONTENT="אָ" GC="0.8">
                </Glyph>
                <Glyph HPOS="1052" VPOS="2993" WIDTH="47" HEIGHT="117" CONTENT="ך" GC="0.28">
                </Glyph>
                <Glyph HPOS="1020" VPOS="2993" WIDTH="32" HEIGHT="117" CONTENT="-" GC="0.97">
                </Glyph>
                <Glyph HPOS="971" VPOS="2992" WIDTH="49" HEIGHT="118" CONTENT="פּ" GC="0.82">
                </Glyph>
                <Glyph HPOS="920" VPOS="2992" WIDTH="51" HEIGHT="117" CONTENT="ס" GC="0.84">
                </Glyph>
                <Glyph HPOS="875" VPOS="2992" WIDTH="45" HEIGHT="117" CONTENT="ח" GC="0.75">
                </Glyph>
                <Glyph HPOS="843" VPOS="2992" WIDTH="32" HEIGHT="117" CONTENT="'" GC="0.97">
                </Glyph>
                <Glyph HPOS="794" VPOS="2992" WIDTH="49" HEIGHT="117" CONTENT="ד" GC="0.79">
                </Glyph>
                <Glyph HPOS="768" VPOS="2992" WIDTH="26" HEIGHT="117" CONTENT="י" GC="0.99">
                </Glyph>
                <Glyph HPOS="735" VPOS="2992" WIDTH="33" HEIGHT="117" CONTENT="ג" GC="0.8">
                </Glyph>
                <Glyph HPOS="689" VPOS="2992" WIDTH="46" HEIGHT="117" CONTENT="ע" GC="0.98">
                </Glyph>
                <Glyph HPOS="658" VPOS="2992" WIDTH="31" HEIGHT="117" CONTENT="ן" GC="0.99">
                </Glyph>
              </String>
            </TextLine>
          </TextBlock>
        </PrintSpace>
      </Page>
    }
    val page = Page.fromXML(alto)

    val actualPage = yiddishAltoProcessor.process(page)
    val expected = {
      <Page>
        <PrintSpace>
          <TextBlock>
            <TextLine>
              <String HPOS="1052" VPOS="2993" WIDTH="136" HEIGHT="117" CONTENT="נאָך" WC="0.87">
                <ALTERNATIVE PURPOSE="Roman">nokh</ALTERNATIVE>
              </String>
              <String HPOS="1020" VPOS="2993" WIDTH="32" HEIGHT="117" CONTENT="־" WC="0.97">
                <ALTERNATIVE PURPOSE="Roman">-</ALTERNATIVE>
              </String>
              <String HPOS="658" VPOS="2992" WIDTH="362" HEIGHT="118" CONTENT="פּסח’דיגען" WC="0.87">
                <ALTERNATIVE PURPOSE="Roman">peysekhdikn</ALTERNATIVE>
                <ALTERNATIVE PURPOSE="YIVO">פּסחדיקן</ALTERNATIVE>
              </String>
            </TextLine>
          </TextBlock>
        </PrintSpace>
      </Page>
    }

    val expectedPage = Page.fromXML(expected)

    actualPage.withoutIds shouldEqual expectedPage.withoutIds
  }

  "punctuationSplitRule" should "calculate coordinates correctly when splitting punctuation" in {
    val alto =
      <TextLine HPOS="0" VPOS="10" WIDTH="500" HEIGHT="500">
        <String HPOS="1248" VPOS="1387" WIDTH="47" HEIGHT="42" CONTENT="A." WC="0.5">
          <Glyph HPOS="1263" VPOS="1387" WIDTH="32" HEIGHT="42" CONTENT="A" GC="0.5"></Glyph>
          <Glyph HPOS="1248" VPOS="1415" WIDTH="12" HEIGHT="14" CONTENT="." GC="0.1"></Glyph>
        </String>
      </TextLine>

    val imageInfo = ImageInfo(200, 200, 0)
    val textLine = TextLine.fromXML(imageInfo, alto)

    val transform = YiddishAltoTransformer.punctuationSplitRule()

    val actualTextLine = transform(textLine).asInstanceOf[TextLine]

    val expected =
      <TextLine HPOS="0" VPOS="10" WIDTH="500" HEIGHT="500">
        <String HPOS="1263" VPOS="1387" WIDTH="32" HEIGHT="42" CONTENT="A" WC="0.5">
          <Glyph HPOS="1263" VPOS="1387" WIDTH="32" HEIGHT="42" CONTENT="A" GC="0.5"></Glyph>
        </String>
        <String HPOS="1248" VPOS="1415" WIDTH="12" HEIGHT="14" CONTENT="." WC="0.1">
          <Glyph HPOS="1248" VPOS="1415" WIDTH="12" HEIGHT="14" CONTENT="." GC="0.1"></Glyph>
        </String>
      </TextLine>

    val expectedTextLine = TextLine.fromXML(imageInfo, expected)

    actualTextLine shouldEqual expectedTextLine
  }

  it should "not split on apostrophes for possessives (non-standard)" in {
    val alto =
      <TextLine HPOS="0" VPOS="10" WIDTH="500" HEIGHT="500">
        <String HPOS="1000" VPOS="1000" WIDTH="100" HEIGHT="42" CONTENT="A‛B" WC="0.5">
          <Glyph HPOS="1000" VPOS="1000" WIDTH="45" HEIGHT="42" CONTENT="A" GC="0.5"></Glyph>
          <Glyph HPOS="1045" VPOS="1000" WIDTH="10" HEIGHT="14" CONTENT="‛" GC="0.1"></Glyph>
          <Glyph HPOS="1055" VPOS="1000" WIDTH="45" HEIGHT="42" CONTENT="B" GC="0.1"></Glyph>
        </String>
      </TextLine>

    val imageInfo = ImageInfo(200, 200, 0)
    val textLine = TextLine.fromXML(imageInfo, alto)

    val transform = YiddishAltoTransformer.punctuationSplitRule()

    val actualTextLine = transform(textLine).asInstanceOf[TextLine]
    actualTextLine shouldEqual textLine
  }

  it should "not split on double-quotes for abbreviations (standard)" in {
    val alto =
      <TextLine HPOS="0" VPOS="10" WIDTH="500" HEIGHT="500">
        <String HPOS="1000" VPOS="1000" WIDTH="100" HEIGHT="42" CONTENT="A“B" WC="0.5">
          <Glyph HPOS="1000" VPOS="1000" WIDTH="45" HEIGHT="42" CONTENT="A" GC="0.5"></Glyph>
          <Glyph HPOS="1045" VPOS="1000" WIDTH="10" HEIGHT="14" CONTENT="“" GC="0.1"></Glyph>
          <Glyph HPOS="1055" VPOS="1000" WIDTH="45" HEIGHT="42" CONTENT="B" GC="0.1"></Glyph>
        </String>
      </TextLine>

    val imageInfo = ImageInfo(200, 200, 0)
    val textLine = TextLine.fromXML(imageInfo, alto)

    val transform = YiddishAltoTransformer.punctuationSplitRule()

    val actualTextLine = transform(textLine).asInstanceOf[TextLine]
    actualTextLine shouldEqual textLine
  }

  it should "not split on double-quotes for abbreviations when surrounded by quotes" in {
    val alto =
      <TextLine HPOS="0" VPOS="10" WIDTH="500" HEIGHT="500">
        <String HPOS="1000" VPOS="1000" WIDTH="60" HEIGHT="42" CONTENT="„AA“B“" WC="0.5">
          <Glyph HPOS="1000" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="„" GC="0.5"></Glyph>
          <Glyph HPOS="1010" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="A" GC="0.5"></Glyph>
          <Glyph HPOS="1020" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="A" GC="0.5"></Glyph>
          <Glyph HPOS="1030" VPOS="1000" WIDTH="10" HEIGHT="14" CONTENT="“" GC="0.5"></Glyph>
          <Glyph HPOS="1040" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="B" GC="0.5"></Glyph>
          <Glyph HPOS="1050" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="“" GC="0.5"></Glyph>
        </String>
      </TextLine>

    val imageInfo = ImageInfo(200, 200, 0)
    val textLine = TextLine.fromXML(imageInfo, alto)

    val transform = YiddishAltoTransformer.punctuationSplitRule()

    val actualTextLine = transform(textLine).asInstanceOf[TextLine]
    val expected =
      <TextLine HPOS="0" VPOS="10" WIDTH="500" HEIGHT="500">
        <String HPOS="1000" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="„" WC="0.5">
          <Glyph HPOS="1000" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="„" GC="0.5"></Glyph>
        </String>
        <String HPOS="1010" VPOS="1000" WIDTH="40" HEIGHT="42" CONTENT="AA“B" WC="0.5">
          <Glyph HPOS="1010" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="A" GC="0.5"></Glyph>
          <Glyph HPOS="1020" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="A" GC="0.5"></Glyph>
          <Glyph HPOS="1030" VPOS="1000" WIDTH="10" HEIGHT="14" CONTENT="“" GC="0.5"></Glyph>
          <Glyph HPOS="1040" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="B" GC="0.5"></Glyph>
        </String>
        <String HPOS="1050" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="“" WC="0.5">
          <Glyph HPOS="1050" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="“" GC="0.5"></Glyph>
        </String>
      </TextLine>

    val expectedTextLine = TextLine.fromXML(imageInfo, expected)

    actualTextLine shouldEqual expectedTextLine
  }

  it should "split correctly on multiple punctation and apostrophe" in {
    val alto =
      <TextLine HPOS="0" VPOS="10" WIDTH="500" HEIGHT="500">
        <String HPOS="1000" VPOS="1000" WIDTH="60" HEIGHT="42" CONTENT="„A“BB“" WC="0.5">
          <Glyph HPOS="1000" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="„" GC="0.5"></Glyph>
          <Glyph HPOS="1010" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="A" GC="0.5"></Glyph>
          <Glyph HPOS="1020" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="‛" GC="0.5"></Glyph>
          <Glyph HPOS="1030" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="B" GC="0.5"></Glyph>
          <Glyph HPOS="1040" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="B" GC="0.5"></Glyph>
          <Glyph HPOS="1050" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="“" GC="0.5"></Glyph>
        </String>
      </TextLine>

    val imageInfo = ImageInfo(200, 200, 0)
    val textLine = TextLine.fromXML(imageInfo, alto)

    val transform = YiddishAltoTransformer.punctuationSplitRule()

    val actualTextLine = transform(textLine).asInstanceOf[TextLine]

    val expected =
      <TextLine HPOS="0" VPOS="10" WIDTH="500" HEIGHT="500">
        <String HPOS="1000" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="„" WC="0.5">
          <Glyph HPOS="1000" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="„" GC="0.5"></Glyph>
        </String>
        <String HPOS="1010" VPOS="1000" WIDTH="40" HEIGHT="42" CONTENT="A‛BB" WC="0.5">
          <Glyph HPOS="1010" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="A" GC="0.5"></Glyph>
          <Glyph HPOS="1020" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="‛" GC="0.5"></Glyph>
          <Glyph HPOS="1030" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="B" GC="0.5"></Glyph>
          <Glyph HPOS="1040" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="B" GC="0.5"></Glyph>
        </String>
        <String HPOS="1050" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="“" WC="0.5">
          <Glyph HPOS="1050" VPOS="1000" WIDTH="10" HEIGHT="42" CONTENT="“" GC="0.5"></Glyph>
        </String>
      </TextLine>

    val expectedTextLine = TextLine.fromXML(imageInfo, expected)

    actualTextLine shouldEqual expectedTextLine
  }

  it should "work with an opening double comma" in {
    val alto =
      <TextLine HPOS="0" VPOS="10" WIDTH="500" HEIGHT="500">
        <String HPOS="659" VPOS="2612" WIDTH="250" HEIGHT="110" CONTENT="„קעני“" WC="0.62">
          <Glyph HPOS="875" VPOS="2612" WIDTH="34" HEIGHT="110" CONTENT=",," GC="0.83">
          </Glyph>
          <Glyph HPOS="811" VPOS="2612" WIDTH="63" HEIGHT="110" CONTENT="ק" GC="0.48">
          </Glyph>
          <Glyph HPOS="764" VPOS="2612" WIDTH="46" HEIGHT="110" CONTENT="ע" GC="0.99">
          </Glyph>
          <Glyph HPOS="732" VPOS="2612" WIDTH="31" HEIGHT="110" CONTENT="נ" GC="0.87">
          </Glyph>
          <Glyph HPOS="698" VPOS="2612" WIDTH="33" HEIGHT="110" CONTENT="י" GC="0.98">
          </Glyph>
          <Glyph HPOS="681" VPOS="2612" WIDTH="16" HEIGHT="110" CONTENT="'" GC="0.79">
          </Glyph>
          <Glyph HPOS="661" VPOS="2612" WIDTH="19" HEIGHT="110" CONTENT="'" GC="0.93">
          </Glyph>
        </String>
      </TextLine>

    val imageInfo = ImageInfo(200, 200, 0)
    val textLine = TextLine.fromXML(imageInfo, alto)

    val transform = YiddishAltoTransformer.punctuationSplitRule()

    val actualTextLine = transform(textLine).asInstanceOf[TextLine]
    val actualWords = actualTextLine.words.map(_.content)

    val expectedWords = Seq(",,", "קעני", "''")

    actualWords shouldEqual expectedWords
  }

  "addHyphenRule" should "split off hyphens at the end of a text line" in {
    val alto =
      <TextLine HPOS="0" VPOS="10" WIDTH="90" HEIGHT="100">
          <String HPOS="0" VPOS="10" WIDTH="40" HEIGHT="100" CONTENT="Jimi" WC="0.5">
            <Glyph HPOS="0" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="J" GC="0.5"></Glyph>
            <Glyph HPOS="10" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="i" GC="0.5"></Glyph>
            <Glyph HPOS="20" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="m" GC="0.5"></Glyph>
            <Glyph HPOS="30" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="i" GC="0.5"></Glyph>
          </String>
          <SP HPOS="40" VPOS="10" WIDTH="10" HEIGHT="100"></SP>
          <String HPOS="50" VPOS="10" WIDTH="40" HEIGHT="100" CONTENT="Hen־" WC="0.5">
            <Glyph HPOS="50" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="H" GC="0.5"></Glyph>
            <Glyph HPOS="60" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="e" GC="0.5"></Glyph>
            <Glyph HPOS="70" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="n" GC="0.5"></Glyph>
            <Glyph HPOS="80" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="־" GC="0.5"></Glyph>
          </String>
        </TextLine>

    val imageInfo = ImageInfo(200, 200, 0)
    val textLine = TextLine.fromXML(imageInfo, alto)

    val transform = YiddishAltoTransformer.addHyphenRule()

    val actualTextLine = transform(textLine).asInstanceOf[TextLine]

    val expected =
      <TextLine HPOS="0" VPOS="10" WIDTH="90" HEIGHT="100">
          <String HPOS="0" VPOS="10" WIDTH="40" HEIGHT="100" CONTENT="Jimi" WC="0.5">
            <Glyph HPOS="0" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="J" GC="0.5"></Glyph>
            <Glyph HPOS="10" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="i" GC="0.5"></Glyph>
            <Glyph HPOS="20" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="m" GC="0.5"></Glyph>
            <Glyph HPOS="30" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="i" GC="0.5"></Glyph>
          </String>
          <SP HPOS="40" VPOS="10" WIDTH="10" HEIGHT="100"></SP>
          <String HPOS="50" VPOS="10" WIDTH="30" HEIGHT="100" CONTENT="Hen" WC="0.5">
            <Glyph HPOS="50" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="H" GC="0.5"></Glyph>
            <Glyph HPOS="60" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="e" GC="0.5"></Glyph>
            <Glyph HPOS="70" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="n" GC="0.5"></Glyph>
          </String>
          <HYP HPOS="80" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="־"></HYP>
        </TextLine>

    val expectedTextLine = TextLine.fromXML(imageInfo, expected)

    actualTextLine shouldEqual expectedTextLine
  }

  it should "correctly handle strange glyphs with two letters" in {
    // Note we write HPOS elements as if the text was right-to-left
    val alto =
      <TextLine HPOS="10" VPOS="10" WIDTH="90" HEIGHT="100">
          <String HPOS="70" VPOS="10" WIDTH="40" HEIGHT="100" CONTENT="Jimi" WC="0.5">
            <Glyph HPOS="100" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="J" GC="0.5"></Glyph>
            <Glyph HPOS="90" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="i" GC="0.5"></Glyph>
            <Glyph HPOS="80" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="m" GC="0.5"></Glyph>
            <Glyph HPOS="70" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="i" GC="0.5"></Glyph>
          </String>
          <SP HPOS="60" VPOS="10" WIDTH="20" HEIGHT="100"></SP>
          <String HPOS="10" VPOS="10" WIDTH="40" HEIGHT="100" CONTENT="Hen־" WC="0.5">
            <Glyph HPOS="40" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="H" GC="0.5"></Glyph>
            <Glyph HPOS="30" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="e" GC="0.5"></Glyph>
            <Glyph HPOS="10" VPOS="10" WIDTH="20" HEIGHT="100" CONTENT="n־" GC="0.5"></Glyph>
          </String>
        </TextLine>

    val imageInfo = ImageInfo(200, 200, 0)
    val textLine = TextLine.fromXML(imageInfo, alto)

    val transform = YiddishAltoTransformer.addHyphenRule()

    val actualTextLine = transform(textLine).asInstanceOf[TextLine]
    val expected =
      <TextLine HPOS="10" VPOS="10" WIDTH="90" HEIGHT="100">
          <String HPOS="70" VPOS="10" WIDTH="40" HEIGHT="100" CONTENT="Jimi" WC="0.5">
            <Glyph HPOS="100" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="J" GC="0.5"></Glyph>
            <Glyph HPOS="90" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="i" GC="0.5"></Glyph>
            <Glyph HPOS="80" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="m" GC="0.5"></Glyph>
            <Glyph HPOS="70" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="i" GC="0.5"></Glyph>
          </String>
          <SP HPOS="60" VPOS="10" WIDTH="20" HEIGHT="100"></SP>
          <String HPOS="20" VPOS="10" WIDTH="30" HEIGHT="100" CONTENT="Hen" WC="0.5">
            <Glyph HPOS="40" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="H" GC="0.5"></Glyph>
            <Glyph HPOS="30" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="e" GC="0.5"></Glyph>
            <Glyph HPOS="20" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="n" GC="0.5"></Glyph>
          </String>
          <HYP HPOS="10" VPOS="10" WIDTH="10" HEIGHT="100" CONTENT="־"></HYP>
        </TextLine>

    val expectedTextLine = TextLine.fromXML(imageInfo, expected)

    actualTextLine shouldEqual expectedTextLine
  }
}
