package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.analysis.AltoAlternative
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.xml.{Elem, PrettyPrinter}
import scala.xml.transform.RuleTransformer

class YiddishAltoProcessorTest extends AnyFlatSpec with Matchers {
  private val yiddishConfig: YiddishConfig = YiddishConfig.fromConfig

  "A YiddishAltoProcessor" should "correctly add alternatives" in {
    val yiddishAltoProcessor = YiddishAltoProcessor(yiddishConfig)
    yiddishAltoProcessor.getAlternatives("מעהר") shouldEqual Set(
      AltoAlternative(YiddishAltoProcessor.Purpose.YIVO.entryName, "מער"),
      AltoAlternative(YiddishAltoProcessor.Purpose.Roman.entryName, "mer")
    )

    yiddishAltoProcessor.getAlternatives("בלײ") shouldEqual Set(
      AltoAlternative(YiddishAltoProcessor.Purpose.YIVO.entryName, "בלײַ"),
      AltoAlternative(YiddishAltoProcessor.Purpose.Roman.entryName, "blay")
    )
  }

  it should "only add a YIVO alternative if it's different from the original" in {
    val yiddishAltoProcessor = YiddishAltoProcessor(yiddishConfig)
    yiddishAltoProcessor.getAlternatives("מער") shouldEqual Set(
      AltoAlternative(YiddishAltoProcessor.Purpose.Roman.entryName, "mer")
    )
  }

  it should "find first real word if impossible shtumer alef" in {
    val yiddishAltoProcessor = YiddishAltoProcessor(yiddishConfig)

    yiddishAltoProcessor.getAlternatives("אָװנט") shouldEqual Set(
      AltoAlternative(YiddishAltoProcessor.Purpose.Roman.entryName, "ovnt")
    )

    yiddishAltoProcessor.getAlternatives("אַבי") shouldEqual Set(
      AltoAlternative(YiddishAltoProcessor.Purpose.Roman.entryName, "abi")
    )

    yiddishAltoProcessor.getAlternatives("אײראָפּע") shouldEqual Set(
      AltoAlternative(YiddishAltoProcessor.Purpose.Roman.entryName, "eyrope")
    )

    yiddishAltoProcessor.getAlternatives("א") shouldEqual Set(
      AltoAlternative(YiddishAltoProcessor.Purpose.YIVO.entryName, "אַ"),
      AltoAlternative(YiddishAltoProcessor.Purpose.Roman.entryName, "a")
    )

    yiddishAltoProcessor.getAlternatives("װאסער") shouldEqual Set(
      AltoAlternative(YiddishAltoProcessor.Purpose.YIVO.entryName, "װאַסער"),
      AltoAlternative(YiddishAltoProcessor.Purpose.Roman.entryName, "vaser")
    )

    yiddishAltoProcessor.getAlternatives("איבערמאכן") shouldEqual Set(
      AltoAlternative(YiddishAltoProcessor.Purpose.YIVO.entryName, "איבערמאַכן"),
      AltoAlternative(YiddishAltoProcessor.Purpose.Roman.entryName, "ibermakhn")
    )

    yiddishAltoProcessor.getAlternatives("אטאם") shouldEqual Set(
      AltoAlternative(YiddishAltoProcessor.Purpose.YIVO.entryName, "אַטאָם"),
      AltoAlternative(YiddishAltoProcessor.Purpose.Roman.entryName, "atom")
    )
  }

  it should "reverse numbers" in {
    val yiddishAltoProcessor = YiddishAltoProcessor(yiddishConfig)
    val alto = <Page>
      <Paragraph>
        <String CONTENT="Jimi">
          <Glyph CONTENT="J"/>
          <Glyph CONTENT="i"/>
          <Glyph CONTENT="m"/>
          <Glyph CONTENT="i"/>
        </String>
      </Paragraph>
      <Paragraph>
        <String CONTENT="24" WC="80">
          <Glyph CONTENT="2"/>
          <Glyph CONTENT="4"/>
        </String>
        <String CONTENT="123.45">
          <Glyph CONTENT="1"/>
          <Glyph CONTENT="2"/>
          <Glyph CONTENT="3"/>
          <Glyph CONTENT="."/>
          <Glyph CONTENT="4"/>
          <Glyph CONTENT="5"/>
        </String>
      </Paragraph>
    </Page>
    val actual = yiddishAltoProcessor.process(alto, "Jimi.png")

    val expected = <Page>
      <Paragraph>
        <String CONTENT="Jimi"></String>
      </Paragraph>
      <Paragraph>
        <String CONTENT="42" WC="80"></String>
        <String CONTENT="54.321"></String>
      </Paragraph>
    </Page>

    val prettyPrinter = new PrettyPrinter(120, 2)

    prettyPrinter.format(actual) shouldEqual prettyPrinter.format(expected)
  }

  it should "split on punctuation" in {
    val yiddishAltoProcessor = YiddishAltoProcessor(yiddishConfig)
    val alto = <Page>
      <Paragraph>
        <String HPOS="10" VPOS="10" WIDTH="80" HEIGHT="80" CONTENT="-a,.bc." WC="0.8">
          <Glyph HPOS="10" VPOS="10" WIDTH="10" HEIGHT="10" CONTENT="-" GC="0.1"/>
          <Glyph HPOS="20" VPOS="20" WIDTH="10" HEIGHT="10" CONTENT="a" GC="0.2"/>
          <Glyph HPOS="30" VPOS="30" WIDTH="10" HEIGHT="10" CONTENT="," GC="0.3"/>
          <Glyph HPOS="40" VPOS="40" WIDTH="10" HEIGHT="10" CONTENT="." GC="0.4"/>
          <Glyph HPOS="50" VPOS="50" WIDTH="10" HEIGHT="10" CONTENT="b" GC="0.5"/>
          <Glyph HPOS="60" VPOS="60" WIDTH="10" HEIGHT="10" CONTENT="c" GC="0.6"/>
          <Glyph HPOS="70" VPOS="70" WIDTH="10" HEIGHT="10" CONTENT="." GC="0.7"/>
        </String>
      </Paragraph>
    </Page>
    val actual = yiddishAltoProcessor.process(alto, "Jimi.png")

    val expected = <Page>
      <Paragraph>
        <String HPOS="10" VPOS="10" WIDTH="10" HEIGHT="10" CONTENT="-" WC="0.1"></String>
        <String HPOS="20" VPOS="20" WIDTH="10" HEIGHT="10" CONTENT="a" WC="0.2"></String>
        <String HPOS="30" VPOS="30" WIDTH="10" HEIGHT="10" CONTENT="," WC="0.3"></String>
        <String HPOS="40" VPOS="40" WIDTH="10" HEIGHT="10" CONTENT="." WC="0.4"></String>
        <String HPOS="50" VPOS="50" WIDTH="20" HEIGHT="20" CONTENT="bc" WC="0.8"></String>
        <String HPOS="70" VPOS="70" WIDTH="10" HEIGHT="10" CONTENT="." WC="0.7"></String>
      </Paragraph>
    </Page>

    val prettyPrinter = new PrettyPrinter(80, 2)

    prettyPrinter.format(actual) shouldEqual prettyPrinter.format(expected)
  }

  "punctuationSplitRule" should "calculate coordinates correctly when splitting punctutation" in {
    val original = {
      <alto>
        <String HPOS="1248" VPOS="1387" WIDTH="47" HEIGHT="42" CONTENT="A." WC="0.5">
          <Glyph HPOS="1263" VPOS="1387" WIDTH="32" HEIGHT="42" CONTENT="A" GC="0.5"></Glyph>
          <Glyph HPOS="1248" VPOS="1415" WIDTH="12" HEIGHT="14" CONTENT="." GC="0.1"></Glyph>
        </String>
      </alto>
    }
    val transform = new RuleTransformer(YiddishAltoProcessor.punctuationSplitRule)
    val actual = transform(original).asInstanceOf[Elem]
    val expected =
      <alto>
        <String HPOS="1263" VPOS="1387" WIDTH="32" HEIGHT="42" CONTENT="A" WC="0.5">
          <Glyph HPOS="1263" VPOS="1387" WIDTH="32" HEIGHT="42" CONTENT="A" GC="0.5"></Glyph>
        </String>
        <String HPOS="1248" VPOS="1415" WIDTH="12" HEIGHT="14" CONTENT="." WC="0.1">
          <Glyph HPOS="1248" VPOS="1415" WIDTH="12" HEIGHT="14" CONTENT="." GC="0.1"></Glyph>
        </String>
      </alto>

    val prettyPrinter = new PrettyPrinter(80, 2)
    prettyPrinter.format(actual) shouldEqual prettyPrinter.format(expected)
  }
}
