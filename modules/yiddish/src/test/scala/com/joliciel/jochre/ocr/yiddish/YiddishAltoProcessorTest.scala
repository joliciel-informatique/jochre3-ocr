package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.analysis.AltoAlternative
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.xml.PrettyPrinter

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
        <String VPOS="10" HPOS="10" HEIGHT="80" WIDTH="80" WC="0.8" CONTENT="-a,.bc.">
          <Glyph VPOS="10" HPOS="10" HEIGHT="10" WIDTH="10" GC="0.1" CONTENT="-"/>
          <Glyph VPOS="20" HPOS="20" HEIGHT="10" WIDTH="10" GC="0.2" CONTENT="a"/>
          <Glyph VPOS="30" HPOS="30" HEIGHT="10" WIDTH="10" GC="0.3" CONTENT=","/>
          <Glyph VPOS="40" HPOS="40" HEIGHT="10" WIDTH="10" GC="0.4" CONTENT="."/>
          <Glyph VPOS="50" HPOS="50" HEIGHT="10" WIDTH="10" GC="0.5" CONTENT="b"/>
          <Glyph VPOS="60" HPOS="60" HEIGHT="10" WIDTH="10" GC="0.6" CONTENT="c"/>
          <Glyph VPOS="70" HPOS="70" HEIGHT="10" WIDTH="10" GC="0.7" CONTENT="."/>
        </String>
      </Paragraph>
    </Page>
    val actual = yiddishAltoProcessor.process(alto, "Jimi.png")

    val expected = <Page>
      <Paragraph>
        <String VPOS="10" HPOS="10" HEIGHT="10" WIDTH="10" WC="0.1" CONTENT="-"></String>
        <String VPOS="20" HPOS="20" HEIGHT="10" WIDTH="10" WC="0.2" CONTENT="a"></String>
        <String VPOS="30" HPOS="30" HEIGHT="10" WIDTH="10" WC="0.3" CONTENT=","></String>
        <String VPOS="40" HPOS="40" HEIGHT="10" WIDTH="10" WC="0.4" CONTENT="."></String>
        <String VPOS="50" HPOS="50" HEIGHT="20" WIDTH="20" WC="0.8" CONTENT="bc"></String>
        <String VPOS="70" HPOS="70" HEIGHT="10" WIDTH="10" WC="0.7" CONTENT="."></String>
      </Paragraph>
    </Page>

    val prettyPrinter = new PrettyPrinter(80, 2)

    prettyPrinter.format(actual) shouldEqual prettyPrinter.format(expected)
  }
}
