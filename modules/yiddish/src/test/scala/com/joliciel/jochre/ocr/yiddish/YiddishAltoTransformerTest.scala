package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.ocr.core.analysis.AltoAlternative
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.xml.{Elem, PrettyPrinter}
import scala.xml.transform.RuleTransformer

class YiddishAltoTransformerTest extends AnyFlatSpec with Matchers {
  private val yiddishConfig: YiddishConfig = YiddishConfig.fromConfig

  "getAlternatives" should "correctly add alternatives" in {
    val yiddishAltoProcessor = YiddishAltoTransformer(yiddishConfig, textSimplifier = None)
    yiddishAltoProcessor.getAlternatives("מעהר") shouldEqual Set(
      AltoAlternative(YiddishAltoTransformer.Purpose.YIVO.entryName, "מער"),
      AltoAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "mer")
    )

    yiddishAltoProcessor.getAlternatives("בלײ") shouldEqual Set(
      AltoAlternative(YiddishAltoTransformer.Purpose.YIVO.entryName, "בלײַ"),
      AltoAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "blay")
    )
  }

  it should "only add a YIVO alternative if it's different from the original" in {
    val yiddishAltoProcessor = YiddishAltoTransformer(yiddishConfig, textSimplifier = None)
    yiddishAltoProcessor.getAlternatives("מער") shouldEqual Set(
      AltoAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "mer")
    )
  }

  it should "find first real word if impossible shtumer alef" in {
    val yiddishAltoProcessor = YiddishAltoTransformer(yiddishConfig, textSimplifier = None)

    yiddishAltoProcessor.getAlternatives("אָװנט") shouldEqual Set(
      AltoAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "ovnt")
    )

    yiddishAltoProcessor.getAlternatives("אַבי") shouldEqual Set(
      AltoAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "abi")
    )

    yiddishAltoProcessor.getAlternatives("אײראָפּע") shouldEqual Set(
      AltoAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "eyrope")
    )

    yiddishAltoProcessor.getAlternatives("א") shouldEqual Set(
      AltoAlternative(YiddishAltoTransformer.Purpose.YIVO.entryName, "אַ"),
      AltoAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "a")
    )

    yiddishAltoProcessor.getAlternatives("װאסער") shouldEqual Set(
      AltoAlternative(YiddishAltoTransformer.Purpose.YIVO.entryName, "װאַסער"),
      AltoAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "vaser")
    )

    yiddishAltoProcessor.getAlternatives("איבערמאכן") shouldEqual Set(
      AltoAlternative(YiddishAltoTransformer.Purpose.YIVO.entryName, "איבערמאַכן"),
      AltoAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "ibermakhn")
    )

    yiddishAltoProcessor.getAlternatives("אטאם") shouldEqual Set(
      AltoAlternative(YiddishAltoTransformer.Purpose.YIVO.entryName, "אַטאָם"),
      AltoAlternative(YiddishAltoTransformer.Purpose.Roman.entryName, "atom")
    )
  }

  "process" should "keep spaces" in {
    val yiddishAltoProcessor = YiddishAltoTransformer(yiddishConfig, textSimplifier = None)

    val alto = <Page>
      <Paragraph>
        <String HPOS="10" VPOS="10" WIDTH="80" HEIGHT="80" CONTENT="Jimi" WC="0.8"></String>
        <SP HPOS="90" VPOS="10" WIDTH="20" HEIGHT="80"></SP>
        <String  HPOS="110" VPOS="10" WIDTH="80" HEIGHT="80" CONTENT ="Hendrix" WC="0.7"></String>
      </Paragraph>
    </Page>

    val actual = yiddishAltoProcessor.process(alto, "Jimi.png")

    val expected = <Page>
      <Paragraph>
        <String HPOS="10" VPOS="10" WIDTH="80" HEIGHT="80" CONTENT="Jimi" WC="0.8"></String>
        <SP HPOS="90" VPOS="10" WIDTH="20" HEIGHT="80"></SP>
        <String HPOS="110" VPOS="10" WIDTH="80" HEIGHT="80" CONTENT="Hendrix" WC="0.7"></String>
      </Paragraph>
    </Page>

    val prettyPrinter = new PrettyPrinter(120, 2)

    prettyPrinter.format(actual) shouldEqual prettyPrinter.format(expected)
  }

  it should "reverse numbers" in {
    val yiddishAltoProcessor = YiddishAltoTransformer(yiddishConfig, textSimplifier = None)
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
    val yiddishAltoProcessor = YiddishAltoTransformer(yiddishConfig, textSimplifier = None)
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

  "punctuationSplitRule" should "calculate coordinates correctly when splitting punctuation" in {
    val original = {
      <alto>
        <String HPOS="1248" VPOS="1387" WIDTH="47" HEIGHT="42" CONTENT="A." WC="0.5">
          <Glyph HPOS="1263" VPOS="1387" WIDTH="32" HEIGHT="42" CONTENT="A" GC="0.5"></Glyph>
          <Glyph HPOS="1248" VPOS="1415" WIDTH="12" HEIGHT="14" CONTENT="." GC="0.1"></Glyph>
        </String>
      </alto>
    }
    val transform = new RuleTransformer(YiddishAltoTransformer.punctuationSplitRule)
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

  "addHyphenRule" should "split off hyphens at the end of a text line" in {
    val original = {
      <alto>
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
      </alto>
    }
    val transform = new RuleTransformer(YiddishAltoTransformer.addHyphenRule)
    val actual = transform(original).asInstanceOf[Elem]
    val expected =
      <alto>
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
      </alto>

    val prettyPrinter = new PrettyPrinter(80, 2)
    prettyPrinter.format(actual) shouldEqual prettyPrinter.format(expected)
  }

  it should "correctly handle strange glyphs with two letters" in {
    // Note we write HPOS elements as if the text was right-to-left
    val original = {
      <alto>
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
      </alto>
    }
    val transform = new RuleTransformer(YiddishAltoTransformer.addHyphenRule)
    val actual = transform(original).asInstanceOf[Elem]
    val expected =
      <alto>
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
      </alto>

    val prettyPrinter = new PrettyPrinter(80, 2)
    prettyPrinter.format(actual) shouldEqual prettyPrinter.format(expected)
  }
}
