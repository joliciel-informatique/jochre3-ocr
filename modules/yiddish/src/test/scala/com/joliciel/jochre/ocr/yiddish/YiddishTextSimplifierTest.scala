package com.joliciel.jochre.ocr.yiddish

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class YiddishTextSimplifierTest extends AnyFlatSpec with Matchers {
  "YiddishTextSimplifier" should "simplify yiddish orthography" in {
    YiddishTextSimpifier.simplify("זייַנען") shouldEqual "זײַנען"
    YiddishTextSimpifier.simplify("ייִדן") shouldEqual "ייִדן"
    YiddishTextSimpifier.simplify("פּרוּוון") shouldEqual "פּרוּװן"
    YiddishTextSimpifier.simplify("איידער") shouldEqual "אײדער"
    YiddishTextSimpifier.simplify("ברויט") shouldEqual "ברױט"
    YiddishTextSimpifier.simplify("ווען") shouldEqual "װען"
    YiddishTextSimpifier.simplify("כִּמְעַט") shouldEqual "כּמעט"
    YiddishTextSimpifier.simplify("אֶרֶץ-יִשְׂרָאֵל") shouldEqual "ארץ־יִשׂראל"
    YiddishTextSimpifier.simplify("Hello") shouldEqual "LLLLL"
    YiddishTextSimpifier.simplify("Привет") shouldEqual "CCCCCC"
  }
}
