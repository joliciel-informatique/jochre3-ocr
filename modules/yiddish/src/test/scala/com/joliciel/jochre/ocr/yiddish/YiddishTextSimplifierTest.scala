package com.joliciel.jochre.ocr.yiddish

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class YiddishTextSimplifierTest extends AnyFlatSpec with Matchers {
  "YiddishTextSimplifier" should "simplify yiddish orthography" in {
    YiddishTextSimpifier.simplify("זייַנען") shouldEqual "זײַנען"
  }
}
