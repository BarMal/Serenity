package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** How many random lines the ligature oracle's fuzz test counts (#2447 B2): a tenth of the full run in the default
  * suite, which shares a macOS runner with every other spec, and the full 100,000 when a run asks for it (the scheduled
  * and manual workflow does).
  */
class OracleCorpusSizeSpec extends AnyFlatSpec with Matchers:

  "The fuzz test's size" should "be 10,000 lines when nothing asks otherwise" in {
    OracleCorpusSize.fuzzLines(Map.empty) shouldBe 10000
  }

  it should "be the number a run asks for in SERENITY_FUZZ_LINES" in {
    OracleCorpusSize.fuzzLines(Map("SERENITY_FUZZ_LINES" -> "100000")) shouldBe 100000
    OracleCorpusSize.fuzzLines(Map("SERENITY_FUZZ_LINES" -> " 250 ")) shouldBe 250
  }

  it should "stay at the default when the request is not a positive whole number" in
    List("", "many", "0", "-5", "1.5", "99999999999").foreach { asked =>
      withClue(s"'$asked': ")(OracleCorpusSize.fuzzLines(Map("SERENITY_FUZZ_LINES" -> asked)) shouldBe 10000)
    }

  it should "name the variable the scheduled workflow sets" in {
    OracleCorpusSize.Variable shouldBe "SERENITY_FUZZ_LINES"
  }
