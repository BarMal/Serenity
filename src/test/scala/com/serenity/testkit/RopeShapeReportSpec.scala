package com.serenity.testkit

import com.serenity.rope.{Balance, RopeBuilder}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RopeShapeReportSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def unit(code: Int): String = code.toChar.toString

  "A rope's report" should "give each leaf's length and text, in order" in {
    val report = RopeShapeReport.of(RopeBuilder.fromLeaves(List("ab", "cde")))

    report should include("2 leaves")
    report should include("[2 \"ab\"] [3 \"cde\"]")
  }

  it should "write a lone surrogate and other non-ASCII units as \\uXXXX, so a log shows what the leaf holds" in {
    val report = RopeShapeReport.of(RopeBuilder.fromLeaves(List("a" + unit(0xd83d), unit(0xdc4d) + "b " + unit(0xe9))))

    report should include("[2 \"a\\uD83D\"]")
    report should include("[4 \"\\uDC4Db \\u00E9\"]")
  }

  it should "write a quote, a backslash and a line break unambiguously" in {
    val report = RopeShapeReport.of(RopeBuilder.fromLeaves(List("\"\\\n")))

    report should include("[3 \"\\u0022\\u005C\\u000A\"]")
  }

  "The report of an empty rope" should "say there are no leaves" in {
    RopeShapeReport.of(RopeBuilder.fromLeaves(Nil)) should include("0 leaves")
  }
