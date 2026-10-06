package com.serenity.manuscript

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class ManuscriptMetaSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks:

  private val roundings = Table(
    ("count", "rounding", "described"),
    (86412, WordCountRounding.Novel, "about 86,000 words"),
    (86500, WordCountRounding.Novel, "about 87,000 words"),
    (4260, WordCountRounding.ShortFiction, "about 4,300 words"),
    (40, WordCountRounding.ShortFiction, "about 100 words"),
    (0, WordCountRounding.Novel, "about 0 words"),
    (86412, WordCountRounding.Exact, "86,412 words")
  )

  "WordCountRounding" should "round to its unit for the title page" in
    forAll(roundings)((count, rounding, described) => rounding.describe(count) shouldBe described)

  "AuthorName.fromLegal" should "take the last word of the legal name as the surname" in {
    AuthorName.fromLegal("  Jane Q. Writer ") shouldBe AuthorName("Jane Q. Writer", "Writer")
    AuthorName.fromLegal("") shouldBe AuthorName("", "")
  }
