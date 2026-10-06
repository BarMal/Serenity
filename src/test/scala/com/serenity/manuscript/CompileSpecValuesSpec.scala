package com.serenity.manuscript

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class CompileSpecValuesSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks:

  private val numerals = Table(
    ("number", "roman"),
    (1, "I"),
    (4, "IV"),
    (9, "IX"),
    (14, "XIV"),
    (40, "XL"),
    (1994, "MCMXCIV"),
    (3999, "MMMCMXCIX"),
    (0, "0"),
    (4000, "4000")
  )

  "HeadingTemplate.roman" should "spell numbers 1 to 3999 in Roman numerals" in
    forAll(numerals)((number, roman) => HeadingTemplate.roman(number) shouldBe roman)

  "HeadingTemplate" should "fill every placeholder and split lines" in {
    HeadingTemplate("Chapter <$n>\n<$t>").render(3, "The Storm") shouldBe
      Some(SectionHeading(Vector("Chapter 3", "The Storm")))
    HeadingTemplate("Part <$R> (<$r>)").render(4, "") shouldBe Some(SectionHeading(Vector("Part IV (iv)")))
  }

  it should "drop lines a missing title leaves empty, and give no heading when nothing is left" in {
    HeadingTemplate("Chapter <$n>\n<$t>").render(2, "") shouldBe Some(SectionHeading(Vector("Chapter 2")))
    HeadingTemplate.SourceTitle.render(2, "") shouldBe None
  }

  "CompileSpec" should "list only included sources, in order" in {
    val spec = CompileSpec
      .forTitle("Book")
      .copy(sources = List(SourceEntry("a.md", true), SourceEntry("notes.md", false), SourceEntry("b.md", true)))

    spec.includedSourcePaths shouldBe List("a.md", "b.md")
  }

  "ManuscriptFormat" should "hold the standard manuscript numbers in its modern preset" in {
    val modern = ManuscriptFormat.Modern

    (modern.fontFamily, modern.fontSizePoints, modern.lineSpacing) shouldBe ("Times New Roman", 12, 480)
    (modern.marginTwips, modern.firstLineIndentTwips, modern.paper) shouldBe (1440, 720, PaperSize.Letter)
    modern.sceneBreak shouldBe "#"
    ManuscriptFormat.Classic.fontFamily shouldBe "Courier New"
  }
