package com.serenity.manuscript.layout

import com.serenity.manuscript.typography.{FaceStyle, FontFamily, FontSpec}
import com.serenity.richtext.RichTextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The filler's rules in isolation, on pages of exactly ten 10-point lines. */
class PageFillerSpec extends AnyFlatSpec with Matchers:

  private val geometry = Geometry(textTop = 0f, textHeight = 100f, descent = 0f, widows = 2, orphans = 2)
  private val font     = FontSpec(FontFamily.CourierPrime, FaceStyle.Regular, 10f)

  private def line(marker: Char, spaceBefore: Float = 0f): SetLine =
    SetLine(Vector(PlacedRun(marker.toString, font, 0f, RichTextStyle.empty)), spaceBefore, 10f)

  private def lines(marker: Char, count: Int): Vector[SetLine] = Vector.fill(count)(line(marker))

  private def render(pages: Vector[FilledPage]): Vector[String] =
    pages.map(_.lines.map(_.text).mkString)

  "keeping a heading with its next line" should "carry a heading to the next page when no text fits beside it" in {
    val heading = Group(Opening.Continue, line('h') +: lines('b', 3), held = 1)

    render(PageFiller.fill(Vector(Group(Opening.Continue, lines('a', 9), 0), heading), geometry)) shouldBe
      Vector("a" * 9, "hbbb")
  }

  it should "keep a heading with orphans-many lines of its text, not just one" in {
    val heading = Group(Opening.Continue, line('h') +: lines('b', 5), held = 1)

    render(PageFiller.fill(Vector(Group(Opening.Continue, lines('a', 8), 0), heading), geometry)) shouldBe
      Vector("a" * 8, "hbbbbb")
  }

  it should "leave a heading where it is when its text fits beside it" in {
    val heading = Group(Opening.Continue, line('h') +: lines('b', 3), held = 1)

    render(PageFiller.fill(Vector(Group(Opening.Continue, lines('a', 5), 0), heading), geometry)) shouldBe
      Vector("a" * 5 + "hbbb")
  }

  "a new-page opening" should "finish the current page, apply its drop and number nothing itself" in {
    val groups = Vector(
      Group(Opening.Continue, lines('a', 2), 0),
      Group(Opening.NewPage(PageKind.SectionStart, 30f), lines('b', 2), 0)
    )

    val pages = PageFiller.fill(groups, geometry)

    pages.map(_.kind) shouldBe Vector(PageKind.Body, PageKind.SectionStart)
    pages(1).lines.map(_.baselineY) shouldBe Vector(40f, 50f)
  }

  it should "reuse the empty first page instead of leaving a blank one" in {
    val pages = PageFiller.fill(Vector(Group(Opening.NewPage(PageKind.Title, 0f), lines('a', 1), 0)), geometry)

    pages.map(_.kind) shouldBe Vector(PageKind.Title)
  }

  it should "give up the drop rather than leave its held lines alone on the page" in {
    val group = Group(Opening.NewPage(PageKind.SectionStart, 80f), line('h') +: lines('b', 3), held = 1)

    val pages = PageFiller.fill(Vector(group), geometry)

    render(pages) shouldBe Vector("hbbb")
    pages.flatMap(_.lines.headOption).map(_.baselineY) shouldBe Vector(10f)
  }

  "space before a line" should "be dropped at the top of a page and kept below other lines" in {
    val group = Group(Opening.Continue, Vector(line('a', 5f), line('b', 5f)), 0)

    PageFiller.fill(Vector(group), geometry).flatMap(_.lines).map(_.baselineY) shouldBe Vector(10f, 25f)
  }

  "a group with nothing but held lines" should "move to the next page whole when it does not fit" in {
    val held = Group(Opening.Continue, lines('s', 2), held = 2)

    render(PageFiller.fill(Vector(Group(Opening.Continue, lines('a', 9), 0), held), geometry)) shouldBe
      Vector("a" * 9, "ss")
  }
