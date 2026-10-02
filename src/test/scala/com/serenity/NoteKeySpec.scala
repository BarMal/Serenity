package com.serenity

import com.serenity.state.models.HeadingIdentity
import com.serenity.ui.layout.{Location, Symbol, SymbolKind}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A chapter note must find its chapter again however the manuscript changes around it, without storing a position:
  * text typed above the chapter, a chapter inserted before it, or a renumbering must not detach or misfile it.
  */
class NoteKeySpec extends AnyFlatSpec with Matchers:

  private def heading(title: String, line: Int): Symbol = Symbol(title, SymbolKind.Heading, Location(line, 0))

  "HeadingIdentity.normalizedTitle" should "drop a chapter number and its separator" in {
    HeadingIdentity.normalizedTitle("Chapter 3: The Storm") shouldBe "the storm"
    HeadingIdentity.normalizedTitle("chapter 7 - Aftermath") shouldBe "aftermath"
    HeadingIdentity.normalizedTitle("Chapter 2. Begin") shouldBe "begin"
  }

  it should "leave nothing for a heading that is only a chapter number" in {
    HeadingIdentity.normalizedTitle("Chapter 12") shouldBe ""
  }

  it should "ignore case and runs of whitespace" in {
    HeadingIdentity.normalizedTitle("  The   STORM ") shouldBe "the storm"
  }

  it should "leave a word that merely starts with 'chapter' alone" in {
    HeadingIdentity.normalizedTitle("Chapterhouse Dune") shouldBe "chapterhouse dune"
    HeadingIdentity.normalizedTitle("Prologue") shouldBe "prologue"
  }

  "HeadingIdentity.forHeadings" should "number repeated titles in document order" in {
    val headings = List(
      heading("Chapter 1: Storm", 0),
      heading("Interlude", 5),
      heading("Chapter 2: Calm", 9),
      heading("Interlude", 20)
    )

    HeadingIdentity.forHeadings(headings).map(_._1) shouldBe List(
      HeadingIdentity("storm", 0),
      HeadingIdentity("interlude", 0),
      HeadingIdentity("calm", 0),
      HeadingIdentity("interlude", 1)
    )
  }

  it should "skip symbols that are not headings" in {
    val symbols = List(
      heading("Chapter 1: Storm", 0),
      Symbol("Storm", SymbolKind.Section, Location(4, 0)),
      Symbol("Comment: storm", SymbolKind.Comment, Location(6, 0))
    )

    HeadingIdentity.forHeadings(symbols).map(_._2.location.line) shouldBe List(0)
  }

  "HeadingIdentity.resolve" should "find a heading by its title" in {
    val headings = List(heading("Chapter 1: Storm", 0), heading("Chapter 2: Calm", 9))

    HeadingIdentity.resolve(HeadingIdentity("calm", 0), headings).map(_.location.line) shouldBe Some(9)
  }

  it should "still find a chapter after an earlier chapter is inserted and the numbers shift" in {
    val headings = List(
      heading("Chapter 1: New Opening", 0),
      heading("Chapter 2: Storm", 5),
      heading("Chapter 3: Calm", 90)
    )

    HeadingIdentity.resolve(HeadingIdentity("calm", 0), headings).map(_.location.line) shouldBe Some(90)
  }

  it should "pick the nth of repeated titles" in {
    val headings = List(heading("Interlude", 5), heading("Interlude", 20))

    HeadingIdentity.resolve(HeadingIdentity("interlude", 1), headings).map(_.location.line) shouldBe Some(20)
  }

  it should "find nothing once the chapter is deleted or retitled" in {
    val headings = List(heading("Chapter 1: Storm", 0))

    HeadingIdentity.resolve(HeadingIdentity("calm", 0), headings) shouldBe None
    HeadingIdentity.resolve(HeadingIdentity("interlude", 1), headings) shouldBe None
  }

  it should "not match a symbol that is not a heading" in {
    val symbols = List(Symbol("Storm", SymbolKind.Section, Location(4, 0)))

    HeadingIdentity.resolve(HeadingIdentity("storm", 0), symbols) shouldBe None
  }
