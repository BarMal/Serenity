package com.serenity.state.models

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a document's own text says about how it is indented (#1996). */
class IndentationDetectorSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def detected(text: String, markdown: Boolean = false): IndentationHint =
    IndentationDetector.detect(text.linesIterator, markdown)

  private def spaces(width: Int) = IndentationHint(insertSpaces = Some(true), tabSize = Some(width))

  private val tabs = IndentationHint(insertSpaces = Some(false))

  "A file indented with spaces" should "give the width of one level" in {
    detected("class A:\n    def f():\n        pass\n    def g():\n        pass\n") shouldBe spaces(4)
    detected("a:\n  b:\n    c: 1\n  d: 2\n") shouldBe spaces(2)
    detected("a\n   b\n      c\n   d\n") shouldBe spaces(3)
  }

  it should "prefer 4 over 2 when both steps are equally common" in {
    detected("a\n  b\nc\n    d\ne\n") shouldBe spaces(4)
  }

  it should "read a dedent across several levels as one level" in {
    detected("a\n  b\n    c\n      d\ne\n  f\n") shouldBe spaces(2)
  }

  "A file indented with tabs" should "say tabs and leave the width to the settings" in {
    detected("a\n\tb\n\t\tc\n\td\n") shouldBe tabs
  }

  "A file that is not indented" should "say nothing" in {
    detected("one\ntwo\nthree\n") shouldBe IndentationHint.Empty
    detected("") shouldBe IndentationHint.Empty
  }

  "A file mixing tabs and spaces" should "follow whichever starts more lines" in {
    detected("a\n\tb\n\tc\n\td\n    e\n") shouldBe tabs
    detected("a\n    b\n    c\n    d\n\te\n") shouldBe spaces(4)
  }

  it should "call a tie spaces" in {
    detected("a\n    b\n\tc\n") shouldBe spaces(4)
  }

  it should "not read the step between a tab-indented line and a space-indented one" in {
    detected("a\n\tb\n  c\nd\n  e\n") shouldBe spaces(2)
  }

  "Blank lines" should "be ignored, however much whitespace they hold" in {
    detected("a\n\n        \n  b\n\t\t\n  c\n") shouldBe spaces(2)
    detected("\t\t\n   \n\n") shouldBe IndentationHint.Empty
  }

  "Block comment continuation lines" should "not count as indented lines" in {
    val licensed = "/*\n * Copyright\n * Licence\n * Terms\n */\nobject A {\n\tdef f = 1\n\tdef g = 2\n}\n"

    detected(licensed) shouldBe tabs
  }

  it should "not be mistaken for the code's step" in {
    detected("object A {\n    /**\n     * Doc.\n     */\n    def f = 1\n}\n") shouldBe spaces(4)
  }

  "A Markdown document" should "ignore text lined up under a list item" in {
    val text = "- one\n  continued\n- two\n  continued\n1. three\n   continued\n"

    detected(text, markdown = true) shouldBe IndentationHint.Empty
    detected(text) shouldBe spaces(2)
  }

  it should "read nested list items" in {
    detected("- a\n    - b\n        - c\n- d\n", markdown = true) shouldBe spaces(4)
    detected("- a\n\t- b\n\t\t- c\n", markdown = true) shouldBe tabs
  }

  it should "read indented code outside any list" in {
    detected("Intro\n\n    code\n    more\n\nEnd\n", markdown = true) shouldBe spaces(4)
  }

  "Detecting from a rope" should "use the language to decide how Markdown is read" in {
    val text = Rope("- one\n  continued\n- two\n  continued\n")

    IndentationDetector.of(text, Some(LanguageId.Markdown)) shouldBe IndentationHint.Empty
    IndentationDetector.of(text, None) shouldBe spaces(2)
  }

  it should "look no further than the start of the file" in {
    val text = Rope("plain\n" * IndentationDetector.SampleLines + "\tindented\n" * 50)

    IndentationDetector.of(text, None) shouldBe IndentationHint.Empty
  }
