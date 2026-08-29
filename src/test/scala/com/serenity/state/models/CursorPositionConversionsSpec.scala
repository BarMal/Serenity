package com.serenity.state.models

import com.serenity.rope.{Balance, Rope}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers `Rope.offsetToCursorPosition`, the single canonical rope-offset -> `CursorPosition` conversion shared by
  * every call site that used to reimplement it (event reducers, effect handlers, and the workflow capability's
  * string-backed replace path, which now routes through a `Rope` instead of iterating characters by hand).
  */
class CursorPositionConversionsSpec extends AnyFlatSpec with Matchers:

  given balance: Balance = Balance.default

  "offsetToCursorPosition" should "resolve offset 0 to line 0, column 0" in {
    Rope("hello\nworld").offsetToCursorPosition(0) shouldBe CursorPosition(0, 0)
  }

  it should "resolve an offset within the first line" in {
    Rope("hello\nworld").offsetToCursorPosition(3) shouldBe CursorPosition(0, 3)
  }

  it should "resolve an offset exactly at a line boundary to the start of the next line" in {
    val rope          = Rope("hello\nworld")
    val newlineOffset = rope.lineColumnToOffset(0, 5)
    rope.offsetToCursorPosition(newlineOffset) shouldBe CursorPosition(0, 5) // the newline itself is still on line 0
    rope.offsetToCursorPosition(newlineOffset + 1) shouldBe CursorPosition(1, 0)
  }

  it should "resolve an offset on an empty line between two newlines" in {
    val rope = Rope("a\n\nb")
    rope.offsetToCursorPosition(2) shouldBe CursorPosition(1, 0)
  }

  it should "resolve the end-of-document offset" in {
    val rope = Rope("hello\nworld")
    rope.offsetToCursorPosition(rope.weight) shouldBe CursorPosition(1, 5)
  }

  it should "clamp an out-of-range offset to the end of the document" in {
    val rope = Rope("hello")
    rope.offsetToCursorPosition(999) shouldBe CursorPosition(0, 5)
    rope.offsetToCursorPosition(-5) shouldBe CursorPosition(0, 0)
  }

  it should "resolve offset 0 on an empty document" in {
    Rope.empty.offsetToCursorPosition(0) shouldBe CursorPosition(0, 0)
  }

  it should "agree with Rope.offsetToLineColumn for every offset in a multi-line document" in {
    val text = "alpha\nbeta\n\ngamma\ndelta"
    val rope = Rope(text)
    (0 to rope.weight).foreach { offset =>
      val (line, column) = rope.offsetToLineColumn(offset)
      rope.offsetToCursorPosition(offset) shouldBe CursorPosition(line, column)
    }
  }
