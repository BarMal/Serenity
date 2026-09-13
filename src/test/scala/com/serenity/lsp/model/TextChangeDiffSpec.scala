package com.serenity.lsp.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextChangeDiffSpec extends AnyFlatSpec with Matchers:

  "TextChangeDiff.diff" should "describe an append at the end of the document as a zero-length insert" in {
    val change = TextChangeDiff.diff("object Foo", "object Foo2")
    change.range shouldBe LspRange(LspPosition(0, 10), LspPosition(0, 10))
    change.rangeLength shouldBe 0
    change.text shouldBe "2"
  }

  it should "describe a prepend at the start of the document as a zero-length insert at position 0" in {
    val change = TextChangeDiff.diff("Foo", "Xtra Foo")
    change.range shouldBe LspRange(LspPosition(0, 0), LspPosition(0, 0))
    change.rangeLength shouldBe 0
    change.text shouldBe "Xtra "
  }

  it should "describe a mid-document replacement by the smallest span that differs" in {
    val change = TextChangeDiff.diff("object Foo extends Bar", "object Foo extends Baz")
    change.range shouldBe LspRange(LspPosition(0, 21), LspPosition(0, 22))
    change.rangeLength shouldBe 1
    change.text shouldBe "z"
  }

  it should "describe a deletion as a non-empty range replaced with empty text" in {
    val change = TextChangeDiff.diff("object FooBar", "object Foo")
    change.range shouldBe LspRange(LspPosition(0, 10), LspPosition(0, 13))
    change.rangeLength shouldBe 3
    change.text shouldBe ""
  }

  it should "span multiple lines when the edit crosses a newline" in {
    val change = TextChangeDiff.diff("line one\nline two", "line one\nline TWO")
    change.range shouldBe LspRange(LspPosition(1, 5), LspPosition(1, 8))
    change.rangeLength shouldBe 3
    change.text shouldBe "TWO"
  }

  it should "describe identical text as a no-op empty-range replacement" in {
    val change = TextChangeDiff.diff("unchanged", "unchanged")
    change.range shouldBe LspRange(LspPosition(0, 9), LspPosition(0, 9))
    change.rangeLength shouldBe 0
    change.text shouldBe ""
  }

  it should "describe replacing the whole document when nothing is shared" in {
    val change = TextChangeDiff.diff("abc", "xyz")
    change.range shouldBe LspRange(LspPosition(0, 0), LspPosition(0, 3))
    change.rangeLength shouldBe 3
    change.text shouldBe "xyz"
  }

  it should "not split a surrogate pair on the prefix boundary when two astral characters share a high surrogate" in {
    // U+1F600 and U+1F601 both encode as high surrogate \uD83D, with low surrogates \uDE00/\uDE01 --
    // a naive char-by-char prefix scan would match the shared high surrogate and stop one unit later,
    // leaving a lone low surrogate as the "changed" text.
    val change = TextChangeDiff.diff("a😀b", "a😁b")
    change.range shouldBe LspRange(LspPosition(0, 1), LspPosition(0, 3))
    change.rangeLength shouldBe 2
    change.text shouldBe "😁"
  }

  it should "not split a surrogate pair on the suffix boundary when two astral characters share a low surrogate" in {
    // Both pairs end in the low surrogate \uDE00 but start with different high surrogates -- a naive
    // suffix scan would match the shared low surrogate and stop one unit early, leaving a lone low
    // surrogate at the start of the common suffix.
    val change = TextChangeDiff.diff("x😀y", "x🨀y")
    change.range shouldBe LspRange(LspPosition(0, 1), LspPosition(0, 3))
    change.rangeLength shouldBe 2
    change.text shouldBe "🨀"
  }

  it should "describe an insertion immediately before a surrogate pair without touching the pair" in {
    val change = TextChangeDiff.diff("😀", "X😀")
    change.range shouldBe LspRange(LspPosition(0, 0), LspPosition(0, 0))
    change.rangeLength shouldBe 0
    change.text shouldBe "X"
  }

  it should "describe an insertion immediately after a surrogate pair without touching the pair" in {
    val change = TextChangeDiff.diff("😀", "😀X")
    change.range shouldBe LspRange(LspPosition(0, 2), LspPosition(0, 2))
    change.rangeLength shouldBe 0
    change.text shouldBe "X"
  }

  it should "describe a deletion spanning an entire surrogate pair in the middle of the document" in {
    val change = TextChangeDiff.diff("hello😀world", "helloworld")
    change.range shouldBe LspRange(LspPosition(0, 5), LspPosition(0, 7))
    change.rangeLength shouldBe 2
    change.text shouldBe ""
  }
