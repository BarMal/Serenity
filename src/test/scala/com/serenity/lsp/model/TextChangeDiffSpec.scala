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
