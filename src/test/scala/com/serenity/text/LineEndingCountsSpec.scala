package com.serenity.text

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1964: what a file's line terminators were before the editor normalised them, and what writing them back does. */
class LineEndingCountsSpec extends AnyFlatSpec with Matchers:

  "Counting terminators" should "count each kind, with CRLF as one terminator rather than a CR and an LF" in {
    LineEndingCounts.of("a\nb\r\nc\rd\r\n") shouldBe LineEndingCounts(lf = 1, crlf = 2, cr = 1)
  }

  it should "find none in text with no line break" in {
    LineEndingCounts.of("single line") shouldBe LineEndingCounts.empty
  }

  "A file" should "be mixed only when more than one kind of terminator occurs" in {
    LineEndingCounts.of("a\r\nb\r\n").isMixed shouldBe false
    LineEndingCounts.of("a\r\nb\n").isMixed shouldBe true
    LineEndingCounts.of("a\rb\n").isMixed shouldBe true
    LineEndingCounts.empty.isMixed shouldBe false
  }

  it should "take the majority terminator as dominant, and Lf on a tie" in {
    LineEndingCounts.of("a\r\nb\r\nc\n").dominant shouldBe LineEnding.Crlf
    LineEndingCounts.of("a\rb\rc\n").dominant shouldBe LineEnding.Cr
    LineEndingCounts.of("a\r\nb\n").dominant shouldBe LineEnding.Lf
    LineEndingCounts.empty.dominant shouldBe LineEnding.Lf
  }

  "A lone-CR file" should "be detected as CR rather than counted as having no line endings" in {
    LineEnding.detect("a\rb\rc") shouldBe LineEnding.Cr
  }

  "Writing a line ending" should "turn normalised content back into that terminator" in {
    LineEnding.Lf.applyTo("a\nb\n") shouldBe "a\nb\n"
    LineEnding.Crlf.applyTo("a\nb\n") shouldBe "a\r\nb\r\n"
    LineEnding.Cr.applyTo("a\nb\n") shouldBe "a\rb\r"
  }

  it should "be named by a config key that reads back to it" in {
    LineEnding.values.toList.map(ending => LineEnding.fromConfigKey(ending.configKey)) shouldBe
      LineEnding.values.toList.map(Some(_))
    LineEnding.Cr.label shouldBe "CR"
  }
