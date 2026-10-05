package com.serenity.markdown

import com.serenity.rope.{Balance, Rope}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Each case is written as Markdown with its selections marked: `[` and `]` around a selection, `|` for a caret. */
class MarkdownFormattingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def parse(marked: String): (Rope, List[SourceRange]) =
    val (text, ranges, _) = marked.foldLeft((Vector.empty[Char], Vector.empty[SourceRange], 0)) {
      case ((text, ranges, _), '[')     => (text, ranges, text.length)
      case ((text, ranges, start), ']') => (text, ranges :+ SourceRange(start, text.length), start)
      case ((text, ranges, start), '|') => (text, ranges :+ SourceRange(text.length, text.length), start)
      case ((text, ranges, start), c)   => (text :+ c, ranges, start)
    }
    (Rope(text.mkString), ranges.toList)

  private def render(source: Rope, result: Reformatted): String =
    val text = result.edits.foldRight(source.collect()) { (edit, current) =>
      current.substring(0, edit.start) + edit.text + current.substring(edit.end)
    }
    val marks = result.ranges.flatMap { range =>
      if range.isEmpty then List(range.start -> "|") else List(range.start -> "[", range.end -> "]")
    }
    marks.sortBy((offset, mark) => (-offset, mark == "[")).foldLeft(text) {
      case (current, (offset, mark)) =>
        current.substring(0, offset) + mark + current.substring(offset)
    }

  private def toggled(marked: String, emphasis: Emphasis): String =
    val (source, ranges) = parse(marked)
    render(source, MarkdownFormatting.toggle(source, ranges, emphasis))

  private def headed(marked: String, level: Int): String =
    val (source, ranges) = parse(marked)
    render(source, MarkdownFormatting.setHeading(source, ranges, level))

  "Toggling emphasis" should "wrap a selection, keeping the words selected" in {
    toggled("say [hello] there", Emphasis.Bold) shouldBe "say **[hello]** there"
    toggled("say [hello] there", Emphasis.Italic) shouldBe "say *[hello]* there"
    toggled("say [hello] there", Emphasis.Underline) shouldBe "say <u>[hello]</u> there"
  }

  it should "unwrap a selection inside its markers, or one that takes the markers in" in {
    toggled("say **[hello]** there", Emphasis.Bold) shouldBe "say [hello] there"
    toggled("say [**hello**] there", Emphasis.Bold) shouldBe "say [hello] there"
    toggled("say <u>[hello]</u> there", Emphasis.Underline) shouldBe "say [hello] there"
  }

  it should "tell bold and italic apart in a run of stars" in {
    toggled("**[bold]**", Emphasis.Italic) shouldBe "***[bold]***"
    toggled("***[both]***", Emphasis.Italic) shouldBe "**[both]**"
    toggled("***[both]***", Emphasis.Bold) shouldBe "*[both]*"
    toggled("*[italic]*", Emphasis.Bold) shouldBe "***[italic]***"
  }

  it should "act on the word under a caret, keeping the caret in place" in {
    toggled("say hel|lo there", Emphasis.Bold) shouldBe "say **hel|lo** there"
    toggled("say **hel|lo** there", Emphasis.Bold) shouldBe "say hel|lo there"
  }

  it should "open an empty pair at a caret outside any word, and close it again" in {
    toggled("say | there", Emphasis.Bold) shouldBe "say **|** there"
    toggled("say **|** there", Emphasis.Bold) shouldBe "say | there"
  }

  it should "wrap each line of a multi-line selection on its own, leaving the blank line and indentation alone" in {
    toggled("[one\n\n  two]", Emphasis.Italic) shouldBe "*[one*\n\n  *two]*"
  }

  it should "add to every selection unless all of them already carry it" in {
    toggled("[a] **[b]**", Emphasis.Bold) shouldBe "**[a]** **[b]**"
    toggled("**[a]** **[b]**", Emphasis.Bold) shouldBe "[a] [b]"
  }

  it should "leave a caret inside an inline code span alone rather than writing delimiters into the code" in {
    toggled("call `fo|o` now", Emphasis.Bold) shouldBe "call `fo|o` now"
    toggled("call ``a ` b|c`` now", Emphasis.Italic) shouldBe "call ``a ` b|c`` now"
  }

  it should "leave a selection that starts or ends inside a code span alone" in {
    toggled("call `f[oo` no]w", Emphasis.Bold) shouldBe "call `f[oo` no]w"
  }

  it should "still wrap a selection that takes in a whole code span" in {
    toggled("call [`foo`] now", Emphasis.Bold) shouldBe "call **[`foo`]** now"
  }

  it should "leave a fenced code block alone" in {
    toggled("```\nco|de\n```", Emphasis.Bold) shouldBe "```\nco|de\n```"
  }

  it should "remove underscore emphasis when toggling it off" in {
    toggled("say __str|ong__ now", Emphasis.Bold) shouldBe "say str|ong now"
    toggled("say [__strong__] now", Emphasis.Bold) shouldBe "say [strong] now"
    toggled("say _e|m_ now", Emphasis.Italic) shouldBe "say e|m now"
  }

  it should "find emphasis through nested delimiters of the other kind" in {
    toggled("**_[word]_**", Emphasis.Bold) shouldBe "_[word]_"
    toggled("**_[word]_**", Emphasis.Italic) shouldBe "**[word]**"
  }

  it should "not treat underscores inside a word as emphasis" in {
    toggled("snake_[case]_name", Emphasis.Italic) shouldBe "snake_*[case]*_name"
  }

  it should "not treat an escaped star as a delimiter" in {
    toggled("\\*wo|rd\\*", Emphasis.Italic) shouldBe "\\**wo|rd*\\*"
  }

  it should "wrap the lines of one paragraph as a single span, and remove it the same way" in {
    toggled("[one\ntwo]", Emphasis.Bold) shouldBe "**[one\ntwo]**"
    toggled("**[one\ntwo]**", Emphasis.Bold) shouldBe "[one\ntwo]"
  }

  "The active emphasis" should "be what every selection carries" in {
    val (source, ranges) = parse("***[a]*** **[b]**")
    MarkdownFormatting.emphasisAt(source, ranges) shouldBe Set(Emphasis.Bold)
    val (caretSource, caret) = parse("<u>wo|rd</u>")
    MarkdownFormatting.emphasisAt(caretSource, caret) shouldBe Set(Emphasis.Underline)
  }

  "Setting a heading level" should "add, replace or remove the prefix of the caret's line" in {
    headed("Ti|tle\nbody", 2) shouldBe "## Ti|tle\nbody"
    headed("### Ti|tle", 1) shouldBe "# Ti|tle"
    headed("## Ti|tle", 0) shouldBe "Ti|tle"
    headed("|Title", 1) shouldBe "# |Title"
  }

  it should "change every non-blank line a selection spans" in {
    headed("[one\n\ntwo]\nthree", 3) shouldBe "### [one\n\n### two]\nthree"
  }

  it should "report the level of the caret's line" in {
    val (source, caret) = parse("intro\n#### dee|p")
    MarkdownFormatting.headingLevelAt(source, caret) shouldBe 4
    MarkdownFormatting.headingLevelAt(parse("int|ro")._1, parse("int|ro")._2) shouldBe 0
  }
