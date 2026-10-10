package com.serenity.ui.renderer

import com.serenity.perf.SettledAllocation
import com.serenity.state.models.{TextCaretStop, TextVisualLine}
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MeasuredLineRunsAllocationSpec extends AnyFlatSpec with Matchers:

  private val theme = Theme.light

  private val text  = "the quick brown fox jumps over the lazy dog " * 14
  private val spans = CharacterRenderer.computeGraphemeSpans(text)

  private val line = TextVisualLine(
    bufferLine = 0,
    startColumn = 0,
    endColumn = text.length,
    text = text,
    widthPx = text.length * 8.0f,
    caretStops = Vector.tabulate(text.length + 1)(column => TextCaretStop(column, column * 8.0f))
  )

  private val segments = List(StyledText(text, TextStyle.normal, theme.foreground, theme.background))

  private def runsOf() = MeasuredLineRuns.of(line, segments, spans, theme)

  "MeasuredLineRuns.of" should "not allocate attribute objects for every grapheme of a line" in {
    runsOf().map(_.text).mkString shouldBe text
    SettledAllocation.perCall(() => runsOf(), () => runsOf()) match
      case Some((bytes, _)) =>
        withClue(s"$bytes bytes for ${spans.length} graphemes: ")(bytes.toDouble / spans.length should be < 40.0)
      case None => info("per-thread allocation counter unsupported -- skipping")
  }
