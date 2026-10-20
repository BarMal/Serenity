package com.serenity.ui.renderer

import com.serenity.state.models.{CompositionClause, TextVisualLine}
import com.serenity.state.reducers.CompositionPreview.Span
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The pre-edit's look (#2008): underlined, with the clause being converted on the selection background. */
class CompositionStylingSpec extends AnyFlatSpec with Matchers:

  private val theme = Theme.light

  private def lineOf(text: String, bufferLine: Int = 0, startColumn: Int = 0): TextVisualLine =
    TextVisualLine(bufferLine, startColumn, startColumn + text.length, text, 0.0f, Vector.empty)

  private def plain(text: String): List[StyledText] =
    List(StyledText(text, TextStyle.normal, theme.foreground, theme.background))

  private val underlined = TextStyle(isUnderlined = true)

  "CompositionStyling.overlay" should "leave a line the pre-edit is not on exactly as it was" in {
    val segments = plain("heかんllo")

    CompositionStyling.overlay(
      segments,
      lineOf("heかんllo", bufferLine = 3),
      Span(0, 2, 4, List(CompositionClause(0, 2, selected = true))),
      theme
    ) shouldBe theSameInstanceAs(segments)
  }

  it should "underline the pre-edit and put the selected clause on the selection background" in {
    val styled = CompositionStyling.overlay(
      plain("heかんllo"),
      lineOf("heかんllo"),
      Span(0, 2, 4, List(CompositionClause(0, 2, selected = true))),
      theme
    )

    styled shouldBe List(
      StyledText("he", TextStyle.normal, theme.foreground, theme.background),
      StyledText("かん", underlined, theme.selection.foreground, theme.selection.background),
      StyledText("llo", TextStyle.normal, theme.foreground, theme.background)
    )
  }

  it should "underline a clause that is not selected without changing its colours" in {
    val styled = CompositionStyling.overlay(
      plain("heかんllo"),
      lineOf("heかんllo"),
      Span(0, 2, 4, List(CompositionClause(0, 1, selected = false), CompositionClause(1, 2, selected = true))),
      theme
    )

    styled shouldBe List(
      StyledText("he", TextStyle.normal, theme.foreground, theme.background),
      StyledText("か", underlined, theme.foreground, theme.background),
      StyledText("ん", underlined, theme.selection.foreground, theme.selection.background),
      StyledText("llo", TextStyle.normal, theme.foreground, theme.background)
    )
  }

  it should "keep what the text already looked like, adding the underline to it" in {
    val bold = TextStyle(isBold = true)
    val styled = CompositionStyling.overlay(
      List(StyledText("heかんllo", bold, theme.foreground, theme.background)),
      lineOf("heかんllo"),
      Span(0, 2, 4, List(CompositionClause(0, 2, selected = false))),
      theme
    )

    styled.map(_.style) shouldBe List(bold, TextStyle(isBold = true, isUnderlined = true), bold)
  }

  it should "cut across segments the highlighter had already split" in {
    val keyword = theme.foreground
    val styled = CompositionStyling.overlay(
      List(
        StyledText("he", TextStyle.normal, keyword, theme.background),
        StyledText("かんllo", TextStyle.normal, theme.muted, theme.background)
      ),
      lineOf("heかんllo"),
      Span(0, 3, 5, List(CompositionClause(0, 2, selected = false))),
      theme
    )

    styled.map(_.content) shouldBe List("he", "か", "んl", "lo")
    styled.map(_.style.isUnderlined) shouldBe List(false, false, true, false)
  }

  it should "style only the part of the pre-edit on a wrapped row, by the row's own columns" in {
    val styled = CompositionStyling.overlay(
      plain("んllo"),
      lineOf("んllo", startColumn = 3),
      Span(0, 2, 4, List(CompositionClause(0, 2, selected = true))),
      theme
    )

    styled.map(_.content) shouldBe List("ん", "llo")
    styled.head.style.isUnderlined shouldBe true
  }

  it should "still spell exactly the row's text" in {
    val styled = CompositionStyling.overlay(
      plain("heかんllo"),
      lineOf("heかんllo"),
      Span(0, 2, 4, List(CompositionClause(0, 2, selected = true))),
      theme
    )

    StyledText.spell(styled, "heかんllo") shouldBe true
  }
