package com.serenity

import java.awt.{Color, Font}

import com.serenity.state.models.{TextCaretStop, TextVisualLine}
import com.serenity.ui.layout.TextLayoutSnapshot
import com.serenity.ui.renderer.CharacterRenderer
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CharacterRendererProportionalSpec extends AnyFlatSpec with Matchers:

  private def makeVisualLine(): TextVisualLine =
    TextVisualLine(
      bufferLine = 0,
      startColumn = 0,
      endColumn = 3,
      text = "abc",
      widthPx = 19.5f,
      caretStops = Vector(
        TextCaretStop(0, 0.0f),
        TextCaretStop(1, 7.5f),
        TextCaretStop(2, 13.0f),
        TextCaretStop(3, 19.5f)
      )
    )

  "CharacterRenderer.renderMeasuredLine" should
    "produce a single drawRunPx call when all chars share the theme color" in {
      val surface = new MockRenderSurface(200, 24)
      CharacterRenderer.renderMeasuredLine(
        surface,
        xOriginPx = 0.0f,
        yPx = 0,
        lineHeightPx = 14,
        ascentPx = 10,
        makeVisualLine(),
        Theme.light
      )
      val calls = surface.drawRunPxCalls
      calls should have size 1
      calls.head.xPx shouldBe 0.0f +- 0.001f
      calls.head.bgWidthPx shouldBe 19.5f +- 0.001f
      calls.head.s shouldBe "abc"
    }

  it should "shift all xPx values by xOriginPx" in {
    val surface = new MockRenderSurface(200, 24)
    CharacterRenderer.renderMeasuredLine(
      surface,
      xOriginPx = 8.0f,
      yPx = 0,
      lineHeightPx = 14,
      ascentPx = 10,
      makeVisualLine(),
      Theme.light
    )
    surface.drawRunPxCalls.head.xPx shouldBe 8.0f +- 0.001f
  }

  it should "clip a measured run to an explicit pixel-right boundary" in {
    val surface = new MockRenderSurface(200, 24)

    CharacterRenderer.renderMeasuredLine(
      surface,
      xOriginPx = 0.0f,
      yPx = 0,
      lineHeightPx = 14,
      ascentPx = 10,
      makeVisualLine(),
      Theme.light,
      clipRightXPx = Some(15.0f)
    )

    val calls = surface.drawRunPxCalls
    calls should have size 1
    calls.head.xPx shouldBe 0.0f +- 0.001f
    calls.head.bgWidthPx shouldBe 15.0f +- 0.001f
  }

  it should "render a long measured line as one run " in {
    val text = "Wi" * 2_000
    val visualLine = TextVisualLine(
      bufferLine = 0,
      startColumn = 0,
      endColumn = text.length,
      text = text,
      widthPx = text.length * 6.0f,
      caretStops = Vector.tabulate(text.length + 1)(index => TextCaretStop(index, index * 6.0f))
    )
    val surface = new MockRenderSurface(20_000, 24)

    CharacterRenderer.renderMeasuredLine(
      surface,
      xOriginPx = 0.0f,
      yPx = 0,
      lineHeightPx = 14,
      ascentPx = 10,
      visualLine,
      Theme.light
    )

    surface.drawRunPxCalls.map(_.s).mkString shouldBe text
  }

  it should "render an RTL measured line whose logical endpoints share the right edge" in {
    val text       = "אבג"
    val font       = Font("SansSerif", Font.PLAIN, 12)
    val visualLine = TextLayoutSnapshot.visualLineForText(text, bufferLine = 0, font)
    val surface    = new MockRenderSurface(200, 24)

    visualLine.caretStops.head.xPx shouldBe visualLine.caretStops.last.xPx +- 0.001f

    CharacterRenderer.renderMeasuredLine(
      surface,
      xOriginPx = 0.0f,
      yPx = 0,
      lineHeightPx = 14,
      ascentPx = 10,
      visualLine,
      Theme.light
    )

    val calls = surface.drawRunPxCalls
    calls should have size 1
    calls.head.s shouldBe text
    calls.head.xPx shouldBe visualLine.caretStops.map(_.xPx).min +- 0.001f
    calls.head.bgWidthPx shouldBe (visualLine.caretStops.map(_.xPx).max - visualLine.caretStops
      .map(_.xPx)
      .min) +- 0.001f
  }

  it should "measure the same text correctly against two different sets of caret stops" in {
    val text = "abc"
    val narrowLine = TextVisualLine(
      bufferLine = 0,
      startColumn = 0,
      endColumn = 3,
      text = text,
      widthPx = 15.0f,
      caretStops =
        Vector(TextCaretStop(0, 0.0f), TextCaretStop(1, 5.0f), TextCaretStop(2, 10.0f), TextCaretStop(3, 15.0f))
    )
    val wideLine = TextVisualLine(
      bufferLine = 0,
      startColumn = 0,
      endColumn = 3,
      text = text,
      widthPx = 30.0f,
      caretStops =
        Vector(TextCaretStop(0, 0.0f), TextCaretStop(1, 10.0f), TextCaretStop(2, 20.0f), TextCaretStop(3, 30.0f))
    )

    val narrowSurface = new MockRenderSurface(200, 24)
    CharacterRenderer.renderMeasuredLine(
      narrowSurface,
      xOriginPx = 0.0f,
      yPx = 0,
      lineHeightPx = 14,
      ascentPx = 10,
      narrowLine,
      Theme.light
    )
    narrowSurface.drawRunPxCalls.head.bgWidthPx shouldBe 15.0f +- 0.001f

    val wideSurface = new MockRenderSurface(200, 24)
    CharacterRenderer.renderMeasuredLine(
      wideSurface,
      xOriginPx = 0.0f,
      yPx = 0,
      lineHeightPx = 14,
      ascentPx = 10,
      wideLine,
      Theme.light
    )
    wideSurface.drawRunPxCalls.head.bgWidthPx shouldBe 30.0f +- 0.001f
  }

  it should "keep emoji and combining graphemes at their measured bounds" in {
    val text = "😀e\u0301x"
    val visualLine = TextVisualLine(
      bufferLine = 0,
      startColumn = 0,
      endColumn = text.length,
      text = text,
      widthPx = 30.0f,
      caretStops = Vector(
        TextCaretStop(0, 0.0f),
        TextCaretStop(2, 12.0f),
        TextCaretStop(4, 24.0f),
        TextCaretStop(5, 30.0f)
      )
    )
    val emojiColor  = Color(200, 80, 40)
    val accentColor = Color(40, 120, 210)
    val styled = List(
      StyledText("😀", TextStyle.normal, emojiColor, Theme.light.background),
      StyledText("e\u0301", TextStyle.normal, accentColor, Theme.light.background),
      StyledText("x", TextStyle.normal, Theme.light.foreground, Theme.light.background)
    )
    val surface = new MockRenderSurface(200, 24)

    CharacterRenderer.renderMeasuredLine(
      surface,
      xOriginPx = 0.0f,
      yPx = 0,
      lineHeightPx = 14,
      ascentPx = 10,
      visualLine,
      Theme.light,
      styledSegments = Some(styled)
    )

    val calls = surface.drawRunPxCalls
    calls.map(_.s) shouldBe List("😀", "e\u0301", "x")
    calls.map(_.xPx) shouldBe List(0.0f, 12.0f, 24.0f)
    calls.map(_.bgWidthPx) shouldBe List(12.0f, 12.0f, 6.0f)
    calls.take(2).map(_.foreground) shouldBe List(emojiColor, accentColor)
  }
