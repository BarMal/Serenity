package com.serenity

import java.awt.Color

import com.serenity.animation.{AnimatedCell, AnimationState, CharacterKey}
import com.serenity.ui.layout.{TextCaretStop, TextVisualLine}
import com.serenity.ui.renderer.CharacterRenderer
import com.serenity.ui.theme.Theme
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

  private def animWithMiddleChar(fg: Color, bg: Color): AnimationState =
    AnimationState(
      Map(
        CharacterKey(1, 0) -> AnimatedCell(
          content = Some('b'),
          foregroundSteps = List(fg),
          backgroundSteps = List(bg)
        )
      )
    )

  "CharacterRenderer.renderMeasuredLineWithAnimation" should
    "produce a single drawRunPx call when all chars share the theme color" in {
      val surface = new MockRenderSurface(200, 24)
      CharacterRenderer.renderMeasuredLineWithAnimation(
        surface,
        xOriginPx = 0.0f,
        yPx = 0,
        lineHeightPx = 14,
        ascentPx = 10,
        makeVisualLine(),
        Theme.light,
        AnimationState.empty
      )
      val calls = surface.drawRunPxCalls
      calls should have size 1
      calls.head.xPx shouldBe 0.0f +- 0.001f
      calls.head.bgWidthPx shouldBe 19.5f +- 0.001f
      calls.head.s shouldBe "abc"
    }

  it should "split into three calls when the middle char has a different animation color" in {
    val surface = new MockRenderSurface(200, 24)
    val color   = Color(200, 100, 50)
    CharacterRenderer.renderMeasuredLineWithAnimation(
      surface,
      xOriginPx = 0.0f,
      yPx = 0,
      lineHeightPx = 14,
      ascentPx = 10,
      makeVisualLine(),
      Theme.light,
      animWithMiddleChar(color, color)
    )
    val calls = surface.drawRunPxCalls
    calls should have size 3
    calls(0).s shouldBe "a"
    calls(0).xPx shouldBe 0.0f +- 0.001f
    calls(0).bgWidthPx shouldBe 7.5f +- 0.001f
    calls(1).s shouldBe "b"
    calls(1).xPx shouldBe 7.5f +- 0.001f
    calls(1).bgWidthPx shouldBe 5.5f +- 0.001f
    calls(2).s shouldBe "c"
    calls(2).xPx shouldBe 13.0f +- 0.001f
    calls(2).bgWidthPx shouldBe 6.5f +- 0.001f
  }

  it should "shift all xPx values by xOriginPx" in {
    val surface = new MockRenderSurface(200, 24)
    CharacterRenderer.renderMeasuredLineWithAnimation(
      surface,
      xOriginPx = 8.0f,
      yPx = 0,
      lineHeightPx = 14,
      ascentPx = 10,
      makeVisualLine(),
      Theme.light,
      AnimationState.empty
    )
    surface.drawRunPxCalls.head.xPx shouldBe 8.0f +- 0.001f
  }
