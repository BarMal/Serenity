package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.MockRenderSurface
import com.serenity.ui.theme.TextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DropCapRendererSpec extends AnyFlatSpec with Matchers:

  "DropCapRenderer.renderGlyphPx" should "draw one pixel run spanning the glyph's full multi-line height" in {
    val surface = MockRenderSurface(80, 40)
    val style   = TextStyle(isBold = true, fontSize = Some(36.0f))

    DropCapRenderer.renderGlyphPx(
      surface,
      xOriginPx = 10.0f,
      yTopPx = 5,
      glyphWidthPx = 24.0f,
      glyphHeightPx = 48,
      glyphAscentPx = 36,
      glyphText = "C",
      glyphStyle = style,
      foreground = Color.RED,
      background = Color.BLACK
    )

    surface.drawRunPxCalls should have size 1
    val call = surface.drawRunPxCalls.head
    call.s shouldBe "C"
    call.xPx shouldBe 10.0f
    call.yPx shouldBe 5
    call.bgWidthPx shouldBe 24.0f
    call.lineHeightPx shouldBe 48
    call.ascentPx shouldBe 36
    call.foreground shouldBe Color.RED
  }

  it should "draw nothing for an empty glyph" in {
    val surface = MockRenderSurface(80, 40)

    DropCapRenderer.renderGlyphPx(
      surface,
      xOriginPx = 10.0f,
      yTopPx = 5,
      glyphWidthPx = 24.0f,
      glyphHeightPx = 48,
      glyphAscentPx = 36,
      glyphText = "",
      glyphStyle = TextStyle.normal,
      foreground = Color.RED,
      background = Color.BLACK
    )

    surface.drawRunPxCalls shouldBe Nil
  }

  "DropCapRenderer.renderGlyphCell" should "write the character as a single bold cell" in {
    val surface = MockRenderSurface(80, 40)

    DropCapRenderer.renderGlyphCell(surface, x = 2, y = 1, glyphText = "C", accentForeground = Color.RED, background = Color.BLACK)

    surface.getChar(2, 1) shouldBe 'C'
    surface.styleCalls.map(_.style) should contain(TextStyle(isBold = true))
  }
