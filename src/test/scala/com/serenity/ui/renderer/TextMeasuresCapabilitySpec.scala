package com.serenity.ui.renderer

import java.io.StringWriter

import com.serenity.MockRenderSurface
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.tui.TerminalRenderSurface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Whether a surface draws measured text (a pixel surface) or only cells (a terminal) is a capability it states, not a
  * font render context the callers test for presence: the renderer takes the cell grid on a surface that measures
  * nothing.
  */
class TextMeasuresCapabilitySpec extends AnyFlatSpec with Matchers:

  "A terminal surface" should "state that it measures no text" in {
    val surface =
      new TerminalRenderSurface(10, 5, new StringWriter(), CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0))

    surface.text.measuresText shouldBe false
  }

  "A pixel surface" should "state that it measures text, and a mock of a terminal that it does not" in {
    new MockRenderSurface(40, 4).text.measuresText shouldBe true
    new MockRenderSurface(40, 4, measuresText = false).text.measuresText shouldBe false
  }
