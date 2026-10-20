package com.serenity.ui.renderer

import com.serenity.MockRenderSurface
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TabWidthRenderingSpec extends AnyFlatSpec with Matchers:

  private def painted(draw: MockRenderSurface => Unit): List[(Int, String)] =
    val surface = new MockRenderSurface(80, 24)
    draw(surface)
    surface.putStringCalls.map(call => call.x -> call.s)

  "A plain string" should "expand a tab to the next stop of the tab width it is given" in {
    painted(CharacterRenderer.renderStringPlain(_, 0, 0, "a\tb", tabWidth = 8)) shouldBe
      List(0 -> "a", 1 -> "       ", 8 -> "b")
    painted(CharacterRenderer.renderStringPlain(_, 0, 0, "a\tb", tabWidth = 2)) shouldBe
      List(0 -> "a", 1 -> " ", 2 -> "b")
  }

  it should "keep the four-wide default" in {
    painted(CharacterRenderer.renderStringPlain(_, 0, 0, "a\tb")) shouldBe List(0 -> "a", 1 -> "   ", 4 -> "b")
  }

  it should "count its tab stops from where the text starts, not from the screen's left edge" in {
    painted(CharacterRenderer.renderStringPlain(_, 3, 0, "a\tb", tabWidth = 4)) shouldBe
      List(3 -> "a", 4 -> "   ", 7 -> "b")
  }

  "A themed string" should "expand a tab at the given tab width" in {
    painted(CharacterRenderer.renderThemedPlain(_, 0, 0, "a\tb", Theme.default, tabWidth = 8)) shouldBe
      List(0 -> "a", 1 -> "       ", 8 -> "b")
  }

  "A styled string" should "expand a tab at the given tab width, from the start of the line across its segments" in {
    painted(
      CharacterRenderer.renderStyledString(
        _,
        2,
        0,
        "ab\tc",
        Theme.default,
        syntaxHighlightingEnabled = false,
        tabWidth = 8
      )
    ) shouldBe List(2 -> "ab", 4 -> "      ", 10 -> "c")
  }
