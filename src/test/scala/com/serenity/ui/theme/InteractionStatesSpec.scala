package com.serenity.ui.theme

import java.awt.Color

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InteractionStatesSpec extends AnyFlatSpec with Matchers:

  private val themes = List(Theme.dark, Theme.light)

  "Theme.interactionStates" should "derive without any theme file changes, for both dark and light themes" in
    themes.foreach(theme => theme.interactionStates shouldBe InteractionStates.derive(theme.menuItem))

  it should "keep hover, pressed, and disabled visually distinct from the base surface and from each other" in
    themes.foreach { theme =>
      val base   = theme.menuItem
      val states = theme.interactionStates

      states.hover should not be base
      states.pressed should not be base
      states.disabled should not be base
      states.hover should not be states.pressed
      states.hover should not be states.disabled
      states.pressed should not be states.disabled
    }

  it should "leave hover/pressed's foreground, style, and alpha untouched -- only the background shifts" in
    themes.foreach { theme =>
      val base   = theme.menuItem
      val states = theme.interactionStates

      states.hover.foreground shouldBe base.foreground
      states.hover.style shouldBe base.style
      states.hover.alpha shouldBe base.alpha
      states.pressed.foreground shouldBe base.foreground
      states.pressed.style shouldBe base.style
    }

  it should "leave disabled's background untouched -- only the foreground is muted" in
    themes.foreach(theme => theme.interactionStates.disabled.background shouldBe theme.menuItem.background)

  it should "keep disabled at or above the WCAG non-text contrast floor (SC 1.4.11)" in
    themes.foreach { theme =>
      val disabled = theme.interactionStates.disabled
      Theme.contrastRatio(disabled.foreground, disabled.background) should be >=
        InteractionStates.DisabledContrastFloor
    }

  it should "shift pressed further than hover, toward the base's own foreground" in
    themes.foreach { theme =>
      val base   = theme.menuItem
      val states = theme.interactionStates

      val hoverShift =
        Theme.luminance(states.hover.background) - Theme.luminance(base.background)
      val pressedShift =
        Theme.luminance(states.pressed.background) - Theme.luminance(base.background)

      math.abs(pressedShift) should be > math.abs(hoverShift)
    }

  "InteractionStates.derive" should "work directly from any ThemeColor, not just Theme.menuItem" in {
    val base   = ThemeColor(foreground = Color.WHITE, background = Color.BLACK)
    val states = InteractionStates.derive(base)

    states.hover should not be base
    states.pressed should not be base
    states.disabled should not be base
    Theme.contrastRatio(states.disabled.foreground, states.disabled.background) should be >=
      InteractionStates.DisabledContrastFloor
  }
end InteractionStatesSpec
