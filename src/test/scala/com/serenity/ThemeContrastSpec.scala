package com.serenity

import java.awt.Color

import com.serenity.ui.theme.{TextStyle, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ThemeContrastSpec extends AnyFlatSpec with Matchers:

  "Bundled themes" should "provide WCAG AA contrast for normal, control, placeholder, selected, and surface text" in
    List(Theme.dark, Theme.light).foreach { theme =>
      theme.normalTextContrast should be >= 4.5
      theme.controlLabelContrast should be >= 4.5
      theme.placeholderTextContrast should be >= 4.5
      theme.selectionTextContrast should be >= 4.5
      theme.surfaceTextContrast should be >= 4.5
    }

  they should "keep accent, selection, focus, status, placeholder, surface, and active-pane roles distinct" in
    List(Theme.dark, Theme.light).foreach { theme =>
      theme.accent should not be theme.selection.background
      theme.focus should not be theme.selection.background
      theme.activePane should not be theme.selection.background
      theme.focusStyle shouldBe TextStyle.bold
      theme.status.error should not be theme.status.warning
      theme.placeholder should not be theme.foreground
      theme.surface shouldBe theme.panel
    }

  "Theme.contrastRatio" should "short-circuit to a trivially-passing ratio against a transparent background" in {
    // A background with alpha 0 is the "use the terminal/desktop's own backdrop" sentinel (#1240): its nominal RGB
    // is never actually what ends up on screen, so computing WCAG contrast against it would be a meaningless number
    // compared to an unknown real backdrop. Short-circuit instead of quietly reporting a number that only pretends
    // to mean something.
    val transparentBlack = new Color(0, 0, 0, 0)
    Theme.contrastRatio(Color.WHITE, transparentBlack) shouldBe Double.PositiveInfinity
    Theme.contrastRatio(transparentBlack, Color.WHITE) shouldBe Double.PositiveInfinity
  }

  it should "compute the normal WCAG ratio when both colors are opaque" in {
    Theme.contrastRatio(Color.WHITE, Color.BLACK) shouldBe 21.0 +- 1e-9
  }
end ThemeContrastSpec
