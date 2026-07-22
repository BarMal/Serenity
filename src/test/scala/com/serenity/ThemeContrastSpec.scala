package com.serenity

import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ThemeContrastSpec extends AnyFlatSpec with Matchers:

  "Bundled themes" should "provide WCAG AA contrast for normal, control, placeholder, selected, and surface text" in {
    List(Theme.dark, Theme.light).foreach { theme =>
      theme.normalTextContrast should be >= 4.5
      theme.controlLabelContrast should be >= 4.5
      theme.placeholderTextContrast should be >= 4.5
      theme.selectionTextContrast should be >= 4.5
      theme.surfaceTextContrast should be >= 4.5
    }
  }

  they should "keep accent, selection, focus, status, placeholder, surface, and active-pane roles distinct" in {
    List(Theme.dark, Theme.light).foreach { theme =>
      theme.accent should not be theme.selection.background
      theme.focus should not be theme.selection.background
      theme.activePane should not be theme.selection.background
      theme.status.error should not be theme.status.warning
      theme.placeholder should not be theme.foreground
      theme.surface shouldBe theme.panel
    }
  }
end ThemeContrastSpec
