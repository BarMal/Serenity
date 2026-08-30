package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.AppState
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.DefaultThemes
import com.serenity.ui.tui.TerminalRenderSurface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** End-to-end confirmation that the built-in "Transparent" theme actually reaches the terminal as SGR 49 (#1240),
  * through the real production path -- `Renderer.render` painting a `TerminalRenderSurface` -- rather than only at the
  * `TerminalAnsiDiff.sgr` unit level.
  */
class TransparentThemeTuiIntegrationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Renderer" should "emit SGR 49 for the editor body when the Transparent theme is active" in {
    // Chrome rows (the buffer tab bar, status bar) intentionally keep their own opaque `panel`/`menuItem`
    // backgrounds -- see `ThemeConfig.transparent`'s doc comment -- so this only asserts that SGR 49 (the
    // alpha-0 sentinel) appears somewhere in the output, not that every cell avoids an explicit truecolor fill.
    val writer  = new java.io.StringWriter()
    val metrics = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 13)
    val surface = new TerminalRenderSurface(20, 5, writer, metrics)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(theme = DefaultThemes.transparent)
    )

    Renderer.render(state, cursorVisible = false, surface, ViewportSize(20, 5))
    surface.flush()

    writer.toString should include(";49m")
  }

  it should "still emit an explicit truecolor background fill for an ordinary opaque theme" in {
    val writer  = new java.io.StringWriter()
    val metrics = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 13)
    val surface = new TerminalRenderSurface(20, 5, writer, metrics)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(theme = DefaultThemes.defaultDark)
    )

    Renderer.render(state, cursorVisible = false, surface, ViewportSize(20, 5))
    surface.flush()

    val output = writer.toString
    output should include("48;2;")
    output should not include ";49m"
  }
end TransparentThemeTuiIntegrationSpec
