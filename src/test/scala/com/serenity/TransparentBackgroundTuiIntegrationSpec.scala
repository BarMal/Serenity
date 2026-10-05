package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.AppState
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.RendererEntryPoints
import com.serenity.ui.theme.{DefaultThemes, Theme}
import com.serenity.ui.tui.TerminalRenderSurface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** End-to-end confirmation that a theme whose background is the alpha-0 sentinel reaches the terminal as SGR 49
  * (#1240), through the real production path -- `RendererEntryPoints.render` painting a `TerminalRenderSurface` --
  * rather than only at the `TerminalAnsiDiff.sgr` unit level.
  */
class TransparentBackgroundTuiIntegrationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val transparentBackground =
    Theme.dark.copy(background = RenderColor.fromRgba(0, 0, 0, 0), margin = RenderColor.fromRgba(0, 0, 0, 0))

  "Renderer" should "emit SGR 49 for the editor body when the theme background is transparent" in {
    // Some chrome (the gutter, `menuItem` rows) keeps its own opaque background, so this only asserts that SGR 49
    // appears somewhere in the output, not that every cell avoids an explicit truecolor fill.
    val writer  = new java.io.StringWriter()
    val metrics = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 13)
    val surface = new TerminalRenderSurface(20, 5, writer, metrics)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(theme = transparentBackground)
    )

    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      ViewportSize(20, 5),
      com.serenity.state.manager.RenderCaches.create()
    )
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

    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      ViewportSize(20, 5),
      com.serenity.state.manager.RenderCaches.create()
    )
    surface.flush()

    val output = writer.toString
    output should include("48;2;")
    output should not include ";49m"
  }
end TransparentBackgroundTuiIntegrationSpec
