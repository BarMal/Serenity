package com.serenity.ui.tui

import java.awt.Font
import java.io.StringWriter

import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.{AppState, Notice, NoticeLevel}
import com.serenity.state.reducers.{FallbackHeaderReducer, NoticeReducer}
import com.serenity.ui.layout.{CellMetrics, FallbackHeaderText, ViewportSize}
import com.serenity.ui.renderer.{FontSpec, RendererEntryPoints}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The fallback header paints on the terminal's top row over whatever is there, and takes nothing else with it. */
class TerminalFallbackHeaderSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val message = "Skia could not load: no raster surface. Running in the terminal."

  private def stateFor(viewport: ViewportSize): AppState =
    val base = AppState.initial
    NoticeReducer.shown(
      base.copy(runtime = base.runtime.copy(viewportSize = Some(viewport))),
      Notice(NoticeLevel.Error, "Couldn't save notes.md: permission denied."),
      0L
    )

  private def render(state: AppState, viewport: ViewportSize): TerminalEmulator =
    val writer  = new StringWriter()
    val surface = new TerminalRenderSurface(viewport.width, viewport.height, writer, CellMetrics.cellUnit)
    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      FontSpec.fromAwt(font),
      FontSpec.fromAwt(font),
      CellMetrics.cellUnit,
      None,
      RenderCaches.create()
    )
    surface.flush()
    TerminalEmulator.blank(viewport.width, viewport.height).consume(writer.toString)

  "The fallback header" should "paint its message and the hint on the first row" in {
    val viewport = ViewportSize(100, 24)
    val screen   = render(FallbackHeaderReducer.shown(stateFor(viewport), message), viewport)

    screen.rowText(0).trim shouldBe FallbackHeaderText.fit(message, FallbackHeaderText.Hint, viewport.width)
    screen.rowText(0) should include(message)
    screen.rowText(0) should include(FallbackHeaderText.Hint)
  }

  it should "cut the message with an ellipsis at a narrow width, keeping the hint" in {
    for width <- List(20, 40, 60) do
      val viewport = ViewportSize(width, 24)
      val screen   = render(FallbackHeaderReducer.shown(stateFor(viewport), message), viewport)

      withClue(s"width $width: ") {
        screen.rowText(0).trim shouldBe FallbackHeaderText.fit(message, FallbackHeaderText.Hint, width).trim
        screen.rowText(0) should include("…")
      }
  }

  it should "leave every other row as it was" in {
    val viewport = ViewportSize(100, 24)
    val before   = stateFor(viewport)

    val without = render(before, viewport)
    val withIt  = render(FallbackHeaderReducer.shown(before, message), viewport)

    withIt.rows.drop(1) shouldBe without.rows.drop(1)
    withIt.rowText(0) should not be without.rowText(0)
  }

  it should "give back the frame it covered once it is dismissed" in {
    val viewport = ViewportSize(100, 24)
    val before   = stateFor(viewport)

    val dismissed = FallbackHeaderReducer.dismissed(FallbackHeaderReducer.shown(before, message))

    render(dismissed, viewport).rows shouldBe render(before, viewport).rows
  }

end TerminalFallbackHeaderSpec
