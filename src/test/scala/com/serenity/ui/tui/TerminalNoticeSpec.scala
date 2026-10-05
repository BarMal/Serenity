package com.serenity.ui.tui

import java.awt.Font
import java.io.StringWriter

import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.{AppState, BufferId, Notice, NoticeLevel, NoticeTopic}
import com.serenity.state.reducers.NoticeReducer
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{FontSpec, RendererEntryPoints}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The notice surface (#1717) paints in the terminal too, in the editor's bottom-right corner. */
class TerminalNoticeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val font     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val viewport = ViewportSize(80, 24)

  private def render(state: AppState): TerminalEmulator =
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

  "An error notice in the TUI" should "paint its level, message and keys in the bottom-right corner" in {
    val notice = Notice(
      NoticeLevel.Error,
      "Couldn't save notes.md: permission denied.",
      hint = Some("esc dismiss"),
      topic = Some(NoticeTopic.FileSave(BufferId(0)))
    )
    val base  = AppState.initial
    val state = NoticeReducer.shown(base.copy(runtime = base.runtime.copy(viewportSize = Some(viewport))), notice, 0L)

    val screen = render(state)

    val (column, row) = screen.find("Couldn't save notes.md: permission denied.").getOrElse(fail(screen.render))
    row should be > viewport.height / 2
    column should be > viewport.width / 4
    screen.find("Error").map(_._2) shouldBe Some(row - 1)
    screen.find("esc dismiss").map(_._2) shouldBe Some(row + 1)
  }
