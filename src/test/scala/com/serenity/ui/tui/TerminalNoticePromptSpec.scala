package com.serenity.ui.tui

import java.awt.Font
import java.io.StringWriter

import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.{AppState, Notice, NoticeLevel, NoticePrompt, NoticePromptId}
import com.serenity.state.reducers.NoticeReducer
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{FontSpec, RendererEntryPoints}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A notice that asks a question (#1847) paints its actions one to a row, marks the highlighted one, and names the keys
  * that answer it.
  */
class TerminalNoticePromptSpec extends AnyFlatSpec with Matchers:

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

  private val promptId = NoticePromptId(1)

  private def asked(highlighted: Option[Int]): AppState =
    val notice = Notice(
      NoticeLevel.Info,
      "Scala language server: New build detected.",
      prompt = Some(NoticePrompt(promptId, List("Import build", "Not now"), highlighted))
    )
    val base = AppState.initial
    NoticeReducer.shown(base.copy(runtime = base.runtime.copy(viewportSize = Some(viewport))), notice, 0L)

  "A question notice in the TUI" should "paint its message, one row per action, and the keys that answer it" in {
    val screen = render(asked(None))

    val (_, messageRow) = screen.find("New build detected.").getOrElse(fail(screen.render))
    screen.find("Import build").map(_._2) shouldBe Some(messageRow + 1)
    screen.find("Not now").map(_._2) shouldBe Some(messageRow + 2)
    screen.find("left/right choose, enter confirm, esc dismiss").map(_._2) shouldBe Some(messageRow + 3)
    screen.find("Info").map(_._2) shouldBe Some(messageRow - 1)
  }

  it should "mark the highlighted action" in {
    val screen = render(asked(Some(1)))

    screen.find("> Not now") should not be empty
    screen.find("> Import build") shouldBe empty
    screen.find("Import build") should not be empty
  }
