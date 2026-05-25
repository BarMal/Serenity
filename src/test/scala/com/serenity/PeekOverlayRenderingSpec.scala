package com.serenity

import com.googlecode.lanterna.screen.TerminalScreen
import com.googlecode.lanterna.terminal.virtual.DefaultVirtualTerminal
import com.googlecode.lanterna.{TerminalPosition, TerminalSize as LanternaSize}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, LayoutEngine, PeekContent, PeekOverlay, TerminalSize}
import com.serenity.ui.renderer.Renderer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PeekOverlayRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def screen(width: Int, height: Int): TerminalScreen =
    val terminal = new DefaultVirtualTerminal(new LanternaSize(width, height))
    val screen   = new TerminalScreen(terminal)
    screen.startScreen()
    screen

  private def stateWithPeek(text: String): AppState =
    val buffer = Buffer.fromString(
      bufferId,
      List.fill(10)("abcdefghijklmnopqrstuvwxyz").mkString("\n")
    ).copy(cursors = List(CursorPosition(4, 12)))
    val pane   = EditorPane.withBuffer(paneId, bufferId)

    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.PeekOverlay,
      peekOverlay = Some(PeekOverlay(PeekContent.QuickInfo(text), CursorPosition(4, 12)))
    )

  "Renderer.render" should "paint quick-info peek content inside the above-cursor overlay rect" in {
    val testScreen = screen(100, 30)
    val state      = stateWithPeek("signature(value: Int)")
    val layout     = LayoutEngine.calculateLayout(state, TerminalSize(100, 30))
    val overlay    = layout.aboveCursorOverlayRect.getOrElse(fail("Expected above-cursor overlay rect"))

    Renderer.render(state, cursorVisible = false, testScreen)

    val renderedText =
      (overlay.x + 1 until overlay.right - 1)
        .map(x => testScreen.getBackCharacter(x, overlay.y + 1).getCharacter)
        .mkString
        .trim

    renderedText should include("signature(value: Int)")

    testScreen.stopScreen()
  }
