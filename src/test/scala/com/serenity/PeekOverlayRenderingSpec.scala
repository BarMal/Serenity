package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, LayoutEngine, ViewportSize}
import com.serenity.ui.renderer.Renderer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PeekOverlayRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def stateWithPeek(text: String): AppState =
    val buffer = Buffer
      .fromString(
        bufferId,
        List.fill(10)("abcdefghijklmnopqrstuvwxyz").mkString("\n")
      )
      .copy(cursors = List(CursorPosition(4, 12)))
    val pane = EditorPane.withBuffer(paneId, bufferId)

    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(SurfaceId("peek")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("peek"),
          SurfaceContent.QuickInfo(text),
          SurfacePresentation.Floating(Some(CursorPosition(4, 12)), SurfacePlacement.AboveCursor),
          dismissOnMove = true
        )
      )
    )

  "Renderer.render" should "paint quick-info peek content inside the above-cursor overlay rect" in {
    val surface = new MockRenderSurface(100, 30)
    val state   = stateWithPeek("signature(value: Int)")
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val overlay = layout.aboveCursorOverlayRect.getOrElse(fail("Expected above-cursor overlay rect"))

    Renderer.render(state, cursorVisible = false, surface, ViewportSize(100, 30))

    val renderedText =
      (overlay.x + 1 until overlay.right - 1)
        .map(x => surface.getChar(x, overlay.y + 1))
        .mkString
        .trim

    renderedText should include("signature(value: Int)")
  }
