package com.serenity

import com.serenity.command.CommandRunner
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CursorOverlayLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def baseState(cursor: CursorPosition = CursorPosition(6, 18)): AppState =
    val buffer = Buffer
      .fromString(
        bufferId,
        List.fill(20)("abcdefghijklmnopqrstuvwxyz").mkString("\n")
      )
      .copy(cursors = List(cursor))
    val pane = EditorPane.withBuffer(paneId, bufferId)

    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.EditorPane(paneId)
    )

  "LayoutEngine.calculateLayout" should "place a peek overlay above the anchored cursor when space is available" in {
    val state = baseState().copy(
      uiSurfaces = List(
        UiSurface(
          SurfaceId("peek"),
          SurfaceContent.QuickInfo("map(value: A => B): List[B]"),
          SurfacePresentation.Floating(Some(CursorPosition(6, 18)), SurfacePlacement.AboveCursor),
          dismissOnMove = true
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))

    layout.aboveCursorOverlayRect shouldBe defined
    layout.belowCursorOverlayRect shouldBe None

    val rect          = layout.aboveCursorOverlayRect.get
    val paneRect      = LayoutEngine.calculatePaneLayouts(state, layout)(paneId)
    val contentRect   = CursorLayout.contentRectForPane(paneRect)
    val contentTopY   = paneRect.y + 1
    val cursorScreenY = contentTopY + 6

    rect.bottom should be <= cursorScreenY
    rect.x shouldBe contentRect.x
    rect.width shouldBe contentRect.width
    rect.right shouldBe contentRect.right
    rect.y should be >= contentTopY
  }

  it should "clamp an above-cursor peek overlay into the active pane when the cursor is near the top" in {
    val state = baseState(cursor = CursorPosition(0, 5)).copy(
      uiSurfaces = List(
        UiSurface(
          SurfaceId("peek-top"),
          SurfaceContent.QuickInfo("near-top"),
          SurfacePresentation.Floating(Some(CursorPosition(0, 5)), SurfacePlacement.AboveCursor),
          dismissOnMove = true
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 20))

    layout.aboveCursorOverlayRect shouldBe defined

    val rect        = layout.aboveCursorOverlayRect.get
    val paneRect    = LayoutEngine.calculatePaneLayouts(state, layout)(paneId)
    val contentRect = CursorLayout.contentRectForPane(paneRect)
    val contentTopY = paneRect.y + 1

    rect.y shouldBe contentTopY
    rect.bottom should be <= paneRect.bottom
    rect.x shouldBe contentRect.x
    rect.width shouldBe contentRect.width
  }

  it should "place an active command runner below the anchored cursor when space is available" in {
    val state = baseState().copy(
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(
            CommandRunner(
              isActive = true,
              searchTerm = "",
              selectedIndex = 0,
              filteredCommands = List.empty
            )
          ),
          SurfacePresentation.Floating(Some(CursorPosition(6, 18)), SurfacePlacement.BelowCursor)
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))

    layout.aboveCursorOverlayRect shouldBe None
    layout.belowCursorOverlayRect shouldBe defined

    val rect          = layout.belowCursorOverlayRect.get
    val paneRect      = LayoutEngine.calculatePaneLayouts(state, layout)(paneId)
    val contentRect   = CursorLayout.contentRectForPane(paneRect)
    val contentTopY   = paneRect.y + 1
    val cursorScreenY = contentTopY + 6

    rect.y should be > cursorScreenY
    rect.x shouldBe contentRect.x
    rect.width shouldBe contentRect.width
    rect.right shouldBe contentRect.right
    rect.bottom should be <= paneRect.bottom
  }

  it should "size command runner overlays from configured visible rows" in {
    val state = baseState().copy(
      config = AppState.initial.config.withCommandRunnerVisibleRows(Some(7)),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(
            CommandRunner(
              isActive = true,
              searchTerm = "",
              selectedIndex = 0,
              filteredCommands = List.empty
            )
          ),
          SurfacePresentation.Floating(Some(CursorPosition(6, 18)), SurfacePlacement.BelowCursor)
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))

    layout.belowCursorOverlayRect.map(_.height) shouldBe Some(11)
  }

  it should "size a find overlay to fit its header, query row, and result footer" in {
    val state = baseState().copy(
      uiSurfaces = List(
        UiSurface(
          SurfaceId("find"),
          SurfaceContent.ModalWorkflow(Modal.Find("needle", List(FindResult(1, 0), FindResult(3, 0)), 0)),
          SurfacePresentation.Floating(Some(CursorPosition(6, 18)), SurfacePlacement.BelowCursor)
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))

    layout.belowCursorOverlayRect.map(_.height) shouldBe Some(5)
  }

  it should "size a replace overlay to fit fields, actions, scope, and status" in {
    val state = baseState().copy(
      uiSurfaces = List(
        UiSurface(
          SurfaceId("replace"),
          SurfaceContent.ModalWorkflow(
            Modal.ReplaceWorkflow(
              ReplaceWorkflowState(statusMessage = Some("3 matches will be replaced"))
            )
          ),
          SurfacePresentation.Floating(Some(CursorPosition(6, 18)), SurfacePlacement.BelowCursor)
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))

    layout.belowCursorOverlayRect.map(_.height) shouldBe Some(8)
  }
end CursorOverlayLayoutSpec
