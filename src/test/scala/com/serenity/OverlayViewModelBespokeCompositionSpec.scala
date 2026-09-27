package com.serenity

import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.OverlayViewModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Proves `OverlayViewModel.compositionFor` dispatches floating peeks of bespoke-composed content kinds (Outline,
  * CommandRunnerPeek, ...) through their real composition object, not the generic `RowsSurfaceComposition` adapter --
  * split out of `OverlayViewModelSpec` to keep that file under the architecture ratchet's file-length target (#1683, PR
  * #1741).
  */
class OverlayViewModelBespokeCompositionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  "OverlayViewModel.fromState" should "compose a floating outline peek through OutlineSurfaceComposition, not the generic rows adapter" in {
    val symbols = List(
      Symbol("Serenity", SymbolKind.Class, Location(1, 1)),
      Symbol("render", SymbolKind.Method, Location(10, 3))
    )
    // Cursor at (0, 0) sits before every symbol's location, so `OverlayViewModel`'s own cursor-fallback (mirroring
    // `PinnedPanelViewModel.activeSymbolLocation`) resolves no active symbol here either -- keeping this test's
    // expected composition a plain, fallback-free `activeLocation = None` call.
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(editing = EditingState(List(CursorPosition(0, 0))))
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.Surface(SurfaceId("outline-peek"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("outline-peek"),
            SurfaceContent.Outline(symbols, activeLocation = None),
            SurfacePresentation.Floating(Some(CursorPosition(0, 0)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))
    val overlay = OverlayViewModel.fromState(state, layout).belowCursor.getOrElse(fail("Expected outline overlay"))

    val expected = OutlineSurfaceComposition.forOutline(symbols, activeLocation = None, overlay.rect)
    overlay.composition shouldBe Some(expected)
  }

  it should "compose a floating command-runner peek through CommandRunnerSurfaceComposition, not the generic rows adapter" in {
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(editing = EditingState(List(CursorPosition(1, 2))))
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.Surface(SurfaceId("cursor-peek"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("cursor-peek"),
            SurfaceContent.CommandRunnerPeek(runner),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
        ),
        // The cursor-peek prototype paints from a *frozen* anchor resolved once by `CursorPeekAnchorResolution`
        // (`FloatingSurfaceLayout.calculateFrozenCursorPeekRect`), not the live cursor position -- set directly here
        // rather than exercised through that resolution pass, which is `CursorPeekAnchorResolutionSpec`'s job.
        pointerGesture =
          AppState.initial.runtime.pointerGesture.copy(cursorPeekResolvedAnchor = Some(ScreenPosition(20, 5)))
      )
    )
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))
    val overlay = OverlayViewModel.fromState(state, layout).belowCursor.getOrElse(fail("Expected cursor-peek overlay"))

    val expected = CommandRunnerSurfaceComposition.forRunner(
      runner,
      overlay.rect,
      itemGapRows = 0,
      itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(
        SurfaceContent.CommandRunnerPeek(runner),
        state.persisted.config.interfaceDensity
      ),
      showKeyHints = false
    )
    overlay.composition shouldBe Some(expected)
    overlay.contentRowSlots shouldBe empty
  }
