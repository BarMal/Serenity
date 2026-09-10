package com.serenity.ui.renderer

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.{DockedPanelFixtures, MockRenderSurface, TestWorkspaceTrees}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated unit coverage for `RendererFloatingPanels` (issue #1421).
  *
  * The backdrop-dimming half of [[RendererFloatingPanels.renderModalLayer]] already has direct coverage in
  * `FileWorkflowModalRenderingSpec` ("dim the workspace behind a blocking modal..."), and every modal/floating
  * overlay's *content* is exercised end-to-end by the many `*ModalRenderingSpec`/`*OverlaySpec` specs -- this spec
  * targets the two things about this object those specs pass through without pinning: the trivial
  * `pinnedAndExpandedSurfaces` delegation, blur being applied to a docked panel's backdrop when the surface material
  * calls for it, and floating panels having their *exact* on-screen pixel rect remembered on `RendererFrameState` (the
  * fact [[RendererFrameState.previousFloatingSurfaceRects]] itself round-trips a map is already covered by
  * `RendererFrameStateSpec`; what is not covered anywhere else is that `RendererFloatingPanels` computes and hands it
  * the *right* rect for a real floating overlay).
  */
class RendererFloatingPanelsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def baseState(): AppState =
    val buffer = Buffer.fromString(bufferId, "alpha\nbeta\ngamma")
    val pane   = EditorPane.withBuffer(paneId, bufferId)

    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId)
      )
    )

  "pinnedAndExpandedSurfaces" should "return exactly state.pinnedSurfaces, including a currently-expanded one" in {
    val surfaceId = SurfaceId("diagnostics")
    val docked =
      DockedPanelFixtures.dock(baseState(), surfaceId, SurfaceContent.Diagnostics(Nil), PanelPosition.Right, 22)
    val expanded = DockedPanelFixtures.expand(docked, surfaceId)

    RendererFloatingPanels.pinnedAndExpandedSurfaces(expanded) shouldBe expanded.pinnedSurfaces
    RendererFloatingPanels.pinnedAndExpandedSurfaces(expanded).map(_.id) shouldBe List(surfaceId)
  }

  it should "return an empty list when nothing is docked" in {
    RendererFloatingPanels.pinnedAndExpandedSurfaces(baseState()) shouldBe Nil
  }

  "renderPinnedPanels" should "blur a docked panel's backdrop when the active surface material calls for a blur" in {
    // `AppConfig.default`'s surface material (Frosted, blurRadius = 0.18) already yields a positive
    // `SurfaceMaterials.effectiveBlurRadius` -- no config override needed to exercise this branch.
    val state = DockedPanelFixtures.dock(
      baseState(),
      SurfaceId("diagnostics"),
      SurfaceContent.Diagnostics(Nil),
      PanelPosition.Right,
      22
    )
    val surface = new MockRenderSurface(100, 30)

    RendererEntryPoints.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.blurRegionCalls should not be empty
  }

  it should "not blur a docked panel's backdrop once visual flair is turned off" in {
    val config = com.serenity.config.AppConfig.default.withVisualFlairLevel(com.serenity.config.VisualFlairLevel.Off)
    val docked = DockedPanelFixtures.dock(
      baseState(),
      SurfaceId("diagnostics"),
      SurfaceContent.Diagnostics(Nil),
      PanelPosition.Right,
      22
    )
    val state   = docked.copy(persisted = docked.persisted.copy(config = config))
    val surface = new MockRenderSurface(100, 30)

    RendererEntryPoints.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.blurRegionCalls shouldBe empty
  }

  "renderFloatingPanels" should "remember the exact on-screen pixel rect it painted a floating overlay's surface at" in {
    val surfaceId = SurfaceId("peek")
    val state = baseState().copy(
      persisted = baseState().persisted.copy(focus = Focus.Surface(surfaceId)),
      runtime = baseState().runtime.copy(
        uiSurfaces = List(
          UiSurface(
            surfaceId,
            SurfaceContent.QuickInfo("signature(value: Int)"),
            SurfacePresentation.Floating(Some(CursorPosition(0, 2)), SurfacePlacement.AboveCursor)
          )
        )
      )
    )
    val surface      = new MockRenderSurface(100, 30, persistentContent = true)
    val viewportSize = ViewportSize(100, 30)

    RendererEntryPoints.render(state, cursorVisible = false, surface, viewportSize)

    val layout  = LayoutEngine.calculateLayout(state, viewportSize)
    val overlay = layout.aboveCursorOverlayRect.getOrElse(fail("expected an above-cursor overlay rect"))
    // Mirrors the (defaultFont, cellMetrics) `RendererEntryPoints.render`'s no-font overload resolves to internally.
    val cellMetrics = CellMetrics.fromFont(new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12))
    val leftPx      = cellMetrics.toPixelX(overlay.x)
    val topPx       = cellMetrics.toPixelY(overlay.y)
    val expectedRect = PixelRect(
      leftPx,
      topPx,
      cellMetrics.toPixelX(overlay.right) - leftPx,
      cellMetrics.toPixelY(overlay.bottom) - topPx
    )

    val key = SurfaceContentIdentity(surface)
    RendererFrameState.previousFloatingSurfaceRectsFor(key) shouldBe Map(surfaceId -> expectedRect)
  }

  it should "forget a floating surface's remembered rect once it stops being painted" in {
    val surfaceId = SurfaceId("peek")
    val withPeek = baseState().copy(
      persisted = baseState().persisted.copy(focus = Focus.Surface(surfaceId)),
      runtime = baseState().runtime.copy(
        uiSurfaces = List(
          UiSurface(
            surfaceId,
            SurfaceContent.QuickInfo("x"),
            SurfacePresentation.Floating(Some(CursorPosition(0, 2)), SurfacePlacement.AboveCursor)
          )
        )
      )
    )
    val withoutPeek  = baseState()
    val surface      = new MockRenderSurface(100, 30, persistentContent = true)
    val viewportSize = ViewportSize(100, 30)

    RendererEntryPoints.render(withPeek, cursorVisible = false, surface, viewportSize)
    RendererEntryPoints.render(withoutPeek, cursorVisible = false, surface, viewportSize)

    val key = SurfaceContentIdentity(surface)
    RendererFrameState.previousFloatingSurfaceRectsFor(key) shouldBe Map.empty
  }
