package com.serenity

import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.config.{AppConfig, CursorInfoBarMode, CursorInfoBarPlacement}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{SurfaceContentResolver, SurfaceRenderMode}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CursorInfoBarSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def editorState(
    cursor: CursorPosition = CursorPosition(0, 0),
    config: AppConfig = AppConfig.default.withCursorInfoBarMode(CursorInfoBarMode.Detailed)
  ): AppState =
    val buffer = Buffer
      .fromString(bufferId, "alpha\nbeta\ngamma")
      .copy(editing = EditingState(cursors = List(cursor)))
    val initialState = AppState.initial
    initialState.copy(persisted =
      initialState.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId)
        ),
        focus = Focus.EditorPane(paneId),
        config = config
      )
    )

  "AppState.cursorInfoBarSurface" should "derive a below-cursor surface from the active editor cursor" in {
    val state = editorState(CursorPosition(1, 2))

    val surface = state.cursorInfoBarSurface.getOrElse(fail("Expected cursor info bar surface"))

    surface.id shouldBe SurfaceId("cursor-info-bar")
    surface.content shouldBe SurfaceContent.CursorInfoBar("Line 2, Col 3 | Plain Text | Unsaved")
    surface.presentation shouldBe SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
  }

  it should "use the gutter instead of a pinned surface when configured for pinned placement" in {
    val config = AppConfig.default
      .withCursorInfoBarMode(CursorInfoBarMode.Position)
      .withCursorInfoBarPlacement(CursorInfoBarPlacement.PinnedBottom)
    val state = editorState(CursorPosition(1, 2), config)

    state.cursorInfoBarSurface shouldBe None
    state.pinnedSurfaces.map(_.id) should not contain SurfaceId("cursor-info-bar")
    state.surfaceById(SurfaceId("cursor-info-bar")) shouldBe None
  }

  it should "be disabled when the config mode is off" in {
    val state = editorState(config = AppConfig.default.withCursorInfoBarMode(CursorInfoBarMode.Off))

    state.cursorInfoBarSurface shouldBe None
  }

  "LayoutEngine.calculateLayout" should "place the cursor info bar below the active cursor" in {
    val layout = LayoutEngine.calculateLayout(editorState(), ViewportSize(80, 24))

    layout.belowCursorOverlayStack.map(_._1) should contain(SurfaceId("cursor-info-bar"))
  }

  it should "reserve gutter space for a bottom pinned cursor info bar" in {
    val config = AppConfig.default
      .withCursorInfoBarMode(CursorInfoBarMode.Detailed)
      .withCursorInfoBarPlacement(CursorInfoBarPlacement.PinnedBottom)
      .withGutter(false)
    val state  = editorState(config = config)
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))

    layout.gutterRect shouldBe Some(LayoutRect(0, 23, 80, 1))
    layout.pinnedSurfaceRects.get(SurfaceId("cursor-info-bar")) shouldBe None
    layout.belowCursorOverlayStack.map(_._1) should not contain SurfaceId("cursor-info-bar")
  }

  it should "hide the cursor info bar behind command runner overlays" in {
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)
    val commandRunnerSurface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val baseState = editorState()
    val state     = baseState.copy(runtime = baseState.runtime.copy(uiSurfaces = List(commandRunnerSurface)))

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))

    layout.belowCursorOverlayStack.map(_._1) shouldBe List(SurfaceId("command-runner"))
  }

  "SurfaceContentResolver" should "render cursor info bar text as a single overlay row" in {
    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CursorInfoBar("Line 1, Col 1"),
      LayoutRect(0, 0, 40, 3),
      SurfaceRenderMode.Floating
    )

    resolved.rows.map(_.plainText) shouldBe List("Line 1, Col 1")
  }

  "PinnedPanelViewModel" should "exclude the gutter-backed pinned cursor info bar" in {
    val config = AppConfig.default
      .withCursorInfoBarMode(CursorInfoBarMode.Position)
      .withCursorInfoBarPlacement(CursorInfoBarPlacement.PinnedBottom)
    val state  = editorState(config = config)
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))

    val panels = com.serenity.ui.renderer.PinnedPanelViewModel.fromState(state, layout)

    panels.flatMap(_.lines) should not contain "Line 1, Col 1"
  }
