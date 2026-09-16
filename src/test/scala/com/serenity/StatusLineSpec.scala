package com.serenity

import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.config.{AppConfig, StatusLinePlacement, StatusSegment}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The one status line: a pinned row under the workspace or a quiet floating row under the caret, never both. */
class StatusLineSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private val floating = AppConfig.default
    .withStatusLineSegments(List(StatusSegment.Position, StatusSegment.Title))
    .withStatusLinePlacement(StatusLinePlacement.Floating)

  private def editorState(cursor: CursorPosition = CursorPosition(0, 0), config: AppConfig = floating): AppState =
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
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId),
        config = config
      )
    )

  "AppState.floatingStatusLineSurface" should "derive a below-cursor surface from the active editor cursor" in {
    val state = editorState(CursorPosition(1, 2))

    val surface = state.floatingStatusLineSurface.getOrElse(fail("Expected floating status line"))

    surface.id shouldBe UiSurface.StatusLineSurfaceId
    surface.content shouldBe SurfaceContent.StatusLine("Line 2, Col 3 | Unsaved")
    surface.presentation shouldBe SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
  }

  it should "be the pinned row instead when placement is pinned" in {
    val state = editorState(CursorPosition(1, 2), floating.withStatusLinePlacement(StatusLinePlacement.Pinned))

    state.floatingStatusLineSurface shouldBe None
    state.statusLineText shouldBe Some("Line 2, Col 3 | Unsaved")
    state.surfaceById(UiSurface.StatusLineSurfaceId) shouldBe None
  }

  it should "be absent when the status line is off or has no segments" in {
    editorState(config = floating.withoutStatusLine).floatingStatusLineSurface shouldBe None
    editorState(config = floating.withStatusLineSegments(Nil)).floatingStatusLineSurface shouldBe None
    editorState(config = floating.withoutStatusLine).statusLineText shouldBe None
  }

  it should "step aside for the length of a typing burst, then come back" in {
    val base   = editorState()
    val typing = base.copy(runtime = base.runtime.copy(typingActivity = base.runtime.typingActivity.observed))

    typing.floatingStatusLineSurface shouldBe None

    val settled = Iterator
      .iterate(typing)(state =>
        state.copy(runtime = state.runtime.copy(typingActivity = state.runtime.typingActivity.advance))
      )
      .drop(TypingActivity.QuietTicks)
      .next()
    settled.floatingStatusLineSurface shouldBe defined
  }

  "LayoutEngine.calculateLayout" should "place the floating status line below the active cursor as one unframed row" in {
    val state  = editorState(CursorPosition(1, 2))
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))

    val rect = layout.belowCursorOverlayStack.collectFirst { case (UiSurface.StatusLineSurfaceId, rect) => rect }
    val text = "Line 2, Col 3 | Unsaved"
    rect.map(_.height) shouldBe Some(1)
    rect.map(_.width) shouldBe Some(text.length + 2)
    SurfaceFrameLayout.borderCellsFor(SurfaceContent.StatusLine(text)) shouldBe 0
    layout.gutterRect shouldBe None
  }

  it should "reserve the bottom row for a pinned status line and nothing else" in {
    val state  = editorState(config = floating.withStatusLinePlacement(StatusLinePlacement.Pinned))
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))

    layout.gutterRect shouldBe Some(LayoutRect(0, 23, 80, 1))
    layout.pinnedSurfaceRects.get(UiSurface.StatusLineSurfaceId) shouldBe None
    layout.belowCursorOverlayStack.map(_._1) should not contain UiSurface.StatusLineSurfaceId
  }

  it should "reserve no row when the status line is off" in {
    val state  = editorState(config = floating.withoutStatusLine)
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))

    layout.gutterRect shouldBe None
    layout.belowCursorOverlayStack.map(_._1) should not contain UiSurface.StatusLineSurfaceId
  }

  it should "hide the floating status line behind command runner overlays" in {
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

  "SurfaceContentResolver" should "render status line text as a single overlay row" in {
    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.StatusLine("Line 1, Col 1"),
      LayoutRect(0, 0, 40, 1),
      SurfaceRenderMode.Floating
    )

    resolved.rows.map(_.plainText) shouldBe List("Line 1, Col 1")
  }

  "PinnedPanelViewModel" should "not model the pinned status line as a panel" in {
    val state  = editorState(config = floating.withStatusLinePlacement(StatusLinePlacement.Pinned))
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))

    val panels = com.serenity.ui.renderer.PinnedPanelViewModel.fromState(state, layout)

    panels.flatMap(_.lines) should not contain "Line 1, Col 1"
  }
