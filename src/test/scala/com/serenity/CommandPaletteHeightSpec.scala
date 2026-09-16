package com.serenity

import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.InterfaceDensity
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** How tall the command palette is: its frame follows the viewport (#1045) and the interface density decides how many
  * rows fit inside it; an explicit `command_runner.visible_rows` still pins it exactly.
  */
class CommandPaletteHeightSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "The command palette" should "apply interface density to editor spacing and overlay height" in {
    val runner = com.serenity.command.CommandRunner.empty.activate(
      com.serenity.command.CommandRegistry.default,
      com.serenity.config.AppConfig.default
    )
    val commandSurface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(Some(CursorPosition(0, 0)), SurfacePlacement.BelowCursor)
    )
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val baseState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> Buffer.fromString(bufferId, "alpha\nbeta\ngamma")),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId)
      ),
      runtime = AppState.initial.runtime.copy(uiSurfaces = List(commandSurface))
    )
    val compact = LayoutEngine.calculateLayout(
      baseState.copy(persisted =
        baseState.persisted.copy(config = baseState.persisted.config.withInterfaceDensity(InterfaceDensity.Compact))
      ),
      ViewportSize(120, 30)
    )
    val comfortable = LayoutEngine.calculateLayout(baseState, ViewportSize(120, 30))
    val spacious = LayoutEngine.calculateLayout(
      baseState.copy(persisted =
        baseState.persisted.copy(config = baseState.persisted.config.withInterfaceDensity(InterfaceDensity.Spacious))
      ),
      ViewportSize(120, 30)
    )

    compact.editorPanelRect shouldBe comfortable.editorPanelRect
    spacious.editorPanelRect.x shouldBe comfortable.editorPanelRect.x
    spacious.editorPanelRect.width shouldBe comfortable.editorPanelRect.width
    compact.gutterRect.map(_.height) shouldBe Some(1)
    spacious.gutterRect.map(_.height) shouldBe Some(2)
    // The palette's frame follows the viewport (#1045); density decides how many items fit inside it.
    def itemSlots(layout: CalculatedLayout, density: InterfaceDensity): Int =
      val densityState = baseState.copy(persisted =
        baseState.persisted.copy(config = baseState.persisted.config.withInterfaceDensity(density))
      )
      EditorLayoutContract
        .from(densityState, ViewportSize(120, 30), layout)
        .overlayRowSlots(commandSurface.id)
        .count(slot => slot.kind.isInstanceOf[SurfaceContentRowKind.Item])
    itemSlots(compact, InterfaceDensity.Compact) should be > itemSlots(comfortable, InterfaceDensity.Comfortable)
    itemSlots(spacious, InterfaceDensity.Spacious) should be <= itemSlots(comfortable, InterfaceDensity.Comfortable)
  }

  it should "grow the command palette with the viewport rather than capping it at a few rows (#1045)" in {
    val runner = com.serenity.command.CommandRunner.empty.activate(
      com.serenity.command.CommandRegistry.default,
      com.serenity.config.AppConfig.default
    )
    val commandSurface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(Some(CursorPosition(0, 0)), SurfacePlacement.BelowCursor)
    )
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> Buffer.fromString(bufferId, "alpha")),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId)
      ),
      runtime = AppState.initial.runtime.copy(uiSurfaces = List(commandSurface))
    )
    def itemSlots(viewport: ViewportSize): Int =
      val layout = LayoutEngine.calculateLayout(state, viewport)
      EditorLayoutContract
        .from(state, viewport, layout)
        .overlayRowSlots(commandSurface.id)
        .count(slot => slot.kind.isInstanceOf[SurfaceContentRowKind.Item])
    def frameHeight(viewport: ViewportSize): Int =
      LayoutEngine.calculateLayout(state, viewport).belowCursorOverlayRect.map(_.height).getOrElse(0)

    itemSlots(ViewportSize(120, 60)) should be >= 10
    itemSlots(ViewportSize(120, 60)) should be > itemSlots(ViewportSize(120, 30))
    // Never more than the configured share of the space below the caret, and never fewer than the density's minimum.
    frameHeight(ViewportSize(120, 60)) should be <= 36
    itemSlots(ViewportSize(120, 12)) should be >= 1
    // An explicit override is still honoured exactly (`command_runner.visible_rows`).
    val pinned = state.copy(persisted =
      state.persisted.copy(config = state.persisted.config.withCommandRunnerVisibleRows(Some(4)))
    )
    LayoutEngine.calculateLayout(pinned, ViewportSize(120, 60)).belowCursorOverlayRect.map(_.height) shouldBe
      LayoutEngine.calculateLayout(pinned, ViewportSize(120, 30)).belowCursorOverlayRect.map(_.height)
  }
