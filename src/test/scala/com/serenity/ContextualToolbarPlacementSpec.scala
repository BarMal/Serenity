package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.config.ToolbarDisplayMode
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Where the contextual toolbar is anchored and how it follows the cursor/selection: initial placement below/above the
  * caret, stacking with the command runner, tracking the caret as it moves, and staying hidden once its anchor scrolls
  * out of view. Focus/detail lifecycle is covered in [[ContextualToolbarDetailSpec]], mouse interaction in
  * [[ContextualToolbarMouseSpec]], and display-mode/rendering behaviour in [[ContextualToolbarDisplaySpec]]. Pure
  * cell-space geometry (row wrapping, widths, hit-testing) is covered directly in [[ContextualToolbarLayoutSpec]].
  */
class ContextualToolbarPlacementSpec extends AnyFlatSpec with Matchers with ContextualToolbarTestSupport:

  "Contextual toolbar" should "toggle on below the cursor without stealing focus and stack above the command runner" in {
    val stateManager = createStateManager("ContextualToolbarSpec-stack")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val opened         = stateManager.getCurrentState.unsafeRunSync()
    val toolbarSurface = opened.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar"))
    opened.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    toolbarSurface.presentation shouldBe SurfacePresentation.Floating(
      opened.activeCursorPosition,
      SurfacePlacement.BelowCursor
    )
    toolbarStateFrom(opened).displayMode shouldBe ToolbarDisplayMode.IconAndText

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val withRunner = stateManager.getCurrentState.unsafeRunSync()
    val layout = LayoutEngine
      .calculateLayoutWithUI(withRunner, withRunner.runtime.viewportSize.getOrElse(fail("Expected viewport size")))
    layout.aboveCursorOverlayStack.map(_._1) shouldBe Nil
    layout.belowCursorOverlayStack.map(_._1) shouldBe List(
      toolbarSurface.id,
      withRunner.commandRunnerSurface.getOrElse(fail("Expected command runner")).id
    )

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val afterClose = stateManager.getCurrentState.unsafeRunSync()
    afterClose.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "follow the active cursor while it remains open" in {
    val stateManager = createStateManager("ContextualToolbarSpec-follow-caret")

    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted
            .copy(config = state.persisted.config.withContextualToolbarDisplayMode(ToolbarDisplayMode.IconOnly))
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(content =
                com.serenity.rope.Rope(
                  "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu\nnu xi omicron pi rho sigma tau"
                )
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, buffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    val initialRect = toolbarRect(stateManager.getCurrentState.unsafeRunSync())
    initialRect.width should be < 100
    stateManager.setCursorPosition(PaneId(0), 0, 48).unsafeRunSync()

    val movedHorizontally = toolbarRect(stateManager.getCurrentState.unsafeRunSync())
    movedHorizontally.x should be > initialRect.x

    stateManager.setCursorPosition(PaneId(0), 1, 12).unsafeRunSync()

    val movedVertically = toolbarRect(stateManager.getCurrentState.unsafeRunSync())
    movedVertically.y should be > movedHorizontally.y
  }

  it should "move below the cursor line when there is no room above the selection" in {
    val stateManager = createStateManager("ContextualToolbarSpec-top-row-placement")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 20))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val contentRect = LayoutEngine
      .calculateEditorWorkspaceLayout(state, layout)
      .activeContentRect(state)
      .getOrElse(
        fail("Expected active content rect")
      )
    val rect = toolbarRect(state)

    rect.y should be > contentRect.y
    rect.bottom should be <= contentRect.bottom
  }

  it should "prefer the compact palette above the editing target when that safe placement fits" in {
    val stateManager = createStateManager("ContextualToolbarSpec-above-placement")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager
      .updateState { state =>
        val bufferId = activeBufferId(state)
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(content = com.serenity.rope.Rope(List.fill(12)("toolbar target").mkString("\n"))),
            editing =
              state.persisted.buffers(bufferId).editing.copy(selection = None, cursors = List(CursorPosition(8, 4)))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, buffer)))
      }
      .unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val contentRect = LayoutEngine
      .calculateEditorWorkspaceLayout(state, layout)
      .activeContentRect(state)
      .getOrElse(fail("Expected active content rect"))
    val cursorY = contentRect.y + 8
    val rect    = toolbarRect(state)

    rect.y should be >= cursorY
    rect.y should be >= contentRect.y
  }

  it should "anchor above the start of a multi-line selection rather than its trailing caret" in {
    val stateManager = createStateManager("ContextualToolbarSpec-selection-anchor")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = activeBufferId(state)
        val selection = Selection(CursorPosition(12, 1), CursorPosition(16, 4))
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(content = com.serenity.rope.Rope(List.fill(20)("toolbar selection target").mkString("\n"))),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(selection = Some(selection), cursors = List(selection.focus))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, buffer)))
      }
      .unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val contentRect = LayoutEngine
      .calculateEditorWorkspaceLayout(state, layout)
      .activeContentRect(state)
      .getOrElse(fail("Expected active content rect"))

    toolbarRect(state).bottom should be <= contentRect.y + 12
  }

  it should "center on the bounding box of a same-line selection" in {
    val stateManager = createStateManager("ContextualToolbarSpec-inline-selection-center")

    stateManager.applyEvent(ResizeEvent(ViewportSize(160, 30))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = activeBufferId(state)
        val selection = Selection(CursorPosition(12, 60), CursorPosition(12, 100))
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(content = com.serenity.rope.Rope(List.fill(20)("x" * 140).mkString("\n"))),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(selection = Some(selection), cursors = List(selection.focus))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, buffer)))
      }
      .unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val contentRect = LayoutEngine
      .calculateEditorWorkspaceLayout(state, layout)
      .activeContentRect(state)
      .getOrElse(fail("Expected active content rect"))

    toolbarRect(state).centerX shouldBe contentRect.x + 80
  }

  it should "place below a top-edge multi-line selection without covering its selected text" in {
    val stateManager = createStateManager("ContextualToolbarSpec-top-edge-selection-placement")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 20))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = activeBufferId(state)
        val selection = Selection(CursorPosition(0, 1), CursorPosition(5, 4))
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(content = com.serenity.rope.Rope(List.fill(12)("toolbar selection target").mkString("\n"))),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(selection = Some(selection), cursors = List(selection.focus))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, buffer)))
      }
      .unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val contentRect = LayoutEngine
      .calculateEditorWorkspaceLayout(state, layout)
      .activeContentRect(state)
      .getOrElse(fail("Expected active content rect"))

    toolbarRect(state).y should be >= contentRect.y + 6
  }

  it should "not leave a detached toolbar visible when its anchor scrolls out of view" in {
    val stateManager = createStateManager("ContextualToolbarSpec-offscreen-anchor")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 20))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId = activeBufferId(state)
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(content = com.serenity.rope.Rope(List.fill(40)("toolbar target").mkString("\n"))),
            editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(30, 4))),
            viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 10, visibleColumns = 120)
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, buffer)))
      }
      .unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val surface  = state.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar surface"))
    val contract = EditorLayoutContract.from(state, viewport, LayoutEngine.calculateLayoutWithUI(state, viewport))

    contract.overlayRect(surface.id) shouldBe None
  }
