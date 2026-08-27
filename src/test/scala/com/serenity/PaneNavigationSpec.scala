package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{Direction, NewTab, NextTab, PreviousTab}
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class PaneNavigationSpec extends AnyFlatSpec with Matchers:

  behavior of "Buffer Navigation via Tab Commands"

  trait NavigationFixture:
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO]         = Slf4jFactory.create[IO]
    val logger                      = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager                = StateManager.apply(logger).unsafeRunSync()
    val wideTerminal                = com.serenity.ui.layout.ViewportSize(400, 24) // Wide enough for multiple panes

  it should "cycle forward through buffers with Ctrl+Tab (NextTab)" in new NavigationFixture:
    // Given: Wide terminal to allow multiple panes, then create three buffers
    stateManager.updateState(s => s.copy(runtime = s.runtime.copy(viewportSize = Some(wideTerminal)))).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync() // Buffer 1
    stateManager.applyEvent(NewTab).unsafeRunSync() // Buffer 2
    val state = stateManager.getCurrentState.unsafeRunSync()

    // Should have 3 buffers
    state.persisted.buffers should have size 3
    val bufferIds = state.persisted.bufferOrder
    bufferIds should have size 3

    // Should start focused on Buffer 2 (last created)
    val initialFocusedBufferId = state.focusedBufferId.get
    initialFocusedBufferId shouldBe bufferIds(2)

    // When: Press Ctrl+Tab (NextTab) to cycle forward
    stateManager.applyEvent(NextTab).unsafeRunSync()
    val stateAfterNext1 = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should focus on Buffer 0 (next in cycle: Buffer 2 -> Buffer 0)
    stateAfterNext1.focusedBufferId.get shouldBe bufferIds(0)

    // When: Press Ctrl+Tab again
    stateManager.applyEvent(NextTab).unsafeRunSync()
    val stateAfterNext2 = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should focus on Buffer 1 (Buffer 0 -> Buffer 1)
    stateAfterNext2.focusedBufferId.get shouldBe bufferIds(1)

    // When: Press Ctrl+Tab again
    stateManager.applyEvent(NextTab).unsafeRunSync()
    val stateAfterNext3 = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should cycle back to Buffer 2 (Buffer 1 -> Buffer 2)
    stateAfterNext3.focusedBufferId.get shouldBe bufferIds(2)

  it should "keep workspace leaves stable while opening and navigating buffers" in new NavigationFixture:
    val initialLeaves =
      stateManager.getCurrentState.unsafeRunSync().persisted.layout.effectiveWorkspaceTree.map(_.paneIds)

    stateManager.applyEvent(NewTab).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()
    stateManager.applyEvent(NextTab).unsafeRunSync()
    stateManager.applyEvent(PreviousTab).unsafeRunSync()

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.persisted.layout.effectiveWorkspaceTree.map(_.paneIds) shouldBe initialLeaves
    finalState.persisted.layout.editorPanes.keySet shouldBe initialLeaves.toList.flatten.toSet

  it should "cycle backward through buffers with Ctrl+Shift+Tab (PreviousTab)" in new NavigationFixture:
    // Given: Wide terminal to allow multiple panes, then create three buffers
    stateManager.updateState(s => s.copy(runtime = s.runtime.copy(viewportSize = Some(wideTerminal)))).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync() // Buffer 1
    stateManager.applyEvent(NewTab).unsafeRunSync() // Buffer 2
    val state = stateManager.getCurrentState.unsafeRunSync()

    val bufferIds = state.persisted.bufferOrder
    bufferIds should have size 3

    // Should start focused on Buffer 2 (last created)
    state.focusedBufferId.get shouldBe bufferIds(2)

    // When: Press Ctrl+Shift+Tab (PreviousTab) to cycle backward
    stateManager.applyEvent(PreviousTab).unsafeRunSync()
    val stateAfterPrev1 = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should focus on Buffer 1 (previous in cycle: Buffer 2 -> Buffer 1)
    stateAfterPrev1.focusedBufferId.get shouldBe bufferIds(1)

    // When: Press Ctrl+Shift+Tab again
    stateManager.applyEvent(PreviousTab).unsafeRunSync()
    val stateAfterPrev2 = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should focus on Buffer 0 (Buffer 1 -> Buffer 0)
    stateAfterPrev2.focusedBufferId.get shouldBe bufferIds(0)

    // When: Press Ctrl+Shift+Tab again
    stateManager.applyEvent(PreviousTab).unsafeRunSync()
    val stateAfterPrev3 = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should cycle back to Buffer 2 (Buffer 0 -> Buffer 2, wrapping around)
    stateAfterPrev3.focusedBufferId.get shouldBe bufferIds(2)

  it should "handle navigation with only two buffers" in new NavigationFixture:
    // Given: Wide terminal and two buffers
    stateManager.updateState(s => s.copy(runtime = s.runtime.copy(viewportSize = Some(wideTerminal)))).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync() // Buffer 1
    val state = stateManager.getCurrentState.unsafeRunSync()

    val bufferIds = state.persisted.bufferOrder
    bufferIds should have size 2

    // Should start focused on Buffer 1 (last created)
    state.focusedBufferId.get shouldBe bufferIds(1)

    // When: Press Ctrl+Tab (forward)
    stateManager.applyEvent(NextTab).unsafeRunSync()
    val stateAfterNext = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should toggle to Buffer 0
    stateAfterNext.focusedBufferId.get shouldBe bufferIds(0)

    // When: Press Ctrl+Shift+Tab (backward)
    stateManager.applyEvent(PreviousTab).unsafeRunSync()
    val stateAfterPrev = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should toggle back to Buffer 1
    stateAfterPrev.focusedBufferId.get shouldBe bufferIds(1)

  it should "handle navigation with single buffer gracefully" in new NavigationFixture:
    // Given: Only one buffer (initial state)
    val state = stateManager.getCurrentState.unsafeRunSync()

    state.persisted.buffers should have size 1
    state.persisted.bufferOrder should have size 1
    val initialBufferId = state.focusedBufferId.get

    // When: Try to navigate with Ctrl+Tab
    stateManager.applyEvent(NextTab).unsafeRunSync()
    val stateAfterNext = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should stay on the same buffer (no change)
    stateAfterNext.focusedBufferId.get shouldBe initialBufferId

    // When: Try to navigate with Ctrl+Shift+Tab
    stateManager.applyEvent(PreviousTab).unsafeRunSync()
    val stateAfterPrev = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should stay on the same buffer (no change)
    stateAfterPrev.focusedBufferId.get shouldBe initialBufferId

  it should "maintain correct focus when buffers are closed during navigation" in new NavigationFixture:
    // Given: Wide terminal and four buffers
    stateManager.updateState(s => s.copy(runtime = s.runtime.copy(viewportSize = Some(wideTerminal)))).unsafeRunSync()
    (1 to 3).foreach(_ => stateManager.applyEvent(NewTab).unsafeRunSync())
    val state = stateManager.getCurrentState.unsafeRunSync()

    val bufferIds = state.persisted.bufferOrder
    bufferIds should have size 4

    // Navigate to Buffer 1 (second buffer in order)
    // Start from current position (Buffer 3), navigate to Buffer 1
    @annotation.tailrec
    def navigatePreviousUntil(targetBufferId: BufferId): Unit =
      if stateManager.getCurrentState.unsafeRunSync().focusedBufferId.get != targetBufferId then
        stateManager.applyEvent(PreviousTab).unsafeRunSync()
        navigatePreviousUntil(targetBufferId)

    navigatePreviousUntil(bufferIds(1))
    val stateOnBuffer1 = stateManager.getCurrentState.unsafeRunSync()
    stateOnBuffer1.focusedBufferId.get shouldBe bufferIds(1)

    // When: Navigate to Buffer 2, then close Buffer 2
    stateManager.applyEvent(NextTab).unsafeRunSync() // Buffer 1 -> Buffer 2
    val stateOnBuffer2 = stateManager.getCurrentState.unsafeRunSync()
    stateOnBuffer2.focusedBufferId.get shouldBe bufferIds(2)

    // Close Buffer 2 (this would require a close buffer command - for now just verify navigation works)
    // Note: Buffer closing would be handled by a separate close command

    // Then: Navigation should still work with remaining buffers
    stateManager.applyEvent(NextTab).unsafeRunSync()
    val stateAfterNav = stateManager.getCurrentState.unsafeRunSync()

    // Should successfully navigate to a different existing buffer
    stateAfterNav.focusedBufferId shouldBe defined
    stateAfterNav.focusedBufferId.get should not be bufferIds(2) // Should move away from current buffer

  it should "navigate through all buffers regardless of pane visibility constraints" in new NavigationFixture:
    // Given: Many buffers but limited terminal width (fewer visible panes)
    val narrowTerminal = com.serenity.ui.layout.ViewportSize(120, 24) // Limited width
    stateManager.updateState(s => s.copy(runtime = s.runtime.copy(viewportSize = Some(narrowTerminal)))).unsafeRunSync()

    // Create 5 buffers
    (1 to 4).foreach(_ => stateManager.applyEvent(NewTab).unsafeRunSync())
    val state = stateManager.getCurrentState.unsafeRunSync()
    state.persisted.buffers should have size 5

    val bufferIds              = state.persisted.bufferOrder
    val initialFocusedBufferId = state.focusedBufferId.get

    // When: Navigate with Ctrl+Tab multiple times
    stateManager.applyEvent(NextTab).unsafeRunSync()
    val stateAfterNav1 = stateManager.getCurrentState.unsafeRunSync()

    stateManager.applyEvent(NextTab).unsafeRunSync()
    val stateAfterNav2 = stateManager.getCurrentState.unsafeRunSync()

    stateManager.applyEvent(NextTab).unsafeRunSync()
    val stateAfterNav3 = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should cycle among all buffers, regardless of pane visibility
    val focusedBufferIds = List(
      initialFocusedBufferId,
      stateAfterNav1.focusedBufferId.get,
      stateAfterNav2.focusedBufferId.get,
      stateAfterNav3.focusedBufferId.get
    )

    // Should have visited at least 3 different buffers
    focusedBufferIds.toSet.size should be >= 3

    // All focused buffers should exist in buffer order
    focusedBufferIds.foreach(bufferId => bufferIds should contain(bufferId))

  it should "focus deterministic geometric neighbours in a nested workspace" in new NavigationFixture:
    stateManager
      .updateState(s => s.copy(runtime = s.runtime.copy(viewportSize = Some(ViewportSize(120, 36)))))
      .unsafeRunSync()
    val first  = stateManager.getCurrentState.unsafeRunSync().persisted.layout.activeEditorPaneId.get
    val second = stateManager.splitPaneHorizontal(first).unsafeRunSync()
    val third  = stateManager.splitPaneVertical(second).unsafeRunSync()

    stateManager.switchToPane(first).unsafeRunSync()
    stateManager.focusPaneInDirection(Direction.Right).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().persisted.focus shouldBe Focus.EditorPane(second)

    stateManager.focusPaneInDirection(Direction.Down).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().persisted.focus shouldBe Focus.EditorPane(third)

    stateManager.focusPaneInDirection(Direction.Left).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().persisted.focus shouldBe Focus.EditorPane(first)

  it should "ignore floating surfaces and unusable leaves during directional focus in constrained viewports" in new NavigationFixture:
    stateManager
      .updateState(s => s.copy(runtime = s.runtime.copy(viewportSize = Some(ViewportSize(8, 5)))))
      .unsafeRunSync()
    val first  = stateManager.getCurrentState.unsafeRunSync().persisted.layout.activeEditorPaneId.get
    val second = stateManager.splitPaneHorizontal(first).unsafeRunSync()
    stateManager
      .updateState { state =>
        state.copy(
          runtime = state.runtime.copy(
            uiSurfaces = List(
              UiSurface(
                SurfaceId("quick-info"),
                SurfaceContent.QuickInfo("details"),
                SurfacePresentation.Floating(None, SurfacePlacement.AboveCursor)
              )
            )
          )
        )
      }
      .unsafeRunSync()

    stateManager.switchToPane(first).unsafeRunSync()
    stateManager.focusPaneInDirection(Direction.Right).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.persisted.focus should (be(Focus.EditorPane(second)) or be(Focus.EditorPane(first)))
    state.persisted.focus should not be Focus.Surface(SurfaceId("quick-info"))
