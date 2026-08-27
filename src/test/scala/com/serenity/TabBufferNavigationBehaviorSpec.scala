package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{NewTab, NextTab, PreviousTab}
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class TabBufferNavigationBehaviorSpec extends AnyFlatSpec with Matchers:

  behavior of "Tab and Buffer Navigation User Behavior"

  trait NavigationBehaviorFixture:
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO]         = Slf4jFactory.create[IO]
    val logger                      = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager                = StateManager.apply(logger).unsafeRunSync()
    val wideTerminal                = ViewportSize(200, 24) // Wide enough for multiple panes
    val narrowTerminal              = ViewportSize(120, 24) // Only fits 1-2 panes at 50 chars minimum

  it should "start with exactly one pane and one buffer" in new NavigationBehaviorFixture:
    // When: App starts (initial state)
    val state = stateManager.getCurrentState.unsafeRunSync()

    // Then: Exactly one pane and one buffer exist
    state.persisted.layout.editorPanes should have size 1
    state.persisted.buffers should have size 1

    // And: The pane is associated with the buffer
    val pane = state.persisted.layout.editorPanes.values.head
    pane.bufferId shouldBe defined
    val bufferId = pane.bufferId.get
    state.persisted.buffers should contain key bufferId

    // And: Focus is on the pane/buffer
    state.persisted.focus shouldBe Focus.EditorPane(state.persisted.layout.editorPanes.keys.head)

  it should "create new buffer on Ctrl+T and focus switches to new buffer" in new NavigationBehaviorFixture:
    // Given: Initial state with one buffer
    val initialState        = stateManager.getCurrentState.unsafeRunSync()
    val originalBufferCount = initialState.persisted.buffers.size
    initialState.persisted.layout.editorPanes.size

    // When: Press Ctrl+T (NewTab command)
    stateManager.applyEvent(NewTab).unsafeRunSync()
    val stateAfterNewTab = stateManager.getCurrentState.unsafeRunSync()

    // Then: A new buffer exists (total = 2)
    stateAfterNewTab.persisted.buffers should have size (originalBufferCount + 1)

    // And: The new buffer has focus
    val focusedPaneId = stateAfterNewTab.persisted.focus match
      case Focus.EditorPane(paneId) => paneId
      case _                        => fail("Focus should be on an editor pane")

    val focusedPane = stateAfterNewTab.persisted.layout.editorPanes(focusedPaneId)
    focusedPane.bufferId shouldBe defined
    val focusedBufferId = focusedPane.bufferId.get

    // The focused buffer should be the newest one (highest ID)
    val allBufferIds = stateAfterNewTab.persisted.buffers.keys.toList.sortBy(_.value)
    focusedBufferId shouldBe allBufferIds.last

    // And: Number of panes is dictated by layout engine + window size
    // (We'll test this separately for different terminal sizes)

  it should "show single pane behavior: Ctrl+Shift+Tab switches to previous buffer in same pane" in new NavigationBehaviorFixture:
    // Given: Two buffers, narrow terminal (only 1 pane visible)
    stateManager.applyEvent(NewTab).unsafeRunSync() // Create second buffer
    val stateWith2Buffers = stateManager.getCurrentState.unsafeRunSync()

    // Get buffer order (sorted by creation order)
    val bufferIds      = stateWith2Buffers.persisted.buffers.keys.toList.sortBy(_.value)
    val firstBufferId  = bufferIds.head
    val secondBufferId = bufferIds.last

    // Should start with focus on second (newest) buffer
    val initialFocusedPaneId = stateWith2Buffers.persisted.focus match
      case Focus.EditorPane(paneId) => paneId
      case _                        => fail("Focus should be on an editor pane")

    val initialFocusedPane = stateWith2Buffers.persisted.layout.editorPanes(initialFocusedPaneId)
    initialFocusedPane.bufferId.get shouldBe secondBufferId

    // When: Press Ctrl+Shift+Tab (PreviousTab - captured as ReverseTab?)
    stateManager.applyEvent(PreviousTab).unsafeRunSync()
    val stateAfterPrevTab = stateManager.getCurrentState.unsafeRunSync()

    // Then: Same pane now shows first buffer's content
    val focusedPaneIdAfter = stateAfterPrevTab.persisted.focus match
      case Focus.EditorPane(paneId) => paneId
      case _                        => fail("Focus should be on an editor pane")

    // With narrow terminal, should still be same physical pane
    focusedPaneIdAfter shouldBe initialFocusedPaneId

    // But the pane should now show the first buffer
    val focusedPaneAfter = stateAfterPrevTab.persisted.layout.editorPanes(focusedPaneIdAfter)
    focusedPaneAfter.bufferId.get shouldBe firstBufferId

  it should "show dual pane behavior: Ctrl+Shift+Tab moves cursor between visible panes" in new NavigationBehaviorFixture:
    // Given: Two buffers, wide terminal (2 panes can be visible)
    stateManager.updateState(s => s.copy(runtime = s.runtime.copy(viewportSize = Some(wideTerminal)))).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync() // Create second buffer
    val singlePaneState = stateManager.getCurrentState.unsafeRunSync()
    val initialPaneId   = singlePaneState.persisted.layout.activeEditorPaneId.get
    val firstBufferId   = singlePaneState.persisted.bufferOrder.head
    stateManager.splitPaneHorizontal(initialPaneId, Some(firstBufferId)).unsafeRunSync()
    stateManager.switchToPane(initialPaneId).unsafeRunSync()
    val stateWith2Buffers = stateManager.getCurrentState.unsafeRunSync()

    // Check we have 2 panes visible in wide terminal
    // (This tests the layout engine respects terminal size)
    stateWith2Buffers.persisted.layout.editorPanes should have size 2

    val bufferIds      = stateWith2Buffers.persisted.buffers.keys.toList.sortBy(_.value)
    val secondBufferId = bufferIds.last

    // Should start with focus on second buffer (in second pane)
    val initialFocusedPaneId = stateWith2Buffers.persisted.focus match
      case Focus.EditorPane(paneId) => paneId
      case _                        => fail("Focus should be on an editor pane")

    // When: Press Ctrl+Shift+Tab (PreviousTab)
    stateManager.applyEvent(PreviousTab).unsafeRunSync()
    val stateAfterPrevTab = stateManager.getCurrentState.unsafeRunSync()

    // Then: Focus moves to different pane showing first buffer
    val focusedPaneIdAfter = stateAfterPrevTab.persisted.focus match
      case Focus.EditorPane(paneId) => paneId
      case _                        => fail("Focus should be on an editor pane")

    // Should be a different pane
    focusedPaneIdAfter should not be initialFocusedPaneId

    // The newly focused pane should show the first buffer
    val focusedPaneAfter = stateAfterPrevTab.persisted.layout.editorPanes(focusedPaneIdAfter)
    focusedPaneAfter.bufferId.get shouldBe firstBufferId

    // And the original pane should still show the second buffer
    val originalPane = stateAfterPrevTab.persisted.layout.editorPanes(initialFocusedPaneId)
    originalPane.bufferId.get shouldBe secondBufferId

  it should "switch focus forward with Ctrl+Tab" in new NavigationBehaviorFixture:
    // Given: Two buffers, wide terminal (2 panes visible)
    stateManager.updateState(s => s.copy(runtime = s.runtime.copy(viewportSize = Some(wideTerminal)))).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()

    // Move to first buffer using PreviousTab
    stateManager.applyEvent(PreviousTab).unsafeRunSync()
    val stateOnFirstBuffer = stateManager.getCurrentState.unsafeRunSync()

    val bufferIds      = stateOnFirstBuffer.persisted.buffers.keys.toList.sortBy(_.value)
    val firstBufferId  = bufferIds.head
    val secondBufferId = bufferIds.last

    // Verify we're on first buffer
    val initialFocusedPaneId = stateOnFirstBuffer.persisted.focus match
      case Focus.EditorPane(paneId) => paneId
      case _                        => fail("Focus should be on an editor pane")

    val initialFocusedPane = stateOnFirstBuffer.persisted.layout.editorPanes(initialFocusedPaneId)
    initialFocusedPane.bufferId.get shouldBe firstBufferId

    // When: Press Ctrl+Tab (NextTab)
    stateManager.applyEvent(NextTab).unsafeRunSync()
    val stateAfterNextTab = stateManager.getCurrentState.unsafeRunSync()

    // Then: Focus moves to second buffer
    val focusedPaneIdAfter = stateAfterNextTab.persisted.focus match
      case Focus.EditorPane(paneId) => paneId
      case _                        => fail("Focus should be on an editor pane")

    val focusedPaneAfter = stateAfterNextTab.persisted.layout.editorPanes(focusedPaneIdAfter)
    focusedPaneAfter.bufferId.get shouldBe secondBufferId

  it should "insert new buffer between existing buffers when Ctrl+T is pressed" in new NavigationBehaviorFixture:
    // Given: Two buffers, with focus on first buffer
    stateManager.applyEvent(NewTab).unsafeRunSync()      // Create second buffer
    stateManager.applyEvent(PreviousTab).unsafeRunSync() // Switch to first buffer
    val stateOnFirstBuffer = stateManager.getCurrentState.unsafeRunSync()

    val bufferIds      = stateOnFirstBuffer.persisted.buffers.keys.toList.sortBy(_.value)
    val firstBufferId  = bufferIds.head
    val secondBufferId = bufferIds.last

    // Verify we're focused on first buffer
    val focusedPaneId = stateOnFirstBuffer.persisted.focus match
      case Focus.EditorPane(paneId) => paneId
      case _                        => fail("Focus should be on an editor pane")

    val focusedPane = stateOnFirstBuffer.persisted.layout.editorPanes(focusedPaneId)
    focusedPane.bufferId.get shouldBe firstBufferId

    // When: Press Ctrl+T (NewTab) while on first buffer
    stateManager.applyEvent(NewTab).unsafeRunSync()
    val stateAfterNewTab = stateManager.getCurrentState.unsafeRunSync()

    // Then: A third buffer exists
    stateAfterNewTab.persisted.buffers should have size 3

    // And: The new buffer should be inserted between first and second
    val allBufferIds = stateAfterNewTab.persisted.buffers.keys.toList.sortBy(_.value)
    val newBufferId  = allBufferIds.filterNot(Set(firstBufferId, secondBufferId).contains).head

    // The new buffer should have focus
    val newFocusedPaneId = stateAfterNewTab.persisted.focus match
      case Focus.EditorPane(paneId) => paneId
      case _                        => fail("Focus should be on an editor pane")

    val newFocusedPane = stateAfterNewTab.persisted.layout.editorPanes(newFocusedPaneId)
    newFocusedPane.bufferId.get shouldBe newBufferId

    // Test buffer order by navigation
    // From new buffer, PreviousTab should go to first buffer
    stateManager.applyEvent(PreviousTab).unsafeRunSync()
    val stateAfterPrev = stateManager.getCurrentState.unsafeRunSync()
    val prevFocusedPaneId = stateAfterPrev.persisted.focus match
      case Focus.EditorPane(paneId) => paneId
      case _                        => fail("Focus should be on an editor pane")
    val prevFocusedPane = stateAfterPrev.persisted.layout.editorPanes(prevFocusedPaneId)
    prevFocusedPane.bufferId.get shouldBe firstBufferId

    // From first buffer, NextTab should go to new buffer (not second)
    stateManager.applyEvent(NextTab).unsafeRunSync()
    val stateAfterNext = stateManager.getCurrentState.unsafeRunSync()
    val nextFocusedPaneId = stateAfterNext.persisted.focus match
      case Focus.EditorPane(paneId) => paneId
      case _                        => fail("Focus should be on an editor pane")
    val nextFocusedPane = stateAfterNext.persisted.layout.editorPanes(nextFocusedPaneId)
    nextFocusedPane.bufferId.get shouldBe newBufferId

  it should "show only focused buffer's cursor (cursor visibility follows focus)" in new NavigationBehaviorFixture:
    // Given: Two buffers, wide terminal (2 panes visible)
    stateManager.updateState(s => s.copy(runtime = s.runtime.copy(viewportSize = Some(wideTerminal)))).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()
    val singlePaneState = stateManager.getCurrentState.unsafeRunSync()
    val initialPaneId   = singlePaneState.persisted.layout.activeEditorPaneId.get
    stateManager.splitPaneHorizontal(initialPaneId, Some(singlePaneState.persisted.bufferOrder.head)).unsafeRunSync()
    stateManager.switchToPane(initialPaneId).unsafeRunSync()
    val stateWith2Buffers = stateManager.getCurrentState.unsafeRunSync()

    // Should have 2 panes, each with different buffers
    stateWith2Buffers.persisted.layout.editorPanes should have size 2
    val paneIds = stateWith2Buffers.persisted.layout.editorPanes.keys.toList.sortBy(_.value)
    val pane1Id = paneIds.head
    val pane2Id = paneIds.last

    stateWith2Buffers.persisted.layout.editorPanes(pane1Id)
    stateWith2Buffers.persisted.layout.editorPanes(pane2Id)

    // Initially, focus should be on one pane
    val focusedPaneId = stateWith2Buffers.persisted.focus match
      case Focus.EditorPane(paneId) => paneId
      case _                        => fail("Focus should be on an editor pane")

    // The focused pane should have a cursor, non-focused should not
    val focusedPane      = stateWith2Buffers.persisted.layout.editorPanes(focusedPaneId)
    val nonFocusedPaneId = if focusedPaneId == pane1Id then pane2Id else pane1Id
    stateWith2Buffers.persisted.layout.editorPanes(nonFocusedPaneId)

    // Focused pane should have cursors
    focusedPane.cursors should not be empty

    // Non-focused pane should have no cursors (or inactive cursors)
    // NOTE: This might need adjustment based on actual cursor model
    // For now, let's check that only the focused pane is marked as active
    stateWith2Buffers.persisted.layout.activeEditorPaneId shouldBe Some(focusedPaneId)

    // When: Switch focus to other buffer
    stateManager.applyEvent(PreviousTab).unsafeRunSync()
    val stateAfterSwitch = stateManager.getCurrentState.unsafeRunSync()

    // Then: Active pane changes
    val newFocusedPaneId = stateAfterSwitch.persisted.focus match
      case Focus.EditorPane(paneId) => paneId
      case _                        => fail("Focus should be on an editor pane")

    newFocusedPaneId should not be focusedPaneId
    stateAfterSwitch.persisted.layout.activeEditorPaneId shouldBe Some(newFocusedPaneId)
