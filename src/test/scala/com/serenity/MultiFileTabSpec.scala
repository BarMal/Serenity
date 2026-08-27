package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.*
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class MultiFileTabSpec extends AnyFlatSpec with Matchers:

  given balance: Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Multi-Buffer Tab Management with Layout Engine"

  it should "open multiple files in different tabs" in new MultiFileFixture:
    // Given: Create multiple buffers to simulate different "tabs"
    val buffer1 =
      stateManager.createBuffer("Content of file 1", Some(java.nio.file.Paths.get("file1.txt"))).unsafeRunSync()
    val buffer2 =
      stateManager.createBuffer("Content of file 2", Some(java.nio.file.Paths.get("file2.txt"))).unsafeRunSync()
    val buffer3 =
      stateManager.createBuffer("Content of file 3", Some(java.nio.file.Paths.get("file3.txt"))).unsafeRunSync()

    // When: Check that all buffers exist in state
    val state = stateManager.getCurrentState.unsafeRunSync()

    // Then: All buffers should be available
    state.persisted.buffers.should(have).size(4) // Initial buffer + 3 created buffers
    state.persisted.buffers.keys.should(contain).allOf(buffer1, buffer2, buffer3)

    // And their content should be accessible
    state.persisted.buffers(buffer1).document.content.collect().shouldBe("Content of file 1")
    state.persisted.buffers(buffer2).document.content.collect().shouldBe("Content of file 2")
    state.persisted.buffers(buffer3).document.content.collect().shouldBe("Content of file 3")

    // And they should have the correct file paths
    state.persisted.buffers(buffer1).document.filePath.shouldBe(Some(java.nio.file.Paths.get("file1.txt")))
    state.persisted.buffers(buffer2).document.filePath.shouldBe(Some(java.nio.file.Paths.get("file2.txt")))
    state.persisted.buffers(buffer3).document.filePath.shouldBe(Some(java.nio.file.Paths.get("file3.txt")))

  it should "switch between tabs correctly" in new MultiFileFixture:
    // Given: Create multiple buffers and get the default pane
    val buffer1 = stateManager.createBuffer("First buffer content").unsafeRunSync()
    val buffer2 = stateManager.createBuffer("Second buffer content").unsafeRunSync()
    val state   = stateManager.getCurrentState.unsafeRunSync()
    val paneId  = state.persisted.layout.editorPanes.keys.head

    // When: Switch between buffers in the pane (simulating tab switching)
    stateManager.setBufferForPane(paneId, buffer1).unsafeRunSync()
    val stateWithBuffer1 = stateManager.getCurrentState.unsafeRunSync()

    stateManager.setBufferForPane(paneId, buffer2).unsafeRunSync()
    val stateWithBuffer2 = stateManager.getCurrentState.unsafeRunSync()

    stateManager.setBufferForPane(paneId, buffer1).unsafeRunSync()
    val backToBuffer1 = stateManager.getCurrentState.unsafeRunSync()

    // Then: The active buffer should change correctly
    stateWithBuffer1.persisted.layout.editorPanes(paneId).bufferId.shouldBe(Some(buffer1))
    stateWithBuffer2.persisted.layout.editorPanes(paneId).bufferId.shouldBe(Some(buffer2))
    backToBuffer1.persisted.layout.editorPanes(paneId).bufferId.shouldBe(Some(buffer1))

    // And the content should be accessible in each state
    stateWithBuffer1.persisted.buffers(buffer1).document.content.collect().shouldBe("First buffer content")
    stateWithBuffer2.persisted.buffers(buffer2).document.content.collect().shouldBe("Second buffer content")

  it should "close tabs without affecting other tabs" in new MultiFileFixture:
    // Given: Create multiple buffers
    val buffer1      = stateManager.createBuffer("First buffer").unsafeRunSync()
    val buffer2      = stateManager.createBuffer("Second buffer").unsafeRunSync()
    val buffer3      = stateManager.createBuffer("Third buffer").unsafeRunSync()
    val initialState = stateManager.getCurrentState.unsafeRunSync()

    // Verify all buffers exist
    initialState.persisted.buffers.should(have).size(4) // Initial buffer + 3 created buffers

    // When: "Close" one buffer by removing it from state
    stateManager.closeBuffer(buffer2).unsafeRunSync()
    val stateAfterClose = stateManager.getCurrentState.unsafeRunSync()

    // Then: Only the specified buffer should be removed
    stateAfterClose.persisted.buffers.should(have).size(3) // Initial buffer + 2 remaining created buffers
    stateAfterClose.persisted.buffers.keys.should(contain).allOf(buffer1, buffer3)
    stateAfterClose.persisted.buffers.keys.should(not).contain(buffer2)

    // And the remaining buffers should be unaffected
    stateAfterClose.persisted.buffers(buffer1).document.content.collect().shouldBe("First buffer")
    stateAfterClose.persisted.buffers(buffer3).document.content.collect().shouldBe("Third buffer")

  it should "handle closing tab with unsaved changes" in new MultiFileFixture:
    // Given: Create a tab with content and make changes
    val initialState = stateManager.getCurrentState.unsafeRunSync()
    val paneId       = initialState.persisted.layout.editorPanes.keys.head

    // Create buffer with file path and content, then modify it
    val bufferId =
      stateManager.createBuffer("Original content", Some(java.nio.file.Paths.get("/tmp/test.txt"))).unsafeRunSync()
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.setCursorPosition(paneId, 0, "Original content".length).unsafeRunSync()
    stateManager.applyEvent(InsertChar('!')).unsafeRunSync()

    val modifiedState = stateManager.getCurrentState.unsafeRunSync()
    val buffer        = modifiedState.persisted.buffers(bufferId)

    // Then: Buffer should be dirty with unsaved changes
    buffer.document.isDirty.shouldBe(true)
    buffer.document.content.collect().shouldBe("Original content!")
    buffer.document.filePath.shouldBe(Some(java.nio.file.Paths.get("/tmp/test.txt")))

    // When: Ctrl+W attempts to close tab with unsaved changes
    stateManager.applyEvent(CloseTab).unsafeRunSync()
    val stateAfterClose = stateManager.getCurrentState.unsafeRunSync()

    // Then: A close workflow should intercept the hotkey and keep the buffer open
    stateAfterClose.persisted.buffers should contain key bufferId
    stateAfterClose.modalSurface.flatMap(_.content match
      case SurfaceContent.ModalWorkflow(Modal.CloseWorkflow(workflow)) => Some(workflow.currentBufferId)
      case _                                                           => None) shouldBe Some(bufferId)

  it should "maintain tab order when adding and removing tabs" in new MultiFileFixture:
    // Given: Create multiple buffers in sequence
    val buffer1 =
      stateManager.createBuffer("First tab content", Some(java.nio.file.Paths.get("file1.txt"))).unsafeRunSync()
    val buffer2 =
      stateManager.createBuffer("Second tab content", Some(java.nio.file.Paths.get("file2.txt"))).unsafeRunSync()
    val buffer3 =
      stateManager.createBuffer("Third tab content", Some(java.nio.file.Paths.get("file3.txt"))).unsafeRunSync()
    val buffer4 =
      stateManager.createBuffer("Fourth tab content", Some(java.nio.file.Paths.get("file4.txt"))).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()

    // When: Check that buffers are accessible in creation order
    val bufferIds     = state.persisted.buffers.keys.toList.sorted
    val expectedOrder = List(BufferId(0), buffer1, buffer2, buffer3, buffer4).sorted // Include initial buffer

    // Then: Buffer IDs should be in the expected order (since BufferId is likely ordered by creation)
    bufferIds.shouldBe(expectedOrder)

    // When: Remove the middle buffer (buffer2)
    stateManager.closeBuffer(buffer2).unsafeRunSync()
    val stateAfterRemoval = stateManager.getCurrentState.unsafeRunSync()

    // Then: Remaining buffers should maintain their relative order
    val remainingIds      = stateAfterRemoval.persisted.buffers.keys.toList.sorted
    val expectedRemaining = List(BufferId(0), buffer1, buffer3, buffer4).sorted // Include initial buffer

    remainingIds.shouldBe(expectedRemaining)
    stateAfterRemoval.persisted.buffers.should(have).size(4) // Initial buffer + 3 remaining created buffers

  it should "handle tab switching with keyboard shortcuts" in new MultiFileFixture:
    // Given: Wide terminal to allow multiple panes
    stateManager
      .updateState(state => state.copy(runtime = state.runtime.copy(viewportSize = Some(wideTerminal))))
      .unsafeRunSync()
    val initialState = stateManager.getCurrentState.unsafeRunSync()
    initialState.persisted.layout.editorPanes should have size 1
    initialState.persisted.buffers should have size 1 // Initial buffer
    initialState.persisted.bufferOrder should have size 1
    val initialBufferId = initialState.persisted.bufferOrder.head

    // When: Ctrl+T creates new buffer (using buffer-based navigation)
    stateManager.applyEvent(NewTab).unsafeRunSync()
    val singlePaneState = stateManager.getCurrentState.unsafeRunSync()
    val originalPaneId  = singlePaneState.persisted.layout.activeEditorPaneId.get
    stateManager.splitPaneHorizontal(originalPaneId, Some(initialBufferId)).unsafeRunSync()
    stateManager.switchToPane(originalPaneId).unsafeRunSync()
    val stateAfterNewTab = stateManager.getCurrentState.unsafeRunSync()

    // Then: The explicit split gives both buffers persistent views.
    stateAfterNewTab.persisted.buffers should have size 2
    stateAfterNewTab.persisted.bufferOrder should have size 2
    stateAfterNewTab.persisted.layout.editorPanes should have size 2

    val newBufferId = stateAfterNewTab.persisted.bufferOrder.last
    stateAfterNewTab.focusedBufferId.get shouldBe newBufferId

    // The new buffer should be empty
    stateAfterNewTab.persisted.buffers(newBufferId).document.content.collect() shouldBe ""

    // When: Ctrl+Tab switches to next buffer (cycles through buffer order)
    stateManager.applyEvent(NextTab).unsafeRunSync()
    val stateAfterNext = stateManager.getCurrentState.unsafeRunSync()

    // Then: Focus should change to the initial buffer
    stateAfterNext.focusedBufferId.get shouldBe initialBufferId

    // When: Ctrl+Shift+Tab switches to previous buffer
    stateManager.applyEvent(PreviousTab).unsafeRunSync()
    val stateAfterPrev = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should cycle back to the new buffer
    stateAfterPrev.focusedBufferId.get shouldBe newBufferId

  it should "handle splitting panes for same file" in new MultiFileFixture:
    // Given: A buffer with content
    val bufferId       = stateManager.createBuffer("Shared content between panes").unsafeRunSync()
    val state          = stateManager.getCurrentState.unsafeRunSync()
    val originalPaneId = state.persisted.layout.editorPanes.keys.head

    // Set the buffer in the original pane
    stateManager.setBufferForPane(originalPaneId, bufferId).unsafeRunSync()

    // When: Create a new pane (simulating Ctrl+T creating a new pane)
    // For now, we'll simulate this by creating a new pane manually
    val newPaneId = stateManager.createPane().unsafeRunSync()

    // Set the same buffer in the new pane
    stateManager.setBufferForPane(newPaneId, bufferId).unsafeRunSync()

    val finalState = stateManager.getCurrentState.unsafeRunSync()

    // Then: Both panes should exist and reference the same buffer
    finalState.persisted.layout.editorPanes should have size 2
    finalState.persisted.layout.editorPanes.keys should contain allOf (originalPaneId, newPaneId)

    // And both panes should reference the same buffer
    finalState.persisted.layout.editorPanes(originalPaneId).bufferId shouldBe Some(bufferId)
    finalState.persisted.layout.editorPanes(newPaneId).bufferId shouldBe Some(bufferId)

    // And the buffer content should be accessible from both panes
    finalState.persisted.buffers(bufferId).document.content.collect() shouldBe "Shared content between panes"

  it should "handle pane resizing with minimum width constraints" in new MultiFileFixture:
    // Given: Limited terminal width where only 1-2 panes can fit
    val constrainedTerminal = com.serenity.ui.layout.ViewportSize(120, 24) // About 100 chars for editor
    stateManager
      .updateState(state => state.copy(runtime = state.runtime.copy(viewportSize = Some(constrainedTerminal))))
      .unsafeRunSync()

    val initialState = stateManager.getCurrentState.unsafeRunSync()
    initialState.persisted.layout.editorPanes should have size 1
    initialState.persisted.buffers should have size 1

    // When: Create multiple buffers using Ctrl+T
    stateManager.applyEvent(NewTab).unsafeRunSync() // Buffer 2
    stateManager.applyEvent(NewTab).unsafeRunSync() // Buffer 3
    val stateWith3Buffers = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should have 3 buffers but layout engine limits panes based on width
    stateWith3Buffers.persisted.buffers should have size 3
    stateWith3Buffers.persisted.bufferOrder should have size 3

    // With constrained width, should have fewer panes than buffers
    stateWith3Buffers.persisted.layout.editorPanes.size should be <= 2

    // When: Create a 4th buffer (more buffers than can be displayed)
    stateManager.applyEvent(NewTab).unsafeRunSync()
    val stateWith4Buffers = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should have 4 buffers in memory, but still limited panes
    stateWith4Buffers.persisted.buffers should have size 4
    stateWith4Buffers.persisted.bufferOrder should have size 4
    stateWith4Buffers.persisted.layout.editorPanes.size should be <= 2

    // The focused buffer should still be accessible via navigation
    val focusedBufferId = stateWith4Buffers.focusedBufferId.get
    stateWith4Buffers.persisted.bufferOrder should contain(focusedBufferId)

    // Navigation should work to access all buffers regardless of pane count
    val allBufferIds = stateWith4Buffers.persisted.bufferOrder

    // Navigate through all buffers to ensure they're accessible
    val visitedBuffers = (1 until allBufferIds.size).foldLeft(Set(focusedBufferId)) { (visited, _) =>
      stateManager.applyEvent(NextTab).unsafeRunSync()
      val state = stateManager.getCurrentState.unsafeRunSync()
      visited + state.focusedBufferId.get
    }

    // Should have visited all buffers through navigation
    visitedBuffers shouldBe allBufferIds.toSet

  it should "preserve tab state across sessions (simulated)" in new MultiFileFixture:
    // Given: Create buffers with file paths (simulating open files)
    stateManager.createBuffer("File 1 content", Some(java.nio.file.Paths.get("/tmp/file1.txt"))).unsafeRunSync()
    stateManager.createBuffer("File 2 content", Some(java.nio.file.Paths.get("/tmp/file2.txt"))).unsafeRunSync()
    stateManager.createBuffer("Untitled buffer", None).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()

    // When: Simulate session state capture
    val sessionData =
      state.persisted.buffers.values
        .map(buffer => (buffer.document.filePath, buffer.document.content.collect(), buffer.document.isDirty))
        .toList

    // Then: Session data should contain all buffer information needed for restoration
    sessionData should have size 4 // Initial buffer + 2 file buffers + 1 untitled buffer

    val fileBuffers     = sessionData.filter(_._1.isDefined)
    val untitledBuffers = sessionData.filter(_._1.isEmpty)

    fileBuffers should have size 2
    untitledBuffers should have size 2 // Initial buffer + created untitled buffer

    // File buffers should have their paths preserved
    fileBuffers.map(_._1.get) should contain allOf (
      java.nio.file.Paths.get("/tmp/file1.txt"),
      java.nio.file.Paths.get("/tmp/file2.txt")
    )

    // Content should be preserved
    sessionData.map(_._2) should contain allOf ("File 1 content", "File 2 content", "Untitled buffer")

    // Dirty state should be captured
    sessionData.foreach {
      case (_, _, isDirty) =>
        isDirty shouldBe false // All buffers should be clean initially
    }

  trait MultiFileFixture:

    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))

    val stateManager: StateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    val wideTerminal = com.serenity.ui.layout.ViewportSize(400, 24) // Wide enough for multiple panes
