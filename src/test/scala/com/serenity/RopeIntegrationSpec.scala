package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.components.EditorPaneComponent
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RopeIntegrationSpec extends AnyFlatSpec with Matchers:

  given balance: Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Rope Data Structure Integration with Editor Operations"

  it should "handle rope splitting efficiently for cursor operations" in new RopeIntegrationFixture:
    // Given: Text with various line lengths
    val text = """Short line
This is a much longer line with lots of content
Another short
Final line with medium length content here""".replace("\r\n", "\n")
    val rope = Rope(text)

    // When: Split at various positions (simulating cursor operations)
    val splitPositions = List(0, 10, 50, 100, text.length)

    splitPositions.foreach { position =>
      val splitResult = rope.splitAt(position)
      splitResult should be(defined)

      splitResult.foreach {
        case (left, right) =>
          // Then: Split should maintain content integrity
          val recombined = left.concat(right)
          recombined.collect() shouldBe text

          // And both parts should be valid ropes
          left.isHeightBalanced shouldBe true
          right.isHeightBalanced shouldBe true
      }
    }

  it should "integrate rope operations with editor component processing" in new RopeIntegrationFixture:
    // Given: Editor state with buffer
    val initialText = "Hello world"
    val buffer = Buffer(BufferId(1), Document(Rope(initialText), isDirty = false, filePath = None)).copy(
      editing = EditingState(cursors = List(CursorPosition(0, 6))), // Position at "world"
      viewport = Viewport(0, 0, 80, 24)
    )
    val pane = EditorPane(
      id = PaneId(1),
      bufferId = Some(BufferId(1)),
      cursors = List.empty,
      viewport = Viewport.default,
      centerLine = 0
    )
    val appState = createTestAppState(Map(BufferId(1) -> buffer), Map(PaneId(1) -> pane))

    val component = EditorPaneComponent(PaneId(1))

    // When: Process text entry events through component
    val events = List(
      DeleteBackward, // Delete space
      InsertChar(','),
      InsertChar(' '),
      InsertChar('b'),
      InsertChar('e'),
      InsertChar('a'),
      InsertChar('u'),
      InsertChar('t'),
      InsertChar('i'),
      InsertChar('f'),
      InsertChar('u'),
      InsertChar('l'),
      InsertChar(' '),
      MoveToEnd,
      InsertChar('!')
    )

    val currentState = events.foldLeft(appState) { (state, event) =>
      val result = component.processEvent(event, state)
      result match
        case com.serenity.state.components.ComponentResult.ReducerUpdate(reducerResult) =>
          reducerResult.state
        case _ => state // No change
    }

    // Then: Final state should reflect all rope operations
    val finalBuffer = currentState.persisted.buffers(BufferId(1))
    finalBuffer.document.content.collect() shouldBe "Hello, beautiful world!"
    finalBuffer.document.isDirty shouldBe true

    // Cursor should be at end
    finalBuffer.editing.cursors.head.column shouldBe 23

  it should "handle rope operations at chunk boundaries" in new RopeIntegrationFixture:
    // Given: Text that will span multiple rope chunks (leafChunkSize = 30)
    val chunk1 = "a" * 28 + "XX" // 30 chars, ends with XX
    val chunk2 = "YY" + "b" * 28 // 30 chars, starts with YY
    val text   = chunk1 + chunk2 // 60 chars total
    val rope   = Rope(text)

    // When: Perform operations at chunk boundary
    val boundaryPosition = 30 // Exactly at chunk boundary

    // Insert at boundary
    val insertedRope = rope.insert(boundaryPosition, "BOUNDARY").getOrElse(fail("expected insert to succeed"))
    insertedRope.collect() shouldBe (chunk1 + "BOUNDARY" + chunk2)

    // Search across boundary
    val searchResults = rope.searchAll("XXYY")
    searchResults should contain(28) // Should find the pattern spanning chunks

    // Delete across boundary
    val deletedRope = rope.deleteLeft(32, 4) // Delete "XXYY"
    deletedRope.collect() shouldBe ("a" * 28 + "b" * 28)

    // Then: All operations should maintain rope integrity
    insertedRope.isHeightBalanced shouldBe true
    deletedRope.isHeightBalanced shouldBe true

  trait RopeIntegrationFixture:

    def createTestAppState(buffers: Map[BufferId, Buffer], panes: Map[PaneId, EditorPane]): AppState =
      import com.serenity.ui.layout.Layout

      val layout = Layout(
        editorPanes = panes,
        activeEditorPaneId = panes.keys.headOption,
        workspaceTree = Some(TestWorkspaceTrees.linear(panes.keys.toList.sortBy(_.value)*))
      )

      AppState(
        persisted = Persisted(
          layout = layout,
          buffers = buffers,
          focus = Focus.EditorPane(panes.keys.head)
        ),
        runtime = Runtime(
          uiSurfaces = Nil,
          nextBufferId = BufferId(buffers.size + 1),
          nextPaneId = PaneId(panes.size + 1)
        )
      )
