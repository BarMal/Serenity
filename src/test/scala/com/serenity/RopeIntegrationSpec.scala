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

  it should "maintain rope balance during intensive editing" in new RopeIntegrationFixture:
    // Given: Large text content that will create an unbalanced rope
    val largeText = (1 to 100).map(i => s"Line $i with some content").mkString("\n")
    val rope      = Rope(largeText)
    val buffer    = Buffer(BufferId(1), Document(rope, isDirty = false, filePath = None))

    // When: Perform many insertions that could unbalance the rope
    val insertions = (1 to 50).map(i => (i * 10, s" [Insert $i]"))

    val currentBuffer = insertions.foldLeft(buffer) {
      case (buf, (position, text)) =>
        val newContent = buf.document.content.insert(position, text).getOrElse(fail("expected insert to succeed"))
        buf.copy(document = buf.document.copy(content = newContent))
    }

    // Then: Rope should remain balanced or be rebalanceable
    val finalRope = currentBuffer.document.content
    finalRope.isHeightBalanced shouldBe true
    finalRope.isDepthBalanced shouldBe true

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

  it should "handle large-scale deletions efficiently" in new RopeIntegrationFixture:
    // Given: Large document
    val lines     = (1 to 1000).map(i => s"Line $i: Lorem ipsum dolor sit amet, consectetur adipiscing elit.")
    val largeText = lines.mkString("\n")
    val rope      = Rope(largeText)

    // When: Delete large sections (simulating large backspace operations)
    val deletions = List(
      (1000, 2000),  // Delete 1000 characters
      (5000, 6000),  // Delete another 1000 characters
      (10000, 15000) // Delete 5000 characters
    )

    val currentRope = deletions.foldLeft(rope) {
      case (currentRope, (start, end)) =>
        if start < currentRope.weight && end <= currentRope.weight then
          val newRope = currentRope.deleteLeft(end, end - start)
          // Verify rope remains valid after deletion
          newRope.isHeightBalanced shouldBe true
          newRope
        else currentRope
    }

    // Then: Rope should maintain integrity
    currentRope.weight should be > 0
    currentRope.collect() should not be empty

  it should "maintain cursor position accuracy during rope modifications" in new RopeIntegrationFixture:
    // Given: Multi-line text with known structure
    val lines = List(
      "First line of text",
      "Second line with more content",
      "Third line",
      "Fourth and final line"
    )
    val text = lines.mkString("\n")
    val rope = Rope(text)

    // When: Test cursor position calculations
    val testPositions = List(
      (0, 0, 0),   // Start of first line
      (0, 5, 5),   // Middle of first line
      (1, 0, 19),  // Start of second line (18 chars + newline)
      (1, 10, 29), // Middle of second line
      (2, 0, 49),  // Start of third line
      (3, 4, 64)   // Middle of fourth line
    )

    testPositions.foreach {
      case (line, column, expectedOffset) =>
        val calculatedOffset = lineColumnToOffset(rope, line, column)
        calculatedOffset shouldBe expectedOffset

        // Verify reverse calculation
        val char = rope.index(calculatedOffset)
        char should be(defined)
    }

  it should "handle rope search operations for editor functionality" in new RopeIntegrationFixture:
    // Given: Text with repeated patterns
    val text = """function findText(query) {
  const results = [];
  for (let i = 0; i < text.length; i++) {
    if (text.substring(i).startsWith(query)) {
      results.push(i);
    }
  }
  return results;
}
function processText(input) {
  return findText(input);
}"""
    val rope = Rope(text)

    // When: Search for various patterns
    val searchTerms = List(
      ("function", List(0, 191)),      // Should find both function declarations
      ("text", List(71, 99)),          // Should find multiple text occurrences
      ("results", List(35, 144, 180)), // Should find results variable
      ("nonexistent", List())          // Should find nothing
    )

    searchTerms.foreach {
      case (term, expectedPositions) =>
        val foundPositions = rope.searchAll(term)

        // Then: Search should find all occurrences
        foundPositions should contain theSameElementsAs expectedPositions

        // Verify each found position is correct
        foundPositions.foreach { position =>
          val substring = rope.collect().substring(position, position + term.length)
          substring shouldBe term
        }
    }

  it should "return non-overlapping matches for searchAll" in new RopeIntegrationFixture:
    Rope("aaaa").searchAll("aa") shouldBe List(0, 2)

  it should "name out-of-bounds leaf insertions as a failure instead of silently ignoring them" in new RopeIntegrationFixture:
    val rope = Rope("abc")

    rope.insert(-1, "x") shouldBe None
    rope.insert(4, "x") shouldBe None

  it should "handle rope replace operations for find-and-replace functionality" in new RopeIntegrationFixture:
    // Given: Code with variables to rename
    val code = """let oldName = 5;
function test() {
  console.log(oldName);
  return oldName + 1;
}
let anotherOldName = oldName * 2;"""
    val rope = Rope(code)

    // When: Replace all occurrences of variable name
    val replacedRope = rope.replaceAll("oldName", "newVariableName")

    // Then: All occurrences should be replaced
    val result = replacedRope.collect()
    result should include("let newVariableName = 5;")
    result should include("console.log(newVariableName);")
    result should include("return newVariableName + 1;")
    result should include("let anotherOldName = newVariableName * 2;") // Only exact matches replaced

    // Verify rope structure remains valid
    replacedRope.isHeightBalanced shouldBe true
    replacedRope.isWeightBalanced shouldBe true

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

    def lineColumnToOffset(rope: Rope, line: Int, column: Int): Int =
      val lines     = rope.collect().split('\n')
      val lineStart = lines.take(line).map(_.length + 1).sum // Sum of all previous lines + newlines
      Math.min(lineStart + column, rope.weight)

    def createTestAppState(buffers: Map[BufferId, Buffer], panes: Map[PaneId, EditorPane]): AppState =
      import com.serenity.ui.layout.Layout

      val layout = Layout(
        editorPanes = panes,
        activeEditorPaneId = panes.keys.headOption
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
