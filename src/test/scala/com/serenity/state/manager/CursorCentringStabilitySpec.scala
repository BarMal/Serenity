package com.serenity.state.manager

import com.serenity.keystroke.events.InsertChar
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, EditorPaneComponent}
import com.serenity.state.models.*
import com.serenity.ui.layout.{
  ViewportSize,
  VisualRowCounts,
  WorkspaceNode,
  WorkspaceNodeId,
  WorkspaceTree,
  WrappedLineCache
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

/** Typing inside a long wrapped paragraph must leave the cursor on the same screen row -- the centred one -- after every
  * keystroke, and on the row a cold layout of the same state would centre it on (#1978).
  */
class CursorCentringStabilitySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private val words =
    ("lorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore " +
      "magna aliqua ut enim ad minim veniam quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo " +
      "consequat duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla " +
      "pariatur excepteur sint occaecat cupidatat non proident sunt in culpa qui officia deserunt mollit anim id est " +
      "laborum").split(" ").toVector

  /** The shape of `bench/gen-lorem.py`: 300 paragraphs of 4-8 sentences of 8-20 words. */
  private def loremDocument: String =
    val random = new Random(42)
    Vector
      .fill(300) {
        Vector
          .fill(4 + random.nextInt(5)) {
            val sentence = Vector.fill(8 + random.nextInt(13))(words(random.nextInt(words.length))).mkString(" ")
            sentence.capitalize + "."
          }
          .mkString(" ")
      }
      .mkString("\n\n")

  private def stateWith(text: String, cursor: CursorPosition, columns: Int, typewriter: Boolean): AppState =
    val buffer = Buffer
      .fromString(bufferId, text)
      .copy(
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = columns, visibleLines = 24),
        editing = EditingState(List(cursor))
      )
    val base = AppState.initial
    val surface =
      base.persisted.config.surfaceConfig.copy(wordWrapEnabled = true, typewriterScrollingEnabled = typewriter)
    base.copy(
      persisted = base.persisted.copy(
        config = base.persisted.config.withSurfaceConfig(surface),
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = base.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId)
      ),
      runtime = base.runtime.copy(viewportSize = Some(ViewportSize(columns + 10, 30)))
    )

  private def buffer(state: AppState): Buffer = state.persisted.buffers(bufferId)

  private def typeChar(cache: WrappedLineCache, char: Char)(state: AppState): AppState =
    new EditorPaneComponent(paneId, cache).processEvent(InsertChar(char), state) match
      case ComponentResult.ReducerUpdate(update) => CursorViewport.ensureVisibleCursors(state, update.state, cache)
      case other                                 => fail(s"typing was not reduced: $other")

  /** The viewport a cache-free (cold) layout of `state` places the cursor with. */
  private def coldViewport(state: AppState): Viewport =
    val b = buffer(state)
    CursorViewport.adjustForCursor(b, state, b.editing.cursorPositions.head, WrappedLineCache.Uncached)

  /** Screen rows between the viewport top and the cursor's row, both measured cold. */
  private def cursorScreenRow(state: AppState, viewport: Viewport): Int =
    val b      = buffer(state)
    val cursor = b.editing.cursorPositions.head
    val config = state.persisted.config.editorConfig.fontConfig
    val font   = com.serenity.ui.fonts.FontLoader.previewFontForRole(config, b.typographyRole)
    val width  = com.serenity.ui.layout.TextLayoutSnapshot.gridWrapWidthPx(viewport.visibleColumns, config)
    val rows   = VisualRowCounts.forBuffer(b, width, font, None, false, WrappedLineCache.Uncached)
    val cursorRow = com.serenity.ui.layout.TextLayoutSnapshot.visualLineIndexForCursor(
      b.document.content.getLine(cursor.line).getOrElse(""),
      cursor.column,
      width,
      font,
      rowAffinity = cursor.rowAffinity
    )
    rows.rowsBetween(viewport.topLine, cursor.line) + cursorRow - viewport.topVisualLine

  private def typeMidParagraph(text: String, line: Int, columns: Int, typewriter: Boolean, keystrokes: Int): Unit =
    val lineLength = text.split("\n", -1)(line).length
    val cache      = WrappedLineCache.bounded()
    val start      = stateWith(text, CursorPosition(line, lineLength / 2), columns, typewriter)
    val settled    = start.copy(persisted =
      start.persisted.copy(buffers = start.persisted.buffers + (bufferId -> buffer(start).copy(viewport = coldViewport(start))))
    )
    val typed = "the quick brown fox jumps over the lazy dog "
    val expectedRow = cursorScreenRow(settled, buffer(settled).viewport)
    (0 until keystrokes).foldLeft(settled) { (state, index) =>
      val next = typeChar(cache, typed(index % typed.length))(state)
      withClue(s"keystroke ${index + 1} (columns=$columns typewriter=$typewriter): ") {
        buffer(next).viewport shouldBe coldViewport(next)
        cursorScreenRow(next, buffer(next).viewport) shouldBe expectedRow
      }
      next
    }
    ()

  "Typing mid-paragraph in the bench lorem document" should "keep the cursor on its cold-layout centre row" in {
    for
      columns    <- List(60, 80, 100, 120)
      typewriter <- List(false, true)
    do typeMidParagraph(loremDocument, line = 150 * 2, columns, typewriter, keystrokes = 120)
  }
