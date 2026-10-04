package com.serenity.state.manager

import scala.util.Random

import com.serenity.keystroke.events.{InsertChar, ResizeEvent}
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, EditorPaneComponent}
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{
  LayoutEngine,
  ViewportSize,
  VisualRowCounts,
  WorkspaceNode,
  WorkspaceNodeId,
  WorkspaceTree,
  WrappedLineCache
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

/** Typing inside a long wrapped paragraph must leave the cursor on the same screen row -- the centred one -- after
  * every keystroke, and on the row a cold layout of the same state would centre it on (#1978).
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
    val settled = start.copy(persisted =
      start.persisted.copy(buffers =
        start.persisted.buffers + (bufferId -> buffer(start).copy(viewport = coldViewport(start)))
      )
    )
    val typed       = "the quick brown fox jumps over the lazy dog "
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

  /** The row, counted from the top of the pane, the renderer's own snapshot of `state` puts the cursor on, taken from
    * the shared scene exactly as the window paints it.
    */
  private def paintedCursorRow(state: AppState, caches: RenderCaches): Option[Int] =
    val config = state.persisted.config.editorConfig.fontConfig
    val size   = state.runtime.viewportSize.getOrElse(fail("no viewport size"))
    val scene = caches.authoritativeScene.forState(
      state,
      size,
      FontLoader.previewCodeFont(config),
      FontLoader.previewTextFont(config)
    )
    val b        = buffer(state)
    val snapshot = scene.textSnapshot(paneId).getOrElse(fail("no text snapshot"))
    snapshot.navigationGeometry.visualRowIndexFor(b.editing.cursorPositions.head)

  private def coldCentre(state: AppState): Some[Int] = Some(cursorScreenRow(state, buffer(state).viewport))

  /** Every keystroke painted the cursor on the cold-layout row, and that row never moved. */
  private val centred: Matcher[List[(Option[Int], Some[Int])]] =
    Matcher { rows =>
      val ok = rows.forall((painted, cold) => painted == cold) && rows.map(_._2).distinct.size == 1
      MatchResult(ok, s"painted/cold rows were $rows", "rows were centred")
    }

  private def paintedRows(
    text: String,
    columns: Int,
    typewriter: Boolean,
    fraction: Double,
    line: Int,
    keystrokes: Int
  ): List[(Option[Int], Some[Int])] =
    val lineLength = text.split("\n", -1)(line).length
    val caches     = RenderCaches.create()
    val cache      = caches.wrappedLines
    val raw        = stateWith(text, CursorPosition(line, (lineLength * fraction).toInt), columns, typewriter)
    val size       = raw.runtime.viewportSize.getOrElse(fail("no viewport size"))
    val layout     = LayoutEngine.calculateLayoutWithUI(raw, size)
    val rect       = LayoutEngine.calculateEditorPaneLayouts(raw, layout)(paneId).contentRect
    val sized      = buffer(raw).copy(viewport = LayoutEngine.updateBufferViewportDimensions(buffer(raw), rect, true))
    val start      = raw.copy(persisted = raw.persisted.copy(buffers = Map(bufferId -> sized)))
    val settled = start.copy(persisted =
      start.persisted.copy(buffers = start.persisted.buffers + (bufferId -> sized.copy(viewport = coldViewport(start))))
    )
    val typed = "the quick brown fox jumps over the lazy dog "
    (0 until keystrokes)
      .scanLeft((settled, (paintedCursorRow(settled, caches), coldCentre(settled)))) {
        case ((state, _), index) =>
          val next = typeChar(cache, typed(index % typed.length))(state)
          (next, (paintedCursorRow(next, caches), coldCentre(next)))
      }
      .map(_._2)
      .toList

  "The painted cursor row" should "stay on one row while typing mid-paragraph in the bench lorem document" in {
    for
      columns    <- List(50, 64, 80, 100, 117, 140)
      typewriter <- List(false, true)
      fraction   <- List(0.0, 0.5, 0.95)
    do
      withClue(s"columns=$columns typewriter=$typewriter fraction=$fraction: ")(
        paintedRows(loremDocument, columns, typewriter, fraction, 300, 150) should centred
      )
  }

  it should "stay on the cold-layout centre row inside one very long wrapped paragraph" in {
    val random    = new Random(7)
    val paragraph = Vector.fill(3_000)(words(random.nextInt(words.length))).mkString(" ")
    val text      = Vector("Heading", paragraph, "Tail").mkString("\n")
    for typewriter <- List(false, true) do
      val rows = paintedRows(text, columns = 40, typewriter, fraction = 0.5, line = 1, keystrokes = 100)
      withClue(s"typewriter=$typewriter: ")(rows should centred)
  }

  private def paintedSnapshot(state: AppState, caches: RenderCaches) =
    val config = state.persisted.config.editorConfig.fontConfig
    caches.authoritativeScene
      .forState(
        state,
        state.runtime.viewportSize.getOrElse(fail("no viewport size")),
        FontLoader.previewCodeFont(config),
        FontLoader.previewTextFont(config)
      )
      .textSnapshot(paneId)
      .getOrElse(fail("no text snapshot"))

  /** A cursor deep inside a long wrapped paragraph, placed centred, so `topVisualLine` is a large offset. */
  private def placedInLongParagraph(columns: Int, trailingLines: Vector[String]): AppState =
    val random    = new Random(11)
    val paragraph = Vector.fill(3_000)(words(random.nextInt(words.length))).mkString(" ")
    val text      = (Vector("Heading", paragraph) ++ trailingLines).mkString("\n")
    val raw       = stateWith(text, CursorPosition(1, paragraph.length / 2), columns, typewriter = false)
    val synced =
      com.serenity.state.reducers.SystemEventReducer.reduce(ResizeEvent(raw.runtime.viewportSize.get), raw).state
    val placed = buffer(synced).copy(viewport = coldViewport(synced))
    synced.copy(persisted = synced.persisted.copy(buffers = Map(bufferId -> placed)))

  private def resizeSizes =
    List(ViewportSize(50, 16), ViewportSize(140, 44), ViewportSize(70, 30), ViewportSize(36, 24))

  "Resizing a pane showing a cursor deep in a long wrapped paragraph" should
    "paint a full pane of rows and centre the cursor once resized" in {
      List(Vector("Tail"), Vector.empty[String]).foreach(resizeThroughSizes)
      succeed
    }

  private def resizeThroughSizes(trailingLines: Vector[String]): Unit =
    resizeSizes.foldLeft(placedInLongParagraph(60, trailingLines)) { (before, size) =>
      val resized = EventPipelineTransitions.resized(ResizeEvent(size), before).state
      withClue(s"resized to $size: ") {
        // The resize commits already placed, so no frame paints the stale offset: against a pane that grew past
        // the document's end that offset paints no rows at all (#1978).
        paintedSnapshot(resized, RenderCaches.create()).visualLines.length should be > 0
        // Placement on resize leaves the cursor on the row a cold layout centres it on.
        paintedCursorRow(resized, RenderCaches.create()) shouldBe coldCentre(resized)
        buffer(resized).viewport shouldBe coldViewport(resized)
      }
      resized
    }
    ()
