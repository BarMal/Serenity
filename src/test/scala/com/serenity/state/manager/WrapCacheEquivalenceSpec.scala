package com.serenity.state.manager

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree, WrappedLineCache}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The state-update path's measurements -- viewport placement and the navigation geometry window -- must not change
  * when they read wrapped lines through a shared [[WrappedLineCache]].
  */
class WrapCacheEquivalenceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private val paragraphs = Vector.tabulate(60) { index =>
    if index % 2 == 1 then ""
    else ("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor. " * (1 + index % 7)).trim
  }

  private def stateWith(cursor: CursorPosition, typewriter: Boolean = false): AppState =
    val buffer = Buffer
      .fromString(bufferId, paragraphs.mkString("\n"))
      .copy(
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 60, visibleLines = 20),
        editing = EditingState(List(cursor))
      )
    val base = AppState.initial
    val surface =
      base.persisted.config.surfaceConfig.copy(wordWrapEnabled = true, typewriterScrollingEnabled = typewriter)
    base.copy(
      persisted = base.persisted.copy(
        config = base.persisted.config.copy(surfaceConfig = surface),
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = base.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId)
      ),
      runtime = base.runtime.copy(viewportSize = Some(ViewportSize(70, 24)))
    )

  private val cursors =
    List(CursorPosition(0, 0), CursorPosition(10, 40), CursorPosition(30, 300), CursorPosition(56, 5)) ++
      List(CursorPosition(58, 200), CursorPosition(59, 0))

  "CursorViewport.adjustForCursor" should "place the viewport identically with and without a wrap cache" in {
    val cache = WrappedLineCache.bounded()
    for
      typewriter <- List(false, true)
      cursor     <- cursors
    do
      val state  = stateWith(cursor, typewriter)
      val buffer = state.persisted.buffers(bufferId)
      val placed = CursorViewport.adjustForCursor(buffer, state, cursor)
      withClue(s"cursor $cursor, typewriter $typewriter: ") {
        CursorViewport.adjustForCursor(buffer, state, cursor, cache) shouldBe placed
        CursorViewport.adjustForCursor(buffer, state, cursor, cache) shouldBe placed
      }
  }

  it should "still bottom-align a cursor on the document's last screen" in {
    val state  = stateWith(CursorPosition(59, 0))
    val buffer = state.persisted.buffers(bufferId)
    val placed = CursorViewport.adjustForCursor(buffer, state, CursorPosition(59, 0), WrappedLineCache.bounded())
    val centred =
      CursorViewport.adjustForCursor(buffer, stateWith(CursorPosition(59, 0), typewriter = true), CursorPosition(59, 0))
    (placed.topLine, placed.topVisualLine) should not be ((centred.topLine, centred.topVisualLine))
  }

  "CursorViewport.ensureVisibleCursors" should "produce the same state with and without a wrap cache" in {
    val cache = WrappedLineCache.bounded()
    cursors.sliding(2).foreach {
      case List(from, to) =>
        val before = stateWith(from)
        val after  = stateWith(to)
        CursorViewport.ensureVisibleCursors(before, after, cache) shouldBe
          CursorViewport.ensureVisibleCursors(before, after)
      case _ => ()
    }
  }

  "EditorGeometryProducer.forPane" should "build the same navigation geometry with and without a wrap cache" in {
    val cache = WrappedLineCache.bounded()
    cursors.foreach { cursor =>
      val state = stateWith(cursor)
      withClue(s"cursor $cursor: ") {
        EditorGeometryProducer.forPane(state, paneId, wrapCache = cache) shouldBe
          EditorGeometryProducer.forPane(state, paneId)
        EditorGeometryProducer.forPane(state, paneId, rowsAbove = 20, wrapCache = cache) shouldBe
          EditorGeometryProducer.forPane(state, paneId, rowsAbove = 20)
      }
    }
  }
