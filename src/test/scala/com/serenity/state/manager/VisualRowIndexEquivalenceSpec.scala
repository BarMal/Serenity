package com.serenity.state.manager

import com.serenity.command.NavigationIntent
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, EditorPaneComponent, ModalComponent}
import com.serenity.state.models.*
import com.serenity.state.reducers.{ModalStateReducer, ReducerResult}
import com.serenity.ui.layout.{ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree, WrappedLineCache}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Scrolling, page moves, typewriter centring and jumps read visual-row counts through a cache's per-buffer index.
  * Every placement must match the one walking each line produces (`WrappedLineCache.Uncached` keeps no index), even as
  * one cache carries its index across edits, wrap widths and fonts.
  */
class VisualRowIndexEquivalenceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private val paragraphs = Vector.tabulate(400) { index =>
    if index % 3 == 2 then ""
    else ("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor. " * (1 + index % 9)).trim
  }

  private def stateWith(
    cursor: CursorPosition,
    typewriter: Boolean = false,
    columns: Int = 60,
    columnMode: Boolean = false,
    fontSize: Float = 12.0f,
    text: String = paragraphs.mkString("\n")
  ): AppState =
    val buffer = Buffer
      .fromString(bufferId, text)
      .copy(
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = columns, visibleLines = 20),
        editing = EditingState(List(cursor))
      )
    val base = AppState.initial
    val surface = base.persisted.config.surfaceConfig
      .copy(wordWrapEnabled = true, typewriterScrollingEnabled = typewriter, columnModeEnabled = columnMode)
    val fonts  = base.persisted.config.editorConfig.fontConfig.copy(fontSize = fontSize, textFontSize = fontSize)
    val config = base.persisted.config.withSurfaceConfig(surface).withFontConfig(fonts)
    base.copy(
      persisted = base.persisted.copy(
        config = config,
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

  private def buffer(state: AppState): Buffer = state.persisted.buffers(bufferId)

  private val cursors =
    List(0, 3, 40, 151, 152, 260, 397, 398, 399).flatMap(line =>
      List(CursorPosition(line, 0), CursorPosition(line, 330))
    )

  private def reduced(result: ComponentResult): Option[ReducerResult] =
    result match
      case ComponentResult.ReducerUpdate(update) => Some(update)
      case _                                     => None

  "CursorViewport.adjustForCursor" should "centre and bottom-align identically through the index, typewriter or not" in {
    val cache = WrappedLineCache.bounded()
    for
      typewriter <- List(false, true)
      cursor     <- cursors ++ cursors.reverse
    do
      val state = stateWith(cursor, typewriter)
      withClue(s"cursor $cursor, typewriter $typewriter: ") {
        CursorViewport.adjustForCursor(buffer(state), state, cursor, cache) shouldBe
          CursorViewport.adjustForCursor(buffer(state), state, cursor)
      }
  }

  it should "keep matching once the index has to follow edits to the document" in {
    val cache = WrappedLineCache.bounded()
    val edits = List(
      (150, 10, "inserted words that wrap the paragraph onto another row " * 3),
      (20, 0, "\n\nnew lines above\n"),
      (399, 0, " the end grows"),
      (151, 5, "\n")
    )
    edits.foldLeft(paragraphs.mkString("\n")) {
      case (text, (line, column, inserted)) =>
        val rope   = Buffer.fromString(bufferId, text).document.content
        val offset = rope.lineColumnToOffset(line, column)
        val next   = text.patch(offset, inserted, 0)
        cursors.foreach { cursor =>
          val state = stateWith(cursor, text = next)
          withClue(s"after inserting at $line:$column, cursor $cursor: ") {
            CursorViewport.adjustForCursor(buffer(state), state, cursor, cache) shouldBe
              CursorViewport.adjustForCursor(buffer(state), state, cursor)
          }
        }
        next
    }
  }

  it should "start a fresh index when the wrap width or the font changes" in {
    val cache = WrappedLineCache.bounded()
    for
      (columns, fontSize) <- List((60, 12.0f), (41, 12.0f), (60, 16.0f), (60, 12.0f))
      cursor              <- cursors
    do
      val state = stateWith(cursor, columns = columns, fontSize = fontSize)
      withClue(s"$columns columns at ${fontSize}pt, cursor $cursor: ") {
        CursorViewport.adjustForCursor(buffer(state), state, cursor, cache) shouldBe
          CursorViewport.adjustForCursor(buffer(state), state, cursor)
      }
  }

  "CursorViewport.adjustForCursorColumnMode" should "anchor column pages identically through the index" in {
    val cache = WrappedLineCache.bounded()
    cursors.foldLeft(stateWith(CursorPosition(0, 0), columnMode = true)) { (previous, cursor) =>
      val state = previous.copy(persisted =
        previous.persisted.copy(buffers = Map(bufferId -> buffer(previous).copy(editing = EditingState(List(cursor)))))
      )
      val placed = CursorViewport.adjustForCursorColumnMode(buffer(state), state, cursor)
      withClue(s"cursor $cursor: ") {
        CursorViewport.adjustForCursorColumnMode(buffer(state), state, cursor, cache) shouldBe placed
      }
      state.copy(persisted = state.persisted.copy(buffers = Map(bufferId -> buffer(state).copy(viewport = placed))))
    }
  }

  "Page Down and Page Up" should "move the cursor and the viewport identically through the index" in {
    val cache = WrappedLineCache.bounded()
    for
      event  <- List[TextEntryEvent](PageDown, PageUp, ExtendSelectionPageDown, ExtendSelectionPageUp, ScrollDown(3))
      cursor <- cursors
    do
      val state = stateWith(cursor)
      withClue(s"$event from $cursor: ") {
        reduced(new EditorPaneComponent(paneId, cache).processEvent(event, state)) shouldBe
          reduced(new EditorPaneComponent(paneId).processEvent(event, state))
      }
  }

  "EditorGeometryProducer.forPane" should "build the same navigation window through the index" in {
    val cache = WrappedLineCache.bounded()
    for
      rowsAbove <- List(4, 20, 60)
      cursor    <- cursors
    do
      val state = stateWith(cursor)
      withClue(s"$rowsAbove rows above $cursor: ") {
        EditorGeometryProducer.forPane(state, paneId, rowsAbove, cache) shouldBe
          EditorGeometryProducer.forPane(state, paneId, rowsAbove)
      }
  }

  "A typewriter keystroke" should "re-centre identically through the index" in {
    val cache = WrappedLineCache.bounded()
    cursors.foreach { cursor =>
      val state = stateWith(cursor, typewriter = true)
      withClue(s"typing at $cursor: ") {
        reduced(new EditorPaneComponent(paneId, cache).processEvent(InsertChar('x'), state)) shouldBe
          reduced(new EditorPaneComponent(paneId).processEvent(InsertChar('x'), state))
      }
    }
  }

  "Go to Line" should "land on the same viewport through the index, by prompt or by command" in {
    val cache = WrappedLineCache.bounded()
    for
      from   <- List(CursorPosition(5, 0), CursorPosition(390, 40))
      target <- List(1, 152, 300, 400)
    do
      val state  = stateWith(from)
      val prompt = ModalStateReducer.show(Modal.TextPrompt(TextPrompt.gotoLine(target.toString)), state).state
      withClue(s"from $from to line $target: ") {
        reduced(ModalComponent(ModalType.TextPrompt, cache).processEvent(ModalSubmit, prompt)) shouldBe
          reduced(ModalComponent(ModalType.TextPrompt).processEvent(ModalSubmit, prompt))
        val goTo = NavigationIntent.GoToBufferLine(bufferId, target - 1)
        NavigationTransitions.navigation(goTo, state, cache) shouldBe NavigationTransitions.navigation(goTo, state)
      }
  }
