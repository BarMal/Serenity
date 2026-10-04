package com.serenity.state.manager

import com.serenity.keystroke.events.InsertChar
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, EditorPaneComponent}
import com.serenity.state.models.*
import com.serenity.ui.layout.{ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree, WrappedLineCache}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Centring the cursor needs the rows within half a viewport of it, so a keystroke must wrap a number of paragraphs
  * bounded by the viewport, however many paragraphs the document holds.
  */
class CursorCentringWorkSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId       = PaneId(0)
  private val bufferId     = BufferId(1)
  private val visibleLines = 20
  private val keystrokes   = 12

  private def paragraph(index: Int): String =
    s"Paragraph $index. " + ("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor. " * (1 + index % 7)).trim

  private def stateWith(paragraphCount: Int, cursor: CursorPosition, typewriter: Boolean): AppState =
    val text = Vector.tabulate(paragraphCount)(paragraph).mkString("\n")
    val buffer = Buffer
      .fromString(bufferId, text)
      .copy(
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 60, visibleLines = visibleLines),
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
      runtime = base.runtime.copy(viewportSize = Some(ViewportSize(70, 24)))
    )

  private def wraps(cache: WrappedLineCache.Bounded): Long =
    val stats = cache.wrapStats
    stats.coldWraps + stats.incrementalWraps

  /** The most paragraphs any one keystroke wrapped, the first of them placing the viewport in a cold cache. */
  private def mostWrappedPerKeystroke(paragraphCount: Int, cursorLine: Int, typewriter: Boolean): Long =
    val cache = WrappedLineCache.bounded()
    val start = stateWith(paragraphCount, CursorPosition(cursorLine, 40), typewriter)
    val type_ = (state: AppState) =>
      new EditorPaneComponent(paneId, cache).processEvent(InsertChar('x'), state) match
        case ComponentResult.ReducerUpdate(update) => CursorViewport.ensureVisibleCursors(state, update.state, cache)
        case other                                 => fail(s"typing was not reduced: $other")
    val (_, perKeystroke) = (1 to keystrokes).foldLeft((start, Vector.empty[Long])) {
      case ((state, costs), _) =>
        val before = wraps(cache)
        val next   = type_(state)
        (next, costs :+ (wraps(cache) - before))
    }
    perKeystroke.max

  "A keystroke in a long document" should "wrap only the paragraphs around the cursor" in
    List(false, true).foreach { typewriter =>
      List(0.5, 0.99, 1.0).foreach { position =>
        val count = 5_000
        val line  = math.min(count - 1, (count * position).toInt)
        withClue(s"typewriter=$typewriter cursor line $line: ")(
          mostWrappedPerKeystroke(count, line, typewriter) should be <= (visibleLines + 4).toLong
        )
      }
    }

  it should "wrap as many paragraphs as in a document a tenth the size" in {
    val small = mostWrappedPerKeystroke(500, 250, typewriter = false)
    val large = mostWrappedPerKeystroke(5_000, 2_500, typewriter = false)
    large shouldBe small
  }
