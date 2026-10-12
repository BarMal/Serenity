package com.serenity.state.manager

import com.serenity.config.{AppConfig, MarkdownViewMode}
import com.serenity.keystroke.events.{MouseClick, MouseDrag, MouseInputEvent}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Mouse hit-testing in a Markdown buffer whose markers are hidden (#1851): the pointer resolves through the same
  * visual rows the renderer paints -- heading heights, hidden columns, table padding, wrapped rows -- so a click lands
  * on the character shown under it, below a table or a wrapped paragraph as much as above.
  */
class MarkdownLensMouseHitTestSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId       = PaneId(0)
  private val bufferId     = BufferId(0)
  private val viewportSize = ViewportSize(60, 60)

  private val paragraph =
    "Plain words flow on and on across the pane until the line has to wrap over several rows, " +
      "and then keeps going with some more words so that it wraps a few times before it finally ends."

  private val lines = Vector(
    "# Heading one",
    "",
    paragraph,
    "",
    "| name | value |",
    "|------|-------|",
    "| a much wider cell | x |",
    "",
    "after the table",
    "",
    "tail with **bold** word"
  )

  private def stateIn(mode: MarkdownViewMode, caret: CursorPosition = CursorPosition(0, 0)): AppState =
    val base = Buffer.fromString(bufferId, lines.mkString("\n"))
    val buffer = base.copy(
      document = base.document.copy(language = Some(LanguageId.Markdown)),
      editing = EditingState(List(caret))
    )
    val initial = AppState.initial
    initial.copy(
      persisted = initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config =
          AppConfig.default.withoutStatusLine.withLineNumbers(false).withWordWrap(true).withMarkdownViewMode(mode)
      ),
      runtime = initial.runtime.copy(viewportSize = Some(viewportSize))
    )

  private def cacheOf(state: AppState): MouseTargetCache =
    MouseTargetCache.fromState(state, viewportSize, AuthoritativeUiScene())

  /** Pixel centre of the glyph at buffer position `at`, found only from the painted rows' heights and caret stops. */
  private def pointOn(state: AppState, at: CursorPosition): (Int, Int) =
    val cache       = cacheOf(state)
    val contentRect = cache.scene.paneLayouts(paneId).contentRect
    val snapshot    = cache.scene.textSnapshot(paneId).getOrElse(fail("no text snapshot"))
    val metrics     = MouseHitTestGeometry.floatingCellMetrics(state)
    val rowIndex = snapshot.visualLines.indexWhere(line =>
      line.bufferLine == at.line && at.column >= line.startColumn && at.column < line.endColumn.max(
        line.startColumn + 1
      )
    )
    withClue(s"no visual row shows $at: ")(rowIndex should be >= 0)
    val heights = snapshot.visualLines.map(line => if line.heightPx > 0 then line.heightPx else snapshot.lineHeightPx)
    val top     = metrics.toPixelY(contentRect.y) + heights.take(rowIndex).sum
    val row     = snapshot.visualLines(rowIndex)
    val left    = row.xForColumn(at.column).getOrElse(0.0f)
    val right   = row.xForColumn(at.column + 1).getOrElse(left)
    val cellPx  = snapshot.panelWidthPx.toFloat / contentRect.width
    val x       = (contentRect.x * cellPx + row.xOffsetPx + (left + right) / 2.0f).toInt
    (x, top + heights(rowIndex) / 2)

  private def clickAt(state: AppState, at: CursorPosition): MouseClick =
    val (x, y)  = pointOn(state, at)
    val metrics = MouseHitTestGeometry.floatingCellMetrics(state)
    MouseClick(metrics.toCol(x), metrics.toRow(y), pixelX = Some(x), pixelY = Some(y))

  private def resolve(state: AppState, event: MouseInputEvent): Option[CursorPosition] =
    EditorMouseTargeting
      .targetAt(event, state, cacheOf(state))
      .fold(missing => fail(s"missing snapshot ${missing.paneId}"), identity)
      .map(_._3)

  private val modes = List(MarkdownViewMode.LivePreview, MarkdownViewMode.Read)

  for mode <- modes do
    s"A click in $mode mode" should "land on the character under the pointer in the line below a table" in {
      val state = stateIn(mode)
      resolve(state, clickAt(state, CursorPosition(8, 6))) shouldBe Some(CursorPosition(8, 6))
    }

    it should "land on the character under the pointer in the line below a wrapped paragraph" in {
      val state = stateIn(mode)
      resolve(state, clickAt(state, CursorPosition(4, 3))) shouldBe Some(CursorPosition(4, 3))
      resolve(state, clickAt(state, CursorPosition(2, paragraph.length - 5))) shouldBe
        Some(CursorPosition(2, paragraph.length - 5))
    }

    it should "land on the heading text rather than the row above or below" in {
      val state = stateIn(mode)
      resolve(state, clickAt(state, CursorPosition(0, 4))) shouldBe Some(CursorPosition(0, 4))
    }

    it should "land inside a padded table cell on the character shown under the pointer" in {
      val state = stateIn(mode)
      resolve(state, clickAt(state, CursorPosition(6, 10))) shouldBe Some(CursorPosition(6, 10))
      resolve(state, clickAt(state, CursorPosition(6, 21))) shouldBe Some(CursorPosition(6, 21))
    }

    it should "land past a run of hidden markers on the visible character" in {
      val state = stateIn(mode)
      resolve(state, clickAt(state, CursorPosition(10, 12))) shouldBe Some(CursorPosition(10, 12))
      resolve(state, clickAt(state, CursorPosition(10, 20))) shouldBe Some(CursorPosition(10, 20))
    }

  "A drag in Live Preview" should "extend the selection to the character under the pointer below a table" in {
    val state  = stateIn(MarkdownViewMode.LivePreview)
    val (x, y) = pointOn(state, CursorPosition(8, 6))
    val cells  = MouseHitTestGeometry.floatingCellMetrics(state)
    val drag   = MouseDrag(cells.toCol(x), cells.toRow(y), pixelX = Some(x), pixelY = Some(y))
    val target = EditorMouseTargeting.targetAt(drag, state, cacheOf(state)).toOption.flatten
    val result = MouseTransition.run(state)(MouseHitTesting.editorDrag(target))._1.state
    result.persisted.buffers(bufferId).primarySelection.map(_.focus) shouldBe Some(CursorPosition(8, 6))
  }

  "A double click in Read mode" should "select only the visible word" in {
    val state  = stateIn(MarkdownViewMode.Read)
    val (x, y) = pointOn(state, CursorPosition(10, 12))
    val cells  = MouseHitTestGeometry.floatingCellMetrics(state)
    val click  = MouseClick(cells.toCol(x), cells.toRow(y), pixelX = Some(x), pixelY = Some(y), clickCount = 2)
    val target = EditorMouseTargeting.targetAt(click, state, cacheOf(state)).toOption.flatten
    val result = MouseTransition.run(state)(MouseHitTesting.editorClick(click, target))._1.state
    result.persisted
      .buffers(bufferId)
      .primarySelection
      .map(selection => lines(10).substring(selection.anchor.column, selection.focus.column)) shouldBe Some("bold")
  }
