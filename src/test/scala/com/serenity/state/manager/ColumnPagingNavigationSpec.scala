package com.serenity.state.manager

import com.serenity.VerticalNavSupport
import com.serenity.keystroke.events.{ColumnLeft, ColumnRight, EditorEvent}
import com.serenity.rope.Balance
import com.serenity.state.manager.ColumnPagingFixture.{RowId, paneId, rowId}
import com.serenity.state.models.*
import com.serenity.ui.layout.WrappedLineCache
import com.serenity.ui.renderer.ShapedTextCache
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Moving by column (what PageUp and PageDown do in column mode) walks the canonical columns one at a time, forwards
  * and back, and the page the viewport anchors afterwards always shows the cursor in the column it moved to -- whatever
  * the heights of the rows each column holds.
  */
class ColumnPagingNavigationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val shaping = ShapedTextCache()

  private def uncached: WrappedLineCache = WrappedLineCache.sharing(shaping)

  private def cursorOf(state: AppState, fixture: ColumnPagingFixture): CursorPosition =
    state.persisted.buffers(fixture.buffer.id).editing.cursorPositions.head

  /** The row the cursor sits on, found among the rows of the whole document. */
  private def rowOf(fixture: ColumnPagingFixture, cursor: CursorPosition): Option[RowId] =
    fixture.truth.reverse.find((line, column) => line == cursor.line && column <= cursor.column)

  private def move(state: AppState, event: EditorEvent): AppState =
    CursorViewport.ensureVisibleCursors(
      state,
      VerticalNavSupport.dispatch(event, paneId, state, uncached).state,
      uncached
    )

  /** The column, counted from the first column of the document, the cursor is in -- when the page the viewport anchors
    * shows it.
    */
  private def columnShown(state: AppState, fixture: ColumnPagingFixture): Option[Int] =
    val buffer = state.persisted.buffers(fixture.buffer.id)
    rowOf(fixture, cursorOf(state, fixture))
      .filter(id => fixture.columnsAt(buffer.viewport).flatMap(_.visualLines).map(rowId).contains(id))
      .flatMap(fixture.columnOrdinal)

  for
    (documentLabel, source) <- List(
      ""                     -> ColumnPagingFixture.document,
      " over swept headings" -> ColumnPagingFixture.sweptHeadings
    )
    scale   <- List(1, 2)
    columns <- List(1, 2, 3)
  do
    val label        = s"at ${scale}x with $columns column(s)$documentLabel"
    lazy val fixture = ColumnPagingFixture(scale, columns, source = source)

    s"Column right $label" should "advance one column at a time through the whole document, showing the cursor" in {
      val columnCount = fixture.pages.flatMap(_.columns).size
      val walk        = Iterator.iterate(fixture.state)(move(_, ColumnRight)).take(columnCount + 2).toVector
      val ordinals    = walk.map(columnShown(_, fixture))

      ordinals.forall(_.isDefined) shouldBe true
      ordinals.flatten.distinct shouldBe (0 until columnCount).toVector
      ordinals.flatten.sliding(2).forall { case Vector(a, b) => b - a == 0 || b - a == 1; case _ => true } shouldBe true
    }

    s"Column left $label" should "retrace the columns column right walked, one at a time" in {
      val columnCount = fixture.pages.flatMap(_.columns).size
      val atEnd       = Iterator.iterate(fixture.state)(move(_, ColumnRight)).take(columnCount + 2).toVector.last
      val back        = Iterator.iterate(atEnd)(move(_, ColumnLeft)).take(columnCount + 2).toVector
      val ordinals    = back.map(columnShown(_, fixture))

      ordinals.forall(_.isDefined) shouldBe true
      ordinals.flatten.distinct shouldBe (0 until columnCount).toVector.reverse
    }

    it should "return to the column it left when column right is followed by column left" in {
      val ordinals = fixture.pages.indices.map { pageIndex =>
        val start  = fixture.state
        val onPage = (0 until pageIndex * columns + 1).foldLeft(start)((state, _) => move(state, ColumnRight))
        val there  = columnShown(onPage, fixture)
        val back   = columnShown(move(move(onPage, ColumnRight), ColumnLeft), fixture)
        (there, back)
      }
      val lastColumn = fixture.pages.flatMap(_.columns).size - 1
      ordinals
        .collect { case (there, back) if there.exists(_ < lastColumn) && there != back => (there, back) }
        .take(5) shouldBe Vector.empty
    }
