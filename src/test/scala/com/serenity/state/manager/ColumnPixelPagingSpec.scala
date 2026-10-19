package com.serenity.state.manager

import com.serenity.state.manager.ColumnPagingFixture.{rowId, sameTop, stackedPx}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A column holds the rows that fit its pixel height, not a fixed number of rows: headings (size scale and the space
  * around them), drop caps (the height their glyph reserves) and any other tall row take more than one base line, so
  * the rows of a column must never add up to more than the column is tall -- and every row must still be placed exactly
  * once across the pages, with the cursor's page always showing the cursor.
  */
class ColumnPixelPagingSpec extends AnyFlatSpec with Matchers:

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

    s"The fixture $label" should "lay rows out on a measured layout with headings and drop caps taller than a line" in {
      fixture.textColumns should (be >= 10 and be <= 100)
      fixture
        .columnsAt(com.serenity.state.models.Viewport(0, 0, fixture.textColumns, 1), 1, rows = 100000)
        .head
        .usesMeasuredLayout shouldBe true
      val rows = fixture.pages.flatMap(_.columns).flatMap(_.visualLines)
      rows.exists(_.spaceBeforePx > 0) shouldBe true
      rows.exists(_.reservedBelowPx > 0) shouldBe true
      fixture.pages.size should be > 3
    }

    s"A column $label" should "never stack its rows taller than the column" in {
      val overflowing = fixture.pages.zipWithIndex.flatMap { (page, pageIndex) =>
        page.columns.zipWithIndex.collect {
          case (column, columnIndex) if stackedPx(column) > fixture.columnHeightPx =>
            s"page $pageIndex column $columnIndex stacks ${stackedPx(column)}px of ${fixture.columnHeightPx}px"
        }
      }
      overflowing.take(5) shouldBe Vector.empty
    }

    it should "hold fewer rows than a base-line count when its rows are tall" in {
      val columns = fixture.pages.flatMap(_.columns)
      val sizes   = columns.map(_.visualLines.size)
      sizes.exists(_ < fixture.rowsPerColumn) shouldBe true
      columns.foreach(column => column.visualLines.size should be <= fixture.columnHeightPx / column.lineHeightPx)
    }

    it should "place every row exactly once across the pages" in {
      fixture.pages.flatMap(_.columns).flatMap(_.visualLines).map(rowId) shouldBe fixture.truth
    }

    it should "open every column with no heading space above its first row" in
      fixture.pages.flatMap(_.columns).foreach(column => column.visualLines.head.spaceBeforePx shouldBe 0)

    it should "show the cursor on the page it anchors, whichever row it is on" in {
      val missing = fixture.truth.flatMap { id =>
        val anchored = fixture.placed(fixture.buffer, id).viewport
        val shown    = fixture.columnsAt(anchored).flatMap(_.visualLines).map(rowId)
        Option.when(!shown.contains(id))(s"row $id is not on its page")
      }
      missing.take(5) shouldBe Vector.empty
    }

    it should "anchor the same page walking from the previous page as from a cold viewport" in {
      val walked = fixture.truth.scanLeft(fixture.buffer)((walk, id) => fixture.placed(walk, id))
      val differing = walked.drop(1).zip(fixture.truth).collect {
        case (walk, id) if !sameTop(walk.viewport, fixture.placed(fixture.buffer, id).viewport) =>
          s"row $id walks to ${walk.viewport.topLine}/${walk.viewport.topVisualLine}"
      }
      differing.take(5) shouldBe Vector.empty
    }

    it should "anchor the same page walking back up as walking down" in {
      val upwards = fixture.truth.reverse
      val walked =
        upwards.scanLeft(fixture.placed(fixture.buffer, upwards.head))((walk, id) => fixture.placed(walk, id))
      val differing = walked.drop(1).zip(upwards).collect {
        case (walk, id) if !sameTop(walk.viewport, fixture.placed(fixture.buffer, id).viewport) =>
          s"row $id walks to ${walk.viewport.topLine}/${walk.viewport.topVisualLine}"
      }
      differing.take(5) shouldBe Vector.empty
    }
