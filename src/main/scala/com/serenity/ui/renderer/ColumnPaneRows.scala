package com.serenity.ui.renderer

import com.serenity.ui.layout.*

/** Row reuse for a multi-column (e-reader) pane.
  *
  * Row reuse is keyed by one row index per pane. A column-mode pane paints one snapshot per page column, so its rows
  * are numbered across the columns: column 0's rows first, then column 1's, and so on. Every consumer of
  * [[PaneFrameRecord]] -- damage translation, the comparison with the previous frame, the pixel bands kept -- then
  * works on a multi-column pane exactly as it does on a single-column one, and [[localRows]] hands each column back the
  * rows of its own it has to draw.
  */
private[renderer] object ColumnPaneRows:

  /** Every column's rows as one snapshot, the page's geometry taken from its first column; `None` for a pane not in
    * column mode.
    */
  def flatSnapshot(columns: Vector[ColumnSnapshotPlacement]): Option[TextLayoutSnapshot] =
    columns.headOption.map(first =>
      first.snapshot.copy(
        visualLines = columns.flatMap(_.snapshot.visualLines),
        columnBreaks = firstRows(columns).drop(1)
      )
    )

  /** The pane row each column's first row has, in column order. */
  def firstRows(columns: Vector[ColumnSnapshotPlacement]): Vector[Int] =
    columns.scanLeft(0)(_ + _.snapshot.visualLines.size).dropRight(1)

  /** Each column's share of the pane-wide `dirtyRows`, as that column's own row indices; `None` where the pane draws
    * every row.
    */
  def localRows(columns: Vector[ColumnSnapshotPlacement], dirtyRows: Option[Set[Int]]): Vector[Option[Set[Int]]] =
    columns.zip(firstRows(columns)).map { (column, first) =>
      val rowCount = column.snapshot.visualLines.size
      dirtyRows.map(_.collect { case row if row >= first && row < first + rowCount => row - first })
    }

  /** Each column's share of a pane-wide `rows` to repaint, as that column's own row indices. */
  def localRepaints(columns: Vector[ColumnSnapshotPlacement], rows: Repaint[Int]): Vector[Repaint[Int]] =
    localRows(columns, Option.unless(rows.everything)(rows.parts)).map(_.fold(Repaint.all[Int])(Repaint.only))
