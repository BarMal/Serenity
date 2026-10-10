package com.serenity.ui.layout

/** A run of consecutive rows of the current frame whose pixels the previous frame already holds, `fromRow` being the
  * row the first of them was on then.
  */
final case class RowRun(firstRow: Int, rowCount: Int, fromRow: Int):
  def rows: Range = firstRow until firstRow + rowCount

/** The pixel bands one column of a pane's rows occupies -- its text, or the line numbers beside it -- in the previous
  * frame and in this one, indexed by row.
  */
final case class RowBands(previous: Vector[PixelRect], current: Vector[PixelRect])

/** Finds the rows of a pane that moved rather than changed, so a frame can move their pixels instead of drawing them
  * again: a scroll, or a paragraph gaining a wrapped row above them.
  *
  * Pure data in, plain row runs out, like [[DirtyLineDiff]], and biased the same way: a row is reused only when nothing
  * about its pixels can differ. Its own text must be the same visual line, it must sit the same distance from where it
  * was in every band, and so must its neighbours -- the glyphs of a redrawn row can spill a few pixels into the band
  * above or below, and a moved row carries the spill of its old neighbours rather than its new ones.
  */
object RowMoves:

  /** A frame that moves more separate runs than this is cheaper to draw again than to copy piece by piece. */
  val MaxRuns: Int = 8

  /** The runs of `current`'s rows that `previous` holds a fixed distance away, or none when the frames cannot be
    * compared. `unreusable` rows are redrawn whatever the previous frame held (damage names their line). Only the most
    * common non-zero distance is taken: rows that stayed put need no move, and one distance keeps the copies from
    * overwriting each other's sources.
    */
  def reusableRuns(
    previous: TextLayoutSnapshot,
    current: TextLayoutSnapshot,
    bands: List[RowBands],
    unreusable: Set[Int]
  ): List[RowRun] =
    if bands.isEmpty || !DirtyLineDiff.comparableShape(previous, current) || !sameRichDocument(previous, current) then Nil
    else
      val previousRowOf =
        previous.visualLines.zipWithIndex.map((line, row) => (line.bufferLine, line.startColumn) -> row).toMap
      def sourceOf(row: Int): Option[Int] =
        current.visualLines.lift(row).flatMap { line =>
          previousRowOf.get((line.bufferLine, line.startColumn)).filter(previous.visualLines(_) == line)
        }
      def distanceOf(row: Int, from: Int): Option[Int] =
        val distances = bands.map(band => distanceBetween(band.previous.lift(from), band.current.lift(row)))
        distances.headOption.flatten.filter(distance => distances.forall(_.contains(distance)))
      val moves         = current.visualLines.indices.flatMap(row => sourceOf(row).map(from => row -> from)).toMap
      val distanceByRow = moves.flatMap((row, from) => distanceOf(row, from).map(row -> _))
      mostCommonMove(distanceByRow.values.filter(_ != 0)).fold(List.empty[RowRun]) { distance =>
        def holdsAt(row: Int, from: Int): Boolean =
          moves.get(row).contains(from) && distanceByRow.get(row).contains(distance)
        def stable(row: Int, from: Int): Boolean =
          List(-1, 1).forall { step =>
            current.visualLines.lift(row + step) == previous.visualLines.lift(from + step) &&
            (current.visualLines.lift(row + step).isEmpty ||
              (holdsAt(row + step, from + step) && !unreusable.contains(row + step)))
          }
        val reusable = moves.collect {
          case (row, from) if holdsAt(row, from) && !unreusable.contains(row) && stable(row, from) => row
        }.toSet
        val runs = runsOf(reusable.toList.sorted, moves)
        if runs.sizeIs > MaxRuns then Nil else runs
      }

  /** A rich document edited in place restyles rows whose visual lines compare equal, so only an unchanged one is safe. */
  private def sameRichDocument(previous: TextLayoutSnapshot, current: TextLayoutSnapshot): Boolean =
    (previous.richTextDocument, current.richTextDocument) match
      case (None, None)                => true
      case (Some(before), Some(after)) => before eq after
      case _                           => false

  private def distanceBetween(previous: Option[PixelRect], current: Option[PixelRect]): Option[Int] =
    for
      was <- previous
      now <- current
      if was.heightPx > 0 && was.heightPx == now.heightPx && was.xPx == now.xPx && was.widthPx == now.widthPx
    yield now.yPx - was.yPx

  private def mostCommonMove(distances: Iterable[Int]): Option[Int] =
    distances.groupBy(identity).maxByOption((_, group) => group.size).map(_._1)

  private def runsOf(rows: List[Int], sourceOf: Map[Int, Int]): List[RowRun] =
    rows
      .foldLeft(List.empty[RowRun]) {
        case (run :: earlier, row)
            if run.firstRow + run.rowCount == row && sourceOf.get(row).contains(run.fromRow + run.rowCount) =>
          run.copy(rowCount = run.rowCount + 1) :: earlier
        case (runs, row) => RowRun(row, 1, sourceOf.getOrElse(row, row)) :: runs
      }
      .reverse
