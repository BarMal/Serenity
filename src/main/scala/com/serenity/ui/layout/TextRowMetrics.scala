package com.serenity.ui.layout

/** Where a pane's visual rows sit. A row's text box is where its glyphs, caret and selection go; its band is the text
  * box plus the blank room a heading's space gives it above and below, and the bands of consecutive rows tile the pane.
  * A pixel in a row's room belongs to that row.
  *
  * On a cell grid, space is whole rows and `rowLineHeightPx` is the pixels in one.
  */
final case class TextRowMetrics(
    contentRect: LayoutRect,
    gridMetrics: CellMetrics,
    rowLineHeightPx: Int,
    usesMeasuredLayout: Boolean,
    // Per-visual-row heights (measured/proportional rich text). Empty means every row is `rowLineHeightPx` tall, which
    // reproduces the old uniform stacking exactly. Entries of 0 also fall back to `rowLineHeightPx`.
    rowHeightsPx: Vector[Int] = Vector.empty,
    // The room above and below each visual row; empty means none, and the rows stack as they always have.
    rowSpaceBeforePx: Vector[Int] = Vector.empty,
    rowSpaceAfterPx: Vector[Int] = Vector.empty
):
  private val contentTopPx: Int =
    gridMetrics.toPixelY(contentRect.y)

  private val hasSpace: Boolean =
    rowSpaceBeforePx.exists(_ > 0) || rowSpaceAfterPx.exists(_ > 0)

  /** Whether rows are stacked from their own heights and space rather than at a fixed pitch. */
  private val stacked: Boolean = usesMeasuredLayout || hasSpace

  private val cellRowPx: Int = math.max(1, rowLineHeightPx)

  private val rowCount: Int = math.max(rowHeightsPx.length, math.max(rowSpaceBeforePx.length, rowSpaceAfterPx.length))

  private def heightOf(visualRow: Int): Int =
    rowHeightsPx.lift(visualRow).filter(_ > 0).getOrElse(rowLineHeightPx)

  private def spaceBefore(visualRow: Int): Int = rowSpaceBeforePx.lift(visualRow).getOrElse(0)

  private def spaceAfter(visualRow: Int): Int = rowSpaceAfterPx.lift(visualRow).getOrElse(0)

  private def textBoxPx(visualRow: Int): Int =
    if usesMeasuredLayout then math.max(1, rowHeightsPx.lift(visualRow).getOrElse(rowLineHeightPx))
    else cellRowPx

  /** The top of each row's band, from the top of the content (index i = band of row i; last entry = bottom of the final
    * band). Empty when rows are not stacked.
    */
  private val relativeBandTops: Vector[Int] =
    if stacked then
      Vector.range(0, rowCount).scanLeft(0)((top, row) => top + spaceBefore(row) + textBoxPx(row) + spaceAfter(row))
    else Vector.empty

  private val relativeBottomPx: Int = relativeBandTops.lastOption.getOrElse(0)

  private def rowPitchPx: Int = if usesMeasuredLayout then rowLineHeightPx else cellRowPx

  /** Distance of a row's text box from the top of the content: rows past the table follow the last band at the pitch.
    */
  private def relativeLineTop(visualRow: Int): Int =
    if visualRow < rowCount then relativeBandTops(visualRow) + spaceBefore(visualRow)
    else relativeBottomPx + (visualRow - rowCount) * rowPitchPx

  /** A distance from the top of the content as a surface pixel row; on a cell grid it lands on a whole cell. */
  private def absolutePx(relativePx: Int): Int =
    if usesMeasuredLayout then contentTopPx + relativePx
    else gridMetrics.toPixelY(contentRect.y + relativePx / cellRowPx)

  /** The measured height of a single visual row (the tallest run on it), or the grid row height in cell layout. */
  def rowHeightPx(visualRow: Int): Int =
    if usesMeasuredLayout then heightOf(visualRow) else gridMetrics.lineHeight

  def contentBottomPx: Int =
    gridMetrics.toPixelY(contentRect.bottom)

  def surfaceBottomPx(viewportHeightCells: Int): Int =
    gridMetrics.toPixelY(viewportHeightCells)

  /** The top of the row's text box, below any room its band gives it above. */
  def lineTopPx(visualRow: Int): Int =
    if stacked then absolutePx(relativeLineTop(visualRow))
    else gridMetrics.toPixelY(contentRect.y + visualRow)

  /** The top of the row's band: where the room above its text box begins. */
  def bandTopPx(visualRow: Int): Int =
    if stacked && visualRow < rowCount then absolutePx(relativeBandTops(visualRow)) else lineTopPx(visualRow)

  /** The bottom of the row's band: where the room below its text box ends, and the next row's band begins. */
  def bandBottomPx(visualRow: Int): Int =
    if stacked && visualRow < rowCount then absolutePx(relativeBandTops(visualRow + 1))
    else lineTopPx(visualRow) + rowHeightPx(visualRow)

  /** On a cell grid, the screen row the row's text is drawn on, below the blank rows its heading's space puts above. */
  def screenRow(visualRow: Int): Int =
    if usesMeasuredLayout || !hasSpace then contentRect.y + visualRow
    else contentRect.y + relativeLineTop(visualRow) / cellRowPx

  /** On a cell grid, the blank rows above the row's text: its own room above and every earlier row's room. */
  def blankRowsAbove(visualRow: Int): Int =
    screenRow(visualRow) - contentRect.y - visualRow

  def lineFits(visualRow: Int): Boolean =
    if stacked then lineTopPx(visualRow) < contentBottomPx
    else visualRow < contentRect.height

  def lineVisible(visualRow: Int, viewportHeightCells: Int): Boolean =
    if usesMeasuredLayout then
      val topPx = lineTopPx(visualRow)
      topPx >= contentTopPx &&
      topPx < contentBottomPx &&
      topPx < surfaceBottomPx(viewportHeightCells)
    else
      val row = screenRow(visualRow)
      row >= 0 && row < contentRect.bottom && row < viewportHeightCells

  /** The visual row that owns an absolute surface pixel Y -- the inverse of [[lineTopPx]]. Used by mouse hit-testing so
    * a click on a tall heading row targets that row rather than a uniform-grid guess, and a click in a heading's room
    * targets the heading. Clamps above the first row and below the last.
    */
  def visualRowAt(pixelY: Int): Int =
    if !usesMeasuredLayout && !hasSpace then math.max(0, gridMetrics.toRow(pixelY) - contentRect.y)
    else if usesMeasuredLayout && rowHeightsPx.isEmpty && !hasSpace then
      math.max(0, (pixelY - contentTopPx) / math.max(1, rowLineHeightPx))
    else bandAt(pixelY - contentTopPx)

  /** The visual row that owns a screen row of a cell grid, blank rows included. */
  def visualRowAtScreenRow(screenRowIndex: Int): Int =
    if !hasSpace then math.max(0, screenRowIndex - contentRect.y)
    else bandAt((screenRowIndex - contentRect.y) * cellRowPx)

  private def bandAt(relativePx: Int): Int =
    val firstBelow = relativeBandTops.indexWhere(_ > relativePx)
    val row        = if firstBelow < 0 then rowCount - 1 else firstBelow - 1
    row.max(0)

  def cursorTopPx(visualRow: Int): Int =
    if usesMeasuredLayout then lineTopPx(visualRow)
    else math.max(contentTopPx, lineTopPx(visualRow) - cursorOpticalLiftPx)

  /** A small nudge that lifts a cell-grid cursor's top edge slightly above its row's own top, for a real sub-pixel font
    * (a monospaced code font, say 16px tall) where a couple of spare pixels make the caret look better centred. The
    * `2`-pixel floor below assumes a row several pixels tall to spare; on a grid whose row *is* the pixel unit (TUI's
    * `CellMetricsOne`, `rowLineHeightPx == 1`), that same floor would lift the cursor a full row or more off its real
    * position, so the lift is capped to never exceed half the row -- vanishing entirely once a row has no pixels to
    * spare.
    */
  private def cursorOpticalLiftPx: Int =
    math.min(rowLineHeightPx / 2, math.max(2, math.round(rowLineHeightPx.toFloat * 0.125f)))

object TextRowMetrics:

  /** The metrics of a pane painting `snapshot` into `contentRect`: each visual line's own height, and the room around
    * it only when some line has any, so a pane without heading space stacks exactly as it always has.
    */
  def forSnapshot(
    contentRect: LayoutRect,
    gridMetrics: CellMetrics,
    snapshot: TextLayoutSnapshot,
    measured: Boolean
  ): TextRowMetrics =
    val lines  = snapshot.visualLines
    val spaced = lines.exists(line => line.spaceBeforePx > 0 || line.spaceAfterPx > 0)
    TextRowMetrics(
      contentRect = contentRect,
      gridMetrics = gridMetrics,
      rowLineHeightPx = snapshot.lineHeightPx,
      usesMeasuredLayout = measured,
      rowHeightsPx = snapshot.rowHeightsPx,
      rowSpaceBeforePx = if spaced then lines.map(_.spaceBeforePx) else Vector.empty,
      rowSpaceAfterPx = if spaced then lines.map(_.spaceAfterPx) else Vector.empty
    )
