package com.serenity.ui.tui

/** Turns one screen-buffer transition into the fewest ANSI bytes that reproduce it on a real terminal, the way
  * notcurses, crossterm and Neovim's TUI do:
  *
  *   - SGR is sent as a delta against what the terminal already holds ([[SgrState.transition]]), never as a full reset
  *     per style change;
  *   - the cursor moves by the cheapest means ([[CursorMotion]]), and a short run of unchanged cells between two
  *     changed ones is rewritten when that costs fewer bytes than moving over it;
  *   - a viewport scroll is handed to the terminal as DECSTBM + SU/SD ([[ScrollPlan]]) when the caller says the
  *     terminal can, so only the exposed rows are repainted;
  *   - a full clear-and-repaint when there is no previous frame to diff against (first frame, or a resize).
  */
object TerminalAnsiDiff:

  private val Esc: Char   = 0x1b.toChar
  private val Reset       = s"$Esc[0m"
  private val ClearScreen = s"$Esc[2J"
  private val CursorHome  = s"$Esc[H"

  /** Rewriting unchanged cells instead of skipping them is only worth weighing for gaps up to this many cells. */
  private val MaxBridgedGap = 8

  /** The bytes of one emission and the terminal state it leaves behind. `endPen` is what the next emission can delta
    * against; `wrote` is whether any cell was drawn, i.e. whether the terminal's attributes were touched at all.
    */
  final case class Encoded(ansi: String, endPen: Option[SgrState], wrote: Boolean)

  final private case class Pass(pen: Option[SgrState], cursor: Option[CursorPos], wrote: Boolean)

  /** A self-contained emission: it assumes nothing about the terminal's attributes beforehand and resets them
    * afterwards.
    *
    * `dirtyRows`, when given, is trusted as the complete set of rows that could have changed -- rows outside it are
    * never scanned at all, rather than re-derived or double-checked. `None` (every call site before #1464, and every
    * caller with no such record available) preserves the original full-frame scan. `TerminalScreenBuffer.flushDiff`
    * passes `consumeDirtyRows()`, the one source that actually covers every paint path -- see its own doc comment for
    * why the renderer's pane-scoped `FramePlan.dirtyRowsByPane` can't safely serve here instead.
    *
    * `scrollRegions` says the terminal supports DECSTBM with SU/SD; without it every moved row is rewritten.
    */
  def emit(
    previous: Option[TerminalFrame],
    next: TerminalFrame,
    dirtyRows: Option[Set[Int]] = None,
    scrollRegions: Boolean = true
  ): String =
    val encoded = encode(previous, next, dirtyRows, scrollRegions, startPen = None)
    if encoded.wrote then encoded.ansi + Reset else encoded.ansi

  /** An emission that continues from `startPen` -- the attributes the previous emission left -- and leaves its own
    * attributes in place rather than resetting them. The caller owns that state and must reset the terminal's
    * attributes before anything else writes to it.
    */
  def encode(
    previous: Option[TerminalFrame],
    next: TerminalFrame,
    dirtyRows: Option[Set[Int]],
    scrollRegions: Boolean,
    startPen: Option[SgrState]
  ): Encoded =
    val styles = new StyleTable
    encodeGrids(
      previous.map(CellGrid.fromFrame(_, styles)),
      CellGrid.fromFrame(next, styles),
      dirtyRows,
      scrollRegions,
      startPen
    )

  /** [[encode]] over the compact grids the screen buffer keeps. The two grids must share a [[StyleTable]], which is
    * what makes comparing their style ids comparing their styles; grids that do not are treated as having no usable
    * previous frame.
    */
  def encodeGrids(
    previous: Option[CellGrid],
    next: CellGrid,
    dirtyRows: Option[Set[Int]],
    scrollRegions: Boolean,
    startPen: Option[SgrState]
  ): Encoded =
    previous match
      case Some(prev) if prev.width == next.width && prev.height == next.height && prev.styles.eq(next.styles) =>
        encodeDiff(prev, next, dirtyRows, scrollRegions, startPen)
      case _ => encodeFull(next, startPen)

  private def encodeFull(frame: CellGrid, startPen: Option[SgrState]): Encoded =
    val sb    = new StringBuilder
    val _     = sb.append(ClearScreen).append(CursorHome)
    val start = Pass(startPen, Some(CursorPos(0, 0)), wrote = false)
    val end   = (0 until frame.height).foldLeft(start)((pass, y) => writeRow(sb, pass, frame, y, 0 until frame.width))
    Encoded(sb.toString, end.pen, end.wrote)

  private def encodeDiff(
    previous: CellGrid,
    next: CellGrid,
    dirtyRows: Option[Set[Int]],
    scrollRegions: Boolean,
    startPen: Option[SgrState]
  ): Encoded =
    val candidateRows = dirtyRows.getOrElse((0 until next.height).toSet).toSeq.sorted
    val plain         = encodeRows(previous, next, candidateRows, None, startPen)
    val scrolled = Option
      .when(scrollRegions)(ScrollPlan.detect(previous, next, dirtyRows))
      .flatten
      .map(plan => encodeRows(previous, next, (candidateRows ++ plan.rows).distinct.sorted, Some(plan), startPen))
    scrolled.filter(_.ansi.length < plain.ansi.length).getOrElse(plain)

  private def encodeRows(
    previous: CellGrid,
    next: CellGrid,
    rows: Seq[Int],
    plan: Option[ScrollPlan],
    startPen: Option[SgrState]
  ): Encoded =
    val sb = new StringBuilder
    plan.foreach(scroll => sb.append(scroll.sequence(next.height)))
    val end = rows.foldLeft(Pass(startPen, None, wrote = false)) { (pass, y) =>
      val before = plan.fold(Option(y))(_.sourceRow(y))
      writeRow(sb, pass, next, y, changedColumns(previous, before, next, y))
    }
    Encoded(sb.toString, end.pen, end.wrote)

  private def changedColumns(previous: CellGrid, before: Option[Int], next: CellGrid, y: Int): Seq[Int] =
    before match
      case Some(source) => next.differingColumns(y, previous, source)
      case None         => 0 until next.width

  private def writeRow(sb: StringBuilder, start: Pass, frame: CellGrid, y: Int, columns: Seq[Int]): Pass =
    columns.foldLeft(start)((pass, x) => writeBridged(sb, pass, frame, y, x))

  /** Draws column `x`, first rewriting the unchanged cells between the cursor and `x` if that is the cheaper way across
    * them. Both ways are rendered and measured, so the choice accounts for the style changes bridging would cause.
    */
  private def writeBridged(sb: StringBuilder, pass: Pass, frame: CellGrid, y: Int, x: Int): Pass =
    val gap = pass.cursor match
      case Some(CursorPos(cursorX, cursorY)) if cursorY == y && cursorX < x && x - cursorX <= MaxBridgedGap =>
        cursorX until x
      case _ => Range(0, 0)
    if gap.isEmpty then writeCell(sb, pass, frame, y, x)
    else
      val skipped       = renderCells(pass, frame, y, Seq(x))
      val bridged       = renderCells(pass, frame, y, gap :+ x)
      val (text, after) = if bridged._1.length < skipped._1.length then bridged else skipped
      val _             = sb.append(text)
      after

  private def renderCells(start: Pass, frame: CellGrid, y: Int, columns: Seq[Int]): (String, Pass) =
    val scratch = new StringBuilder
    val end     = columns.foldLeft(start)((pass, x) => writeCell(scratch, pass, frame, y, x))
    (scratch.toString, end)

  /** A continuation cell draws nothing of its own -- its wide glyph already covered it. */
  private def writeCell(sb: StringBuilder, pass: Pass, frame: CellGrid, y: Int, x: Int): Pass =
    val glyph = frame.glyphAt(x, y)
    val span  = CellGrid.spanOf(glyph)
    if span == CellSpan.Continuation then pass
    else
      val pen     = frame.styleAt(x, y).sgr
      val advance = if span == CellSpan.Wide then 2 else 1
      val _ = appendCodePoint(
        sb.append(CursorMotion.between(pass.cursor, CursorPos(x, y), frame.width))
          .append(SgrState.transition(pass.pen, pen)),
        CellGrid.codePointOf(glyph)
      ).append(frame.marksAt(x, y))
      Pass(Some(pen), Some(CursorPos(x + advance, y)), wrote = true)

  private def appendCodePoint(sb: StringBuilder, codePoint: Int): StringBuilder =
    if Character.isBmpCodePoint(codePoint) then sb.append(codePoint.toChar)
    else sb.appendAll(Character.toChars(codePoint))
