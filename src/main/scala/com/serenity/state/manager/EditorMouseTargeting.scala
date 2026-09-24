package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.Transition
import com.serenity.text.TextEditing
import com.serenity.ui.layout.*

/** State the event pipeline exposes for resolving a mouse event to an editor pane, buffer, and cursor position, as a
  * capability record rather than a trait -- nothing here breaks a construction-order cycle (#1389), so mockability is
  * the only reason this needs an interface at all, and a record fakes trivially without one (#1017).
  */
final private[manager] case class EditorMouseTargetingPort(
    mouseTargetCacheRef: Ref[IO, Option[MouseTargetCache]]
)

/** Resolves a mouse event's screen position to an editor pane/buffer/cursor via the shared [[MouseTargetCache]]. Every
  * other mouse-hit-testing module that needs an editor click target goes through here so the cache stays the single
  * source of truth for the prepared scene shared with rendering.
  *
  * Refreshing the cache is the only IO here; the targeting decision is [[EditorMouseTargeting.targetAt]], a pure
  * function of the cache contents.
  */
final private[manager] class EditorMouseTargeting(port: EditorMouseTargetingPort):

  def resolveMouseTarget(
    click: MouseInputEvent,
    state: AppState
  ): IO[Option[(PaneId, Buffer, CursorPosition)]] =
    state.runtime.viewportSize match
      case None => IO.pure(None)
      case Some(viewportSize) =>
        mouseTargetLayout(state, viewportSize).flatMap { cache =>
          IO.fromEither(
            EditorMouseTargeting
              .targetAt(click, state, cache)
              .left
              .map(missing => new IllegalStateException(s"missing text snapshot for pane ${missing.paneId}"))
          )
        }

  private def mouseTargetLayout(state: AppState, viewportSize: ViewportSize): IO[MouseTargetCache] =
    val key = MouseTargetLayoutKey.from(state, viewportSize)
    port.mouseTargetCacheRef.modify {
      case Some(cache) if cache.layoutKey == key =>
        val scene = AuthoritativeUiScene.forState(state, viewportSize)
        val next  = if cache.scene eq scene then cache else cache.copy(scene = scene)
        Some(next) -> next
      case _ =>
        val next = MouseTargetCache.fromState(state, viewportSize)
        Some(next) -> next
    }

private[manager] object EditorMouseTargeting:

  /** The cache holds a text snapshot for every pane that shows a buffer, so this is an invariant breach, not a miss. */
  final case class MissingTextSnapshot(paneId: PaneId)

  def targetAt(
    event: MouseInputEvent,
    state: AppState,
    cache: MouseTargetCache
  ): Either[MissingTextSnapshot, Option[(PaneId, Buffer, CursorPosition)]] =
    cache.scene.paneLayouts.find((_, paneLayout) => paneLayout.contentRect.contains(event.col, event.row)) match
      case None => Right(None)
      case Some((paneId, paneLayout)) =>
        state.persisted.layout.editorPanes
          .get(paneId)
          .flatMap(pane => pane.bufferId.flatMap(state.persisted.buffers.get)) match
          case None => Right(None)
          case Some(buffer) =>
            cache.scene
              .textSnapshot(paneId)
              .toRight(MissingTextSnapshot(paneId))
              .map(snapshot =>
                cursorAt(event, state, cache, paneId, paneLayout.contentRect, buffer, snapshot)
                  .map(cursor => (paneId, buffer, cursor))
              )

  /** Records the editor position under the pointer, leaving `state` untouched when it is already the hovered one. */
  def hover(target: Option[(PaneId, Buffer, CursorPosition)]): Transition[Unit] =
    val hovered = target.map((paneId, buffer, cursor) => HoveredEditorTarget(paneId, buffer.id, cursor))
    Transition.modify(state =>
      if state.runtime.hoveredEditorTarget == hovered then state
      else state.copy(runtime = state.runtime.copy(hoveredEditorTarget = hovered))
    )

  private def cursorAt(
    click: MouseInputEvent,
    state: AppState,
    cache: MouseTargetCache,
    paneId: PaneId,
    contentRect: LayoutRect,
    buffer: Buffer,
    activeSnapshot: TextLayoutSnapshot
  ): Option[CursorPosition] =
    val vp = buffer.viewport
    // Multi-column e-reader layout (issue #1338, Phase 2 / slice 5): a column-mode page carries one
    // `ColumnSnapshotPlacement` per painted column. The click's cell column selects which one it lands in
    // (its `xOffsetCells`/`columnWidthCells` band, inter-column gaps resolving to the nearer column), and
    // from there the click resolves against *that* column's snapshot and x-origin -- not the shared
    // (column-0) active snapshot every non-column consumer still reads. A non-column pane has no
    // placements, so `snapshot`/`columnXOriginCells`/`columnWidthCells` stay the pane's own, leaving the
    // single-column path below unchanged.
    val placements         = cache.scene.columnSnapshotsFor(paneId)
    val columnXCells       = (click.col - contentRect.x).max(0)
    val placement          = MouseHitTestGeometry.columnPlacementForX(placements, columnXCells)
    val snapshot           = placement.map(_.snapshot).getOrElse(activeSnapshot)
    val columnXOriginCells = placement.map(_.xOffsetCells).getOrElse(0)
    // Slice 2 reserves a line-number rail on each column's left, so the text starts `gutterWidthCells`
    // past the band's left edge and wraps in the remaining width. A click therefore resolves against the
    // text region, not the whole band. A non-column pane has no rail (`0`), leaving the single-column
    // path below unchanged.
    val columnGutterCells = placement.map(_.gutterWidthCells).getOrElse(0)
    val columnTextWidthCells =
      placement.map(p => (p.columnWidthCells - p.gutterWidthCells).max(1)).getOrElse(contentRect.width)
    // A column's own snapshot is expressed in its own TEXT width (band minus rail), so its cell size is
    // that text width per its own cell count; a non-column pane keeps the pane-width divisor it used.
    val cellWidthDivisor = columnTextWidthCells
    // With variable per-line heights a cell row no longer maps to a visual line, so reverse the click's
    // pixel Y through the same cumulative row geometry the renderer used. Falls back to the cell row when
    // there's no pixel Y (TUI) or the layout is uniform (cell mode).
    val rowMetrics = TextRowMetrics(
      contentRect = contentRect,
      gridMetrics = MouseHitTestGeometry.floatingCellMetrics(state),
      rowLineHeightPx = snapshot.lineHeightPx,
      usesMeasuredLayout = snapshot.usesMeasuredLayout,
      rowHeightsPx =
        snapshot.visualLines.map(line => if line.heightPx > 0 then line.heightPx else snapshot.lineHeightPx)
    )
    val visualRow = click.pixelY match
      case Some(pixelY) if snapshot.usesMeasuredLayout => rowMetrics.visualRowAt(pixelY)
      case _                                           => (click.row - contentRect.y).max(0)
    val cellWidthPx =
      if cellWidthDivisor > 0 then snapshot.panelWidthPx.toFloat / cellWidthDivisor.toFloat else 1.0f
    val columnOriginPx = (contentRect.x + columnXOriginCells + columnGutterCells) * cellWidthPx
    val rawXPx = click.pixelX match
      case Some(pixelX) => pixelX.toFloat - columnOriginPx
      case None         => (columnXCells - columnXOriginCells - columnGutterCells) * cellWidthPx
    // Clamp into the selected column's own text region: a click on its rail, in a gap, or off-page
    // resolves to that column's near text edge rather than reaching into (or past) it.
    val columnWidthPx = placement.map(_ => columnTextWidthCells * cellWidthPx).getOrElse(Float.MaxValue)
    val xPx           = rawXPx.max(0.0f).min(columnWidthPx)
    snapshot
      .cursorForVisualRowAndXPx(visualRow, xPx.max(0.0f))
      .orElse {
        // Below the last rendered visual row (#1547): the click's X still lands inside a wrap
        // group, so it must resolve against that group's LAST visual row -- reusing the raw
        // click column as a direct offset into the logical line (as the line-count-only clamp
        // below does) is only correct for a wrap group's first row, putting the cursor at the
        // logical start of whatever line the click falls back to instead of its lowest row.
        snapshot.visualLines.lastOption
          .map(lastVisualLine =>
            CursorPosition(lastVisualLine.bufferLine, lastVisualLine.nearestColumnForXPx(xPx.max(0.0f)))
          )
          .orElse {
            val bufferLine  = (vp.topLine + visualRow).max(0)
            val bufferCol   = (vp.leftColumn + (click.col - contentRect.x)).max(0)
            val clampedLine = bufferLine.min(math.max(0, buffer.document.content.lineCount - 1))
            val lineLen     = buffer.document.content.getLine(clampedLine).getOrElse("").length
            Some(CursorPosition(clampedLine, bufferCol.min(lineLen)))
          }
      }

  def wordSelectionAtCursor(buffer: Buffer, cursor: CursorPosition): Option[Selection] =
    val source        = RopeCharacterSource(buffer.document.content)
    val clickedOffset = buffer.document.content.lineColumnToOffset(cursor.line, cursor.column)
    if source.length == 0 then None
    else
      val probeOffset =
        if clickedOffset >= source.length then source.length - 1
        else if source.charAt(clickedOffset).isWhitespace && clickedOffset > 0 && !source
              .charAt(clickedOffset - 1)
              .isWhitespace
        then clickedOffset - 1
        else clickedOffset
      if probeOffset < 0 || source.charAt(probeOffset).isWhitespace then None
      else
        val start = TextEditing.previousWordBoundary(source, probeOffset)
        @annotation.tailrec
        def wordEndFrom(offset: Int): Int =
          if offset < source.length && !source.charAt(offset).isWhitespace then wordEndFrom(offset + 1)
          else offset
        val end = wordEndFrom(probeOffset)
        Some(
          Selection(
            offsetToCursorPosition(buffer.document.content, start),
            offsetToCursorPosition(buffer.document.content, end)
          )
        )

  def lineSelectionAtCursor(buffer: Buffer, cursor: CursorPosition): Option[Selection] =
    val lineText = buffer.document.content.getLine(cursor.line).getOrElse("")
    Some(
      Selection(
        CursorPosition(cursor.line, 0),
        CursorPosition(cursor.line, lineText.length)
      )
    )

  def rangeSelectionFromAnchor(buffer: Buffer, focus: CursorPosition): Option[Selection] =
    val anchor =
      buffer.primarySelection.map(_.anchor).orElse(buffer.editing.cursorPositions.headOption).getOrElse(focus)
    Option.when(anchor != focus)(Selection(anchor, focus))

  private def offsetToCursorPosition(content: com.serenity.rope.Rope, offset: Int): CursorPosition =
    val (line, column) = content.offsetToLineColumn(offset)
    CursorPosition(line, column)

  final private case class RopeCharacterSource(content: com.serenity.rope.Rope) extends TextEditing.CharacterSource:
    override def length: Int =
      content.weight

    override def charAt(index: Int): Char =
      content.index(index).getOrElse('\u0000')
