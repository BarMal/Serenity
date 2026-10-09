package com.serenity.ui.renderer

import com.serenity.config.AppConfig
import com.serenity.state.models.*
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme

/** The package's caret authority: it paints the caret glyph(s) for a buffer's cursors, and owns the pane-clipping
  * geometry every caret and measured run in the package is placed by. [[caretWithin]]/[[cursorColorFor]] are public
  * because [[RendererMarkdownLens]] paints its own carets over the inline preview and must place and colour them
  * identically to the pane path; [[measuredRunWidthWithin]] is public because [[RendererHighlights]] clips selection
  * and diagnostic backgrounds to the same right edge a caret is clipped to, and a highlight that clipped differently
  * would bleed past the pane the caret stops at.
  */
object RendererCursorGlyphs:

  /** Paint every visible cursor in `buffer` and report the pixel rect each one occupies, so a caller can bound a
    * repaint to cover them.
    */
  def renderCursors(
    buffer: Buffer,
    rect: LayoutRect,
    theme: Theme,
    config: AppConfig,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    paintsCursor: CursorPosition => Boolean = _ => true
  ): List[PixelRect] =

    buffer.editing.cursors.toList.zipWithIndex.filter((cursor, _) => paintsCursor(cursor.position)).flatMap {
      (cursor, cursorIndex) =>
        val isPrimaryCursor = cursorIndex == 0
        val shouldRenderCursor =
          context.cursorVisible || (buffer.editing.cursors.size > 1 && !isPrimaryCursor)
        calculateCursorVisualPosition(cursor.position, snapshot) match
          case Some((visualLine, xPx)) if shouldRenderCursor =>
            if RendererPaneContent.visualLineVisible(rect, visualLine, context, snapshot)
            then
              val effectiveCursorColor = cursorColorFor(config, theme, context, isPrimaryCursor)
              val caretWidthPx         = math.max(2, math.round(context.cellMetrics.charWidth * 0.12f))
              val rowMetrics           = RendererPaneContent.textRowMetrics(rect, context, snapshot)
              val screenXPx            = context.cellMetrics.toPixelX(rect.x) + math.round(xPx)
              val screenYPx            = rowMetrics.cursorTopPx(visualLine)
              // Caret is as tall as the row it sits on, so on a heading line it grows with the heading (#1542 prose scale).
              val caretHeightPx = rowMetrics.rowHeightPx(visualLine)
              caretWithin(rect, context.cellMetrics, screenXPx, caretWidthPx) match
                case Some((caretXPx, widthPx)) =>
                  context.surface.pixels.fillPixelRect(
                    caretXPx,
                    screenYPx,
                    widthPx,
                    caretHeightPx,
                    effectiveCursorColor
                  )
                  List(PixelRect(caretXPx, screenYPx, widthPx, caretHeightPx))
                case None => Nil
            else Nil
          case _ => Nil
    }

  /** The (visual-row index, caret x) the caret is drawn at. Row selection is delegated to
    * [[com.serenity.state.models.NavigationGeometry.visualRowIndexFor]] -- the same lookup vertical navigation uses --
    * so the caret is never painted on a different wrapped row than Up/Down moves from at a wrap boundary (the "stuck
    * until nudged left/right" bug). Previously this used `collectFirst` (the *earlier* row at a boundary) while
    * navigation used `.lastOption` (the later row), so the two disagreed.
    */
  private def calculateCursorVisualPosition(
    cursor: CursorPosition,
    snapshot: TextLayoutSnapshot
  ): Option[(Int, Float)] =
    snapshot.navigationGeometry.visualRowIndexFor(cursor).map { visualIndex =>
      val line = snapshot.visualLines(visualIndex)
      val xPx  = line.xForColumn(cursor.column).getOrElse(line.widthPx)
      (visualIndex, xPx)
    }

  /** The column a caret belongs to. A wrapped line that continues into the next column leaves a cursor on the boundary
    * column matching both: the end of the earlier column's last row and the start of the later column's first. The
    * cursor's [[RowAffinity]] settles it exactly as it does between two rows of one snapshot -- downstream (the
    * default) is the later column, upstream the earlier -- so only one column paints it, and a terminal's single
    * hardware cursor is never placed from the wrong one.
    */
  def owningColumn(
    columnPlacements: Vector[ColumnSnapshotPlacement],
    position: CursorPosition
  ): Option[ColumnSnapshotPlacement] =
    val holding = columnPlacements.filter(_.snapshot.navigationGeometry.visualRowIndexFor(position).isDefined)
    position.rowAffinity match
      case RowAffinity.Upstream   => holding.headOption
      case RowAffinity.Downstream => holding.lastOption

  def measuredRunWidthWithin(
    rect: LayoutRect,
    context: RenderContext,
    startXPx: Float,
    endXPx: Float
  ): Option[Float] =
    val rightXPx = context.cellMetrics.toPixelX(rect.right).toFloat
    Option.when(startXPx < rightXPx)(math.max(0.0f, math.min(endXPx, rightXPx) - startXPx)).filter(_ > 0.0f)

  def caretWithin(
    rect: LayoutRect,
    cellMetrics: CellMetrics,
    desiredXPx: Int,
    desiredWidthPx: Int
  ): Option[(Int, Int)] =
    val leftXPx  = cellMetrics.toPixelX(rect.x)
    val rightXPx = cellMetrics.toPixelX(rect.right)
    val widthPx  = math.min(math.max(1, desiredWidthPx), rightXPx - leftXPx)
    Option.when(widthPx > 0)(desiredXPx.max(leftXPx).min(rightXPx - widthPx) -> widthPx)

  def cursorColorFor(
    config: AppConfig,
    theme: Theme,
    context: RenderContext,
    isPrimaryCursor: Boolean
  ): RenderColor =
    val activeColor =
      context.cursorColorOverride.getOrElse(config.cursorColors.active.fold(theme.cursor)(RenderColor.fromAwt))
    if isPrimaryCursor then activeColor
    else config.cursorColors.inactive.fold(activeColor)(RenderColor.fromAwt)
