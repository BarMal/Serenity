package com.serenity.ui.renderer

import com.serenity.animation.Interpolator.given
import com.serenity.config.AppConfig
import com.serenity.state.models.*
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
    isTuiMode: Boolean
  ): List[PixelRect] =

    buffer.editing.cursors.toList.zipWithIndex.flatMap { (cursor, cursorIndex) =>
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
            // Caret-glide (issue #1085 phase 2): while a glide is in flight, paint at its tweened pane-relative offset
            // from this frame's own pane origin instead of the cursor's plain logical position -- see
            // `CursorGlideGeometry`'s doc comment for why the offset is pane-relative rather than an absolute screen
            // pixel captured once at seed time.
            val (screenXPx, screenYPx) = glidePixelPosition(cursor, isTuiMode) match
              case Some(offset) =>
                (context.cellMetrics.toPixelX(rect.x) + offset.xPx, context.cellMetrics.toPixelY(rect.y) + offset.yPx)
              case None =>
                (context.cellMetrics.toPixelX(rect.x) + math.round(xPx), rowMetrics.cursorTopPx(visualLine))
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

  /** Caret-glide (issue #1085 phase 2): the pane-relative pixel offset to paint `cursor` at instead of its plain
    * logical position, or `None` to paint at the logical position unmoved -- GUI-canvas-only (TUI's caret always snaps
    * instantly, mirroring `RendererCursorOverlay.presentHardwareCursor`'s existing GUI/TUI split for hardware-cursor
    * delegation), and `None` once the glide has completed (normally already cleared by
    * `StateManagerEditorCapability.advanceCursorGlides`, checked again here defensively).
    */
  def glidePixelPosition(cursor: Cursor, isTuiMode: Boolean): Option[PixelPoint] =
    if isTuiMode then None else cursor.glide.filterNot(_.isComplete).map(_.currentValue)

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
  ): java.awt.Color =
    val activeColor = context.cursorColorOverride.getOrElse(config.cursorColors.activeOr(theme.cursor))
    if isPrimaryCursor then activeColor
    else config.cursorColors.inactiveOr(activeColor)
