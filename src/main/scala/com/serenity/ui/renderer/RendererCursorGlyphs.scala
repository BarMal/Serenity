package com.serenity.ui.renderer

import com.serenity.config.AppConfig
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme

/** Paints the caret glyph(s) for a buffer's cursors, in both the pane content path and the markdown-lens path
  * ([[RendererMarkdownLens.renderMarkdownLensCursors]]), and the small geometry helpers shared with the highlight
  * renderers for clipping a measured run to its pane.
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
    snapshot: TextLayoutSnapshot
  ): List[PixelRect] =

    buffer.editing.cursors.zipWithIndex.flatMap { (cursor, cursorIndex) =>
      val isPrimaryCursor = cursorIndex == 0
      val shouldRenderCursor =
        context.cursorVisible || (buffer.editing.cursors.size > 1 && !isPrimaryCursor)
      calculateCursorVisualPosition(cursor, snapshot) match
        case Some((visualLine, xPx)) if shouldRenderCursor =>
          if RendererPaneContent.visualLineVisible(rect, visualLine, context, snapshot)
          then
            val effectiveCursorColor = cursorColorFor(config, theme, context, isPrimaryCursor)
            val caretWidthPx         = math.max(2, math.round(context.cellMetrics.charWidth * 0.12f))
            val screenXPx            = context.cellMetrics.toPixelX(rect.x) + math.round(xPx)
            val screenYPx =
              RendererPaneContent.textRowMetrics(rect, context, snapshot).cursorTopPx(visualLine)
            caretWithin(rect, context.cellMetrics, screenXPx, caretWidthPx) match
              case Some((caretXPx, widthPx)) =>
                context.surface.pixels.fillPixelRect(
                  caretXPx,
                  screenYPx,
                  widthPx,
                  snapshot.lineHeightPx,
                  effectiveCursorColor
                )
                List(PixelRect(caretXPx, screenYPx, widthPx, snapshot.lineHeightPx))
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

  private[renderer] def measuredRunWidthWithin(
    rect: LayoutRect,
    context: RenderContext,
    startXPx: Float,
    endXPx: Float
  ): Option[Float] =
    val rightXPx = context.cellMetrics.toPixelX(rect.right).toFloat
    Option.when(startXPx < rightXPx)(math.max(0.0f, math.min(endXPx, rightXPx) - startXPx)).filter(_ > 0.0f)

  private[renderer] def caretWithin(
    rect: LayoutRect,
    cellMetrics: CellMetrics,
    desiredXPx: Int,
    desiredWidthPx: Int
  ): Option[(Int, Int)] =
    val leftXPx  = cellMetrics.toPixelX(rect.x)
    val rightXPx = cellMetrics.toPixelX(rect.right)
    val widthPx  = math.min(math.max(1, desiredWidthPx), rightXPx - leftXPx)
    Option.when(widthPx > 0)(desiredXPx.max(leftXPx).min(rightXPx - widthPx) -> widthPx)

  private[renderer] def cursorColorFor(
    config: AppConfig,
    theme: Theme,
    context: RenderContext,
    isPrimaryCursor: Boolean
  ): java.awt.Color =
    val activeColor = context.cursorColorOverride.getOrElse(config.cursorColors.activeOr(theme.cursor))
    if isPrimaryCursor then activeColor
    else config.cursorColors.inactiveOr(activeColor)
