package com.serenity.state.manager

import com.serenity.config.AppConfig
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, PixelPoint, TextLayoutSnapshot}

/** Caret-glide (issue #1085 phase 2): the caret's pixel position *within its own pane's content area* -- relative to
  * the pane's top-left content origin, not the pane's absolute on-screen rect.
  *
  * The renderer's actual screen position additionally depends on the pane's on-screen rect (`RendererCursorGlyphs`'s
  * `screenXPx`/`screenYPx`), which only exists as output of a full `LayoutEngine.calculateLayoutWithUI` pass -- run
  * once per frame by the renderer, not by the state manager on every cursor move. Measuring pane-relative here instead
  * (using the same `TextLayoutSnapshot`/`CellMetrics` font measurement `CursorViewport.adjustForCursor` already does,
  * not the layout engine) and letting `RendererCursorGlyphs` add whatever the pane's own rect is *that frame* keeps a
  * glide geometrically correct against wherever the pane currently sits, even if a resize or panel toggle moves it
  * mid-flight -- at the cost of a glide that happens to be mid-flight exactly when the pane relocates briefly aiming at
  * where its target sat *within* the pane rather than the literal screen pixels it occupied a moment ago. Invisible in
  * practice given how short a glide runs, and far cheaper than re-running the full layout engine on every cursor move
  * the way pane geometry (rare, whole-panel) can afford to.
  *
  * Row heights are treated as uniform (`CellMetrics.fromFont(font).lineHeight`) even though a heading row can measure
  * taller (`RendererCursorGlyphs`'s prose-scale caret height, #1542) -- the same approximation
  * `CursorViewport.adjustForCursor` already makes for its own scroll-centring arithmetic, and one a short glide across
  * at most a couple of rows does not make visible.
  *
  * GUI-canvas-only by construction: TUI mode measures glyphs with no real font to draw them, so `CursorViewport` never
  * calls this while `state.runtime.isTuiMode` holds.
  */
private[manager] object CursorGlideGeometry:

  def paneRelativePosition(buffer: Buffer, config: AppConfig, cursor: CursorPosition): PixelPoint =
    val viewport        = buffer.viewport
    val fontConfig      = config.editorConfig.fontConfig
    val font            = FontLoader.previewFontForRole(fontConfig, buffer.typographyRole)
    val wordWrapEnabled = config.surfaceConfig.wordWrapEnabled
    val wrapWidthPx     = TextLayoutSnapshot.gridWrapWidthPx(viewport.visibleColumns, fontConfig)
    val lineHeightPx    = CellMetrics.fromFont(font).lineHeight

    def lineText(lineIndex: Int): String = buffer.document.content.getLine(lineIndex).getOrElse("")

    def visualRowCountForLine(lineIndex: Int): Int =
      if !wordWrapEnabled then 1
      else TextLayoutSnapshot.boundedVisualLinesForText(lineText(lineIndex), lineIndex, wrapWidthPx, font).length.max(1)

    val cursorVisualLineWithinItsLine =
      if !wordWrapEnabled then 0
      else
        TextLayoutSnapshot.visualLineIndexForCursor(
          lineText(cursor.line),
          cursor.column,
          wrapWidthPx,
          font,
          wordWrapEnabled = true,
          rowAffinity = cursor.rowAffinity
        )

    val rowsBetweenTopAndCursorLine =
      if cursor.line == viewport.topLine then 0
      else if cursor.line > viewport.topLine then (viewport.topLine until cursor.line).map(visualRowCountForLine).sum
      else -(cursor.line until viewport.topLine).map(visualRowCountForLine).sum

    val visualRowOffset =
      if !wordWrapEnabled then cursor.line - viewport.topLine
      else rowsBetweenTopAndCursorLine + cursorVisualLineWithinItsLine - viewport.topVisualLine

    val safeColumn = cursor.column.max(0)
    val xPxRaw =
      if wordWrapEnabled then
        TextLayoutSnapshot
          .boundedVisualLinesForText(lineText(cursor.line), cursor.line, wrapWidthPx, font)
          .lift(cursorVisualLineWithinItsLine)
          .flatMap(_.xForColumn(cursor.column))
          .getOrElse(0.0f)
      else TextLayoutSnapshot.caretXsForText(lineText(cursor.line), font).lift(safeColumn).getOrElse(0.0f)
    // Horizontal scroll (non-wrap mode only -- word wrap always keeps `leftColumn` at 0) shifts the line's rendered
    // start left of column 0, by `viewport.leftColumn` columns measured in the font's own average advance -- the same
    // unit `CursorViewport`'s own leftColumn clamp already treats a "column" as for this font.
    val scrollOffsetPx =
      if wordWrapEnabled then 0.0f else viewport.leftColumn.max(0) * CellMetrics.fromFont(font).charWidth.toFloat

    PixelPoint(xPx = math.round(xPxRaw - scrollOffsetPx), yPx = visualRowOffset * lineHeightPx)
