package com.serenity.state.manager

import com.serenity.config.AppConfig
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{LayoutRect, TextLayoutSnapshot}

/** Selection grow/settle (issue #1085 phase 3): `rectsForSelection` is the one place a [[Selection]] is turned into the
  * per-visual-line column extents [[SelectionGeometryState.diff]] tweens between -- reusing the same word-wrap
  * measurement `CursorGlideGeometry.paneRelativePosition`/`CursorViewport.adjustForCursor` already do, so this never
  * disagrees with them about where a wrapped line's rows fall.
  *
  * Each returned [[LayoutRect]] is a *column* extent local to its own visual line (`x` the buffer column the highlight
  * starts at, `width` its column span), not a screen-pixel rect -- see `SelectionGeometryState`'s own doc comment for
  * why this is what lets one model serve both the GUI's sub-pixel-measured painting and TUI's whole-cell painting.
  */
private[manager] object SelectionGeometry:

  def rectsForSelection(
    buffer: Buffer,
    config: AppConfig,
    selection: Selection
  ): Map[SelectionLineKey, LayoutRect] =
    val fontConfig      = config.editorConfig.fontConfig
    val font            = FontLoader.previewFontForRole(fontConfig, buffer.typographyRole)
    val wordWrapEnabled = config.surfaceConfig.wordWrapEnabled
    val wrapWidthPx     = TextLayoutSnapshot.gridWrapWidthPx(buffer.viewport.visibleColumns, fontConfig)
    val start           = selection.start
    val end             = selection.end

    def visualLinesForLine(lineIndex: Int): Vector[TextVisualLine] =
      val text = buffer.document.content.getLine(lineIndex).getOrElse("")
      if wordWrapEnabled then TextLayoutSnapshot.boundedVisualLinesForText(text, lineIndex, wrapWidthPx, font)
      else
        Vector(
          TextVisualLine(
            bufferLine = lineIndex,
            startColumn = 0,
            endColumn = text.length,
            text = text,
            widthPx = 0.0f,
            caretStops = Vector.empty
          )
        )

    (start.line to end.line).flatMap { lineIndex =>
      visualLinesForLine(lineIndex).flatMap { visualLine =>
        val rangeStart   = if lineIndex == start.line then start.column else visualLine.startColumn
        val rangeEnd     = if lineIndex == end.line then end.column else visualLine.endColumn
        val clippedStart = math.max(rangeStart, visualLine.startColumn)
        val clippedEnd   = math.min(rangeEnd, visualLine.endColumn)
        Option.when(clippedStart < clippedEnd)(
          SelectionLineKey(visualLine.bufferLine, visualLine.startColumn) ->
            LayoutRect(x = clippedStart, y = 0, width = clippedEnd - clippedStart, height = 1)
        )
      }
    }.toMap
