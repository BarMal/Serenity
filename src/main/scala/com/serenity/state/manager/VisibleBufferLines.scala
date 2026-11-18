package com.serenity.state.manager

import com.serenity.lsp.model.LineRange
import com.serenity.state.models.{AppState, Buffer, TypographyRole}

/** The buffer lines a pane may be showing, for a language server asked only about what is on screen.
  *
  * `Viewport.topLine` is the first buffer line shown (a wrapped line may be scrolled into by `topVisualLine`, but it is
  * still that line), and `Viewport.visibleLines` counts rows of the code grid, not buffer lines: a wrapped line takes
  * several rows, so the last line shown is at or before `topLine + rows - 1`. The range is therefore an upper bound
  * once lines wrap, and exact when they do not. Column mode lays out several screens of rows side by side, so there the
  * range runs to the end of the document rather than guess a column count.
  */
object VisibleBufferLines:

  /** The lines a screen of `rows` rows starting at `topLine` can reach, within a document of `lineCount` lines. */
  def range(topLine: Int, rows: Int, lineCount: Int): LineRange =
    val lastLine = math.max(0, lineCount - 1)
    val first    = math.min(math.max(0, topLine), lastLine)
    LineRange(first, math.min(lastLine, first + math.max(1, rows) - 1))

  /** `lineHeightOf` is the line height of the font a role is drawn in, which the caller reads off the owner's measurer:
    * this object is below the layout in the module graph.
    */
  def of(buffer: Buffer, state: AppState, lineHeightOf: TypographyRole => Int): LineRange =
    val lineCount = buffer.document.content.lineCount
    val surface   = state.persisted.config.surfaceConfig
    val rows =
      if surface.columnModeEnabled && surface.wordWrapEnabled then lineCount
      else rowsFitting(buffer, state, lineHeightOf)
    range(buffer.viewport.topLine, rows, lineCount)

  /** Rows of the font the buffer is drawn in that fit the pane. `visibleLines` is sized on the code font's line height
    * (`LayoutEngine.updateViewportDimensions`), so a buffer drawn in a font with a different line height fits
    * proportionally more or fewer rows -- the same conversion `CursorViewport` scrolls by. A cell grid has one row
    * height whatever the font.
    */
  private def rowsFitting(buffer: Buffer, state: AppState, lineHeightOf: TypographyRole => Int): Int =
    val visibleLines = buffer.viewport.visibleLines
    if state.runtime.capabilities.isCellGrid then visibleLines
    else
      val codeHeight   = lineHeightOf(TypographyRole.Code)
      val bufferHeight = lineHeightOf(buffer.typographyRole)
      math.max(1, visibleLines * codeHeight / math.max(1, bufferHeight))
