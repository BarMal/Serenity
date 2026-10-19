package com.serenity.ui.layout

import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.renderer.{RendererCursorGlyphs, RendererPaneContent}

/** The editor panes of one layout pass, with the text each lays out to built on demand and at most once. A floating
  * surface hangs off [[screenPosition]], which answers with the cell the caret is painted on -- the same snapshot,
  * [[TextRowMetrics]] and caret placement the renderer uses -- so heading space, word wrap and measured row heights
  * move a surface exactly as they move the caret.
  */
final class LaidOutPanes(
    state: AppState,
    val layouts: Map[PaneId, EditorPaneLayout],
    wrappedLines: WrappedLineCache
):

  private val textLayouts: Map[PaneId, LaidOutPanes.Once[Option[PaneTextLayout]]] =
    layouts.map((paneId, paneLayout) => paneId -> LaidOutPanes.Once(buildTextLayout(paneId, paneLayout.contentRect)))

  private def buildTextLayout(paneId: PaneId, contentRect: LayoutRect): Option[PaneTextLayout] =
    val fontConfig = state.persisted.config.editorConfig.fontConfig
    for
      buffer <- bufferOf(paneId)
      codeFont = FontLoader.previewFontForRole(fontConfig, TypographyRole.Code)
      textFont = FontLoader.previewFontForRole(fontConfig, TypographyRole.Prose)
    yield PaneTextLayout.build(
      state,
      buffer,
      contentRect,
      codeFont,
      textFont,
      Option.when(state.runtime.capabilities.isCellGrid)(CellMetrics.cellUnit),
      wrappedLines
    )

  private def bufferOf(paneId: PaneId): Option[Buffer] =
    state.persisted.layout.editorPanes
      .get(paneId)
      .flatMap(_.bufferId)
      .flatMap(state.persisted.buffers.get)

  /** The cell `position` is painted on in pane `paneId`, or `None` when the pane does not draw it. */
  def screenPosition(paneId: PaneId, position: CursorPosition): Option[ScreenPosition] =
    for
      contentRect <- layouts.get(paneId).map(_.contentRect)
      text        <- textLayouts.get(paneId).flatMap(_.value)
      (snapshot, rect) <-
        if text.columns.isEmpty then Some(text.snapshot -> contentRect)
        else
          RendererCursorGlyphs
            .owningColumn(text.columns, position)
            .map(placement => placement.snapshot -> RendererPaneContent.columnRect(contentRect, placement))
      rowMetrics = TextRowMetrics.forSnapshot(rect, text.gridMetrics, snapshot, snapshot.usesMeasuredLayout)
      (visualRow, caret) <- RendererCursorGlyphs.caretPlacement(position, rect, text.gridMetrics, snapshot, rowMetrics)
      if rowMetrics.lineFits(visualRow)
    yield ScreenPosition(text.gridMetrics.toCol(caret.xPx), text.gridMetrics.toRow(caret.yPx))

object LaidOutPanes:

  final private class Once[A](compute: => A):
    lazy val value: A = compute
