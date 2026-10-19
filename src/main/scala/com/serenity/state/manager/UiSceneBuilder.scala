package com.serenity.state.manager

import java.awt.Font

import scala.annotation.tailrec

import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Lays out a whole [[AppState]] into the [[UiSceneSnapshot]] that rendering and mouse targeting share. Pure apart from
  * the shared [[WrappedLineCache]], so [[AuthoritativeUiScene]] can run it without holding any lock.
  */
private[serenity] object UiSceneBuilder:

  type Build = (AppState, ViewportSize, Font, Font, Option[CellMetrics]) => UiSceneSnapshot

  def apply(wrappedLines: WrappedLineCache): Build =
    (state, viewportSize, codeFont, textFont, cellMetrics) =>
      build(wrappedLines, state, viewportSize, codeFont, textFont, cellMetrics)

  private def build(
    wrappedLines: WrappedLineCache,
    state: AppState,
    viewportSize: ViewportSize,
    codeFont: Font,
    textFont: Font,
    cellMetrics: Option[CellMetrics]
  ): UiSceneSnapshot =
    val layout           = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
    val base             = UiSceneSnapshot.from(state, layout, viewportSize)
    val surfaceConfig    = state.persisted.config.surfaceConfig
    val columnModeActive = surfaceConfig.columnModeEnabled && surfaceConfig.wordWrapEnabled
    val perPane = base.paneLayouts.flatMap {
      case (paneId, paneLayout) =>
        for
          pane     <- state.persisted.layout.editorPanes.get(paneId)
          bufferId <- pane.bufferId
          buffer   <- state.persisted.buffers.get(bufferId)
        yield
          val laidOut = PaneTextLayout.build(
            state,
            buffer,
            paneLayout.contentRect,
            codeFont,
            textFont,
            cellMetrics,
            wrappedLines
          )
          val scroll =
            Option.when(!columnModeActive && ScrollbarLayout.hasBar(state, paneLayout))(
              paneScroll(
                state,
                buffer,
                laidOut.viewport,
                laidOut.snapshot,
                laidOut.widthPx,
                laidOut.font,
                laidOut.fontMetrics,
                laidOut.forceCellLayout,
                wrappedLines
              )
            )
          paneId -> (laidOut.snapshot, laidOut.columns, scroll)
    }
    val textSnapshots   = perPane.view.mapValues(_._1).toMap
    val columnSnapshots = perPane.view.mapValues(_._2).filter(_._2.nonEmpty).toMap
    val paneScrolls     = perPane.flatMap((paneId, parts) => parts._3.map(paneId -> _))
    base.withTextSnapshots(textSnapshots).withColumnSnapshots(columnSnapshots).withPaneScrolls(paneScrolls)

  /** Counted as the wrap that laid the pane's rows out, so the bar's rows and the viewport's agree. The lines the
    * snapshot has just drawn whole are counted from it rather than wrapped a second time, which keeps an edit's cost to
    * the bar at the edit's own line.
    */
  private def paneScroll(
    state: AppState,
    buffer: Buffer,
    viewport: Viewport,
    drawn: TextLayoutSnapshot,
    widthPx: Int,
    font: Font,
    fontMetrics: CellMetrics,
    forceCellLayout: Boolean,
    wrappedLines: WrappedLineCache
  ): PaneScroll =
    val surfaceConfig = state.persisted.config.surfaceConfig
    val rows =
      if !surfaceConfig.wordWrapEnabled then VisualRowCounts.oneRowPerLine(buffer.document.content.lineCount)
      else
        val wholeLines = wholeLinesDrawn(drawn)
        VisualRowCounts.forBuffer(
          buffer,
          widthPx,
          font,
          Some(fontMetrics),
          forceCellLayout,
          wrappedLines,
          state.persisted.config.documentConfig.dropCapsEnabled,
          state.persisted.config.inlineMarkdownViewMode,
          buffer.effectiveIndentation(state.persisted.config.indentation).tabSize,
          known = wholeLines.get,
          headings = state.effectiveHeadingSettings,
          openings = buffer.effectiveOpenings(state.persisted.config.openingsConfig),
          dropCapGapEm = state.persisted.config.documentConfig.dropCapGapEm,
          fallback = state.persisted.config.editorConfig.fontConfig.fallback
        )
    PaneScroll.of(rows, viewport.topLine, viewport.topVisualLine, viewport.visibleLines)

  /** The rows of each line `snapshot` draws from its first row to its last: not the first when the view starts part-way
    * through it, nor the last, which the snapshot may have cut short.
    */
  private def wholeLinesDrawn(snapshot: TextLayoutSnapshot): Map[Int, Int] =
    val drawn = snapshot.visualLines
    @tailrec
    def from(first: Int, whole: Map[Int, Int]): Map[Int, Int] =
      drawn.lift(first) match
        case None => whole
        case Some(row) =>
          val next = drawn.indexWhere(_.bufferLine != row.bufferLine, first)
          if next < 0 then whole
          else from(next, if row.startColumn == 0 then whole.updated(row.bufferLine, next - first) else whole)
    from(0, Map.empty)
