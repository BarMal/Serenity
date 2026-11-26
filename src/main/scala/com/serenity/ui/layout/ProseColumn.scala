package com.serenity.ui.layout

import java.awt.Font

import com.serenity.config.WrapColumn
import com.serenity.state.models.{AppState, EditingContext, PaneId, TypographyRole}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.renderer.FontSpec

/** The wrap column as a width on the editor grid, whose cells are the code font's whatever font a buffer draws in. */
object ProseColumn:

  final private[layout] case class GlyphWidths(chPx: Double, gridCellPx: Int)

  /** Grid cells a `measure`-character column needs, where a character is the prose font's `0` advance -- CSS's `ch`.
    */
  def cells(measure: Int, chWidthPx: Double, gridCellWidthPx: Int): Int =
    math.ceil(measure * chWidthPx / gridCellWidthPx.max(1)).toInt.max(1)

  /** The wrap column in grid cells, whatever the mode; `None` at the edge. */
  def wrapCells(state: AppState): Option[Int] =
    state.persisted.config.surfaceConfig.wrapColumn match
      case WrapColumn.Edge                                               => None
      case WrapColumn.Cells(count)                                       => Some(count)
      case WrapColumn.Ch(count) if state.runtime.capabilities.isCellGrid => Some(count)
      case WrapColumn.Ch(count) =>
        val widths = glyphWidths(state)
        Some(cells(count, widths.chPx, widths.gridCellPx))

  /** The most the text area is capped to, in grid cells: the wrap column, except in column mode, where it is the width
    * of one column and the area holds however many fit.
    */
  def widthCells(state: AppState): Option[Int] =
    val surfaceConfig = state.persisted.config.surfaceConfig
    if surfaceConfig.columnModeEnabled && surfaceConfig.wordWrapEnabled then None else wrapCells(state)

  /** [[widthCells]] for the text area of one pane: code is not set in a column, so a pane showing it is not capped,
    * whatever the workspace's mode. The pane's own buffer decides, not the app mode.
    */
  def widthCellsFor(state: AppState, paneId: PaneId): Option[Int] =
    if paneShowsCode(state, paneId) then None else widthCells(state)

  private def paneShowsCode(state: AppState, paneId: PaneId): Boolean =
    state.persisted.layout.editorPanes
      .get(paneId)
      .flatMap(_.bufferId)
      .flatMap(state.persisted.buffers.get)
      .exists(buffer => !EditingContext.bufferKind(buffer).isProse)

  /** How many grid cells one `ch` is: the prose font's `0` advance over the grid cell, and exactly one in a terminal.
    */
  def chInCells(state: AppState): Double =
    if state.runtime.capabilities.isCellGrid then 1.0
    else
      val widths = glyphWidths(state)
      widths.chPx / widths.gridCellPx.max(1)

  /** Left and right spacer widths that leave at most `columnCells` of `availableWidth` for text, centred, never
    * narrower than the inset spacers already asked for.
    */
  def spacers(availableWidth: Int, insetLeft: Int, insetRight: Int, columnCells: Option[Int]): (Int, Int) =
    columnCells.fold((insetLeft, insetRight)) { column =>
      val spare     = (availableWidth - column).max(0)
      val spareLeft = spare / 2
      (insetLeft.max(spareLeft), insetRight.max(spare - spareLeft))
    }

  /** Both widths come from the owner's measurer when the state has one, which is what the grid and the prose are laid
    * out with; without one, from AWT as before.
    */
  private def glyphWidths(state: AppState): GlyphWidths =
    val config = state.persisted.config.editorConfig.fontConfig
    val memos  = state.runtime.anchorWraps.lines.layoutMemos
    state.runtime.proseGlyphWidths.of(config)(
      GlyphWidths(
        chPx = chAdvancePx(FontLoader.previewTextFont(config), memos),
        gridCellPx =
          TextLayoutSnapshot.cellMetricsOf(FontLoader.previewFontForRole(config, TypographyRole.Code), memos).charWidth
      )
    )

  private def chAdvancePx(font: Font, memos: LayoutMemos): Double =
    memos.measurer.fold(CellMetrics.cellUnit.charWidth.toDouble)(
      _.glyphWidthPx(FontSpec.fromAwt(font), "0").toDouble.max(1.0)
    )
