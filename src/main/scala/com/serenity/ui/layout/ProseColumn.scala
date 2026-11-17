package com.serenity.ui.layout

import java.awt.Font
import java.awt.image.BufferedImage

import com.serenity.config.WrapColumn
import com.serenity.state.models.{AppState, TypographyRole}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.renderer.{FontFirstUse, FontSpec}

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
    memos.measurer.fold(awtChAdvancePx(font))(_.glyphWidthPx(FontSpec.fromAwt(font), "0").toDouble.max(1.0))

  /** Measured the way [[CellMetrics.fromFont]] measures the grid cell, so the two widths share a rendering context. */
  private def awtChAdvancePx(font: Font): Double =
    val graphics = BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB).createGraphics()
    graphics.setFont(font)
    FontFirstUse.settle(font, graphics.getFontRenderContext)
    val advance = font.getStringBounds("0", graphics.getFontRenderContext).getWidth
    graphics.dispose()
    advance.max(1.0)
