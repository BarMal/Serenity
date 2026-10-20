package com.serenity.ui.layout

import java.awt.Font
import java.awt.image.BufferedImage

import com.serenity.config.AppMode
import com.serenity.state.models.{AppState, TypographyRole}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.renderer.FontFirstUse

/** The prose measure as a width on the editor grid, whose cells are the code font's whatever font a buffer draws in. */
object ProseColumn:

  final private[layout] case class GlyphWidths(chPx: Double, gridCellPx: Int)

  /** Grid cells a `measure`-character column needs, where a character is the prose font's `0` advance -- CSS's `ch`.
    */
  def cells(measure: Int, chWidthPx: Double, gridCellWidthPx: Int): Int =
    math.ceil(measure * chWidthPx / gridCellWidthPx.max(1)).toInt.max(1)

  /** The text column's width in grid cells while a prose workspace has a measure set. */
  def widthCells(state: AppState): Option[Int] =
    val config = state.persisted.config
    config.surfaceConfig.proseMeasure.filter(_ => config.appMode == AppMode.Prose).map { measure =>
      if state.runtime.capabilities.isCellGrid then measure
      else
        val widths = glyphWidths(config.editorConfig.fontConfig, state.runtime.proseGlyphWidths)
        cells(measure, widths.chPx, widths.gridCellPx)
    }

  /** Left and right spacer widths that leave at most `columnCells` of `availableWidth` for text, centred, never
    * narrower than the inset spacers already asked for.
    */
  def spacers(availableWidth: Int, insetLeft: Int, insetRight: Int, columnCells: Option[Int]): (Int, Int) =
    columnCells.fold((insetLeft, insetRight)) { column =>
      val spare     = (availableWidth - column).max(0)
      val spareLeft = spare / 2
      (insetLeft.max(spareLeft), insetRight.max(spare - spareLeft))
    }

  private def glyphWidths(config: FontConfig, kept: ProseGlyphWidths): GlyphWidths =
    kept.of(config)(
      GlyphWidths(
        chPx = chAdvancePx(FontLoader.previewTextFont(config)),
        gridCellPx = CellMetrics.fromFont(FontLoader.previewFontForRole(config, TypographyRole.Code)).charWidth
      )
    )

  /** Measured the way [[CellMetrics.fromFont]] measures the grid cell, so the two widths share a rendering context. */
  private def chAdvancePx(font: Font): Double =
    val graphics = BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB).createGraphics()
    graphics.setFont(font)
    FontFirstUse.settle(font, graphics.getFontRenderContext)
    val advance = font.getStringBounds("0", graphics.getFontRenderContext).getWidth
    graphics.dispose()
    advance.max(1.0)
