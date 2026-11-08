package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.state.models.{AppState, TypographyRole}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.renderer.{MeasuredTextAdvances, SurfaceTextInset}

/** What a line of dialog text is measured against: terminal cells, or the pixels of the GUI font it is drawn in.
  * `cellPx` is the grid cell a frame's width is counted in, and `insetPx` the room a GUI frame's text inset takes from
  * each row.
  */
enum TextMeasure:
  case Cells

  case Pixels(
      font: Font,
      fontRenderContext: FontRenderContext,
      cellPx: Int,
      insetPx: Double,
      advances: MeasuredTextAdvances
  )

object TextMeasure:

  def pixelsFor(state: AppState, advances: MeasuredTextAdvances): Pixels =
    val fontConfig = state.persisted.config.editorConfig.fontConfig
    Pixels(
      FontLoader.previewUiFont(fontConfig),
      TextLayoutSnapshot.defaultFontRenderContext(),
      CellMetrics.fromFont(FontLoader.previewFontForRole(fontConfig, TypographyRole.Code)).charWidth,
      SurfaceTextInset.px(state.persisted.config),
      advances
    )

object TextWrapping:

  /** `text` broken into rows that fit `widthCells`, at spaces where it can and mid-word where a word alone is too long.
    * A paragraph's leading spaces indent every row it wraps to; an empty text is one empty row.
    */
  def wrap(text: String, widthCells: Int, measure: TextMeasure = TextMeasure.Cells): List[String] =
    CellWrapping.wrap(text, fitsIn(math.max(1, widthCells), measure))

  private def fitsIn(widthCells: Int, measure: TextMeasure): String => Boolean =
    measure match
      case TextMeasure.Cells => CellWrapping.fitsCells(widthCells)
      case TextMeasure.Pixels(font, frc, cellPx, insetPx, advances) =>
        val limitPx = math.max(1.0, widthCells * cellPx - insetPx)
        row => advances.advanceOf(row, font, frc) <= limitPx
