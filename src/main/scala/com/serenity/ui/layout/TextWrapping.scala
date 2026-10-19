package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext
import java.util.concurrent.ConcurrentHashMap

import scala.annotation.tailrec

import com.serenity.state.models.{AppState, TypographyRole}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.renderer.{MeasuredTextAdvances, SurfaceTextInset}

/** What a line of dialog text is measured against: terminal cells, or the pixels of the GUI font it is drawn in.
  * `cellPx` is the grid cell a frame's width is counted in, and `insetPx` the room a GUI frame's text inset takes from
  * each row.
  */
enum TextMeasure:
  case Cells
  case Pixels(font: Font, fontRenderContext: FontRenderContext, cellPx: Int, insetPx: Double)

object TextMeasure:

  final private case class GuiFont(font: Font, cellPx: Int)

  private val guiFonts = ConcurrentHashMap[FontConfig, GuiFont]()

  private val GuiWrapsInPixels = false

  /** Pixel measuring needs a font to measure against, which a terminal does not have. */
  def forState(state: AppState): TextMeasure =
    if GuiWrapsInPixels && !state.runtime.capabilities.isCellGrid then pixelsFor(state) else Cells

  def pixelsFor(state: AppState): Pixels =
    val fontConfig = state.persisted.config.editorConfig.fontConfig
    val gui = guiFonts.computeIfAbsent(
      fontConfig,
      (config: FontConfig) =>
        GuiFont(
          FontLoader.previewUiFont(config),
          CellMetrics.fromFont(FontLoader.previewFontForRole(config, TypographyRole.Code)).charWidth
        )
    )
    Pixels(
      gui.font,
      TextLayoutSnapshot.defaultFontRenderContext(),
      gui.cellPx,
      SurfaceTextInset.px(state.persisted.config)
    )

object TextWrapping:

  /** `text` broken into rows that fit `widthCells`, at spaces where it can and mid-word where a word alone is too long.
    * A paragraph's leading spaces indent every row it wraps to; an empty text is one empty row.
    */
  def wrap(text: String, widthCells: Int, measure: TextMeasure = TextMeasure.Cells): List[String] =
    val fits   = fitsIn(math.max(1, widthCells), measure)
    val indent = text.takeWhile(_ == ' ')
    val prefix = if fits(indent + "x") then indent else ""
    val rows = text
      .split(' ')
      .filter(_.nonEmpty)
      .foldLeft(List.empty[String]) { (rows, word) =>
        rows match
          case current :: earlier if fits(s"$current $word") =>
            s"$current $word" :: earlier
          case _ => breakToFit(word, prefix, fits).reverse ::: rows
      }
      .reverse
    if rows.isEmpty then List("") else rows

  private def fitsIn(widthCells: Int, measure: TextMeasure): String => Boolean =
    measure match
      case TextMeasure.Cells => row => cellsOf(row) <= widthCells
      case TextMeasure.Pixels(font, frc, cellPx, insetPx) =>
        val limitPx = math.max(1.0, widthCells * cellPx - insetPx)
        row => MeasuredTextAdvances.shared.advanceOf(row, font, frc) <= limitPx

  private def cellsOf(row: String): Int =
    row.codePoints().map(codePoint => if codePoint < NarrowBelow then 1 else CharWidth.of(codePoint)).sum()

  // Nothing below Hangul Jamo is East Asian wide, so the common case never reaches the ICU property lookup.
  private val NarrowBelow = 0x1100

  /** `word` as rows, each starting with `prefix` and as long as still fits -- one code point at least, so a row too
    * narrow for any glyph still makes progress.
    */
  private def breakToFit(word: String, prefix: String, fits: String => Boolean): List[String] =
    @tailrec
    def fittingEnd(rest: String, end: Int): Int =
      if end >= rest.length then end
      else
        val next = end + Character.charCount(rest.codePointAt(end))
        if fits(prefix + rest.substring(0, next)) then fittingEnd(rest, next) else end
    @tailrec
    def pieces(rest: String, done: List[String]): List[String] =
      if rest.isEmpty then done.reverse
      else
        val end = math.max(Character.charCount(rest.codePointAt(0)), fittingEnd(rest, 0))
        pieces(rest.substring(end), (prefix + rest.substring(0, end)) :: done)
    pieces(word, Nil)
