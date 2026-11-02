package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import scala.annotation.tailrec

import com.serenity.ui.renderer.ShapedTextCache

/** The advance of each printable ASCII character in a font that nothing can set otherwise: no ligatures or kerning are
  * asked for, the font's own tables substitute no glyph, and the layout's advance is the sum of the glyphs'. A line
  * written in such characters then measures to the sum of these advances, to the float, which is what the wrap reads
  * for it (`GlyphAdvances.measureRun`), so whether it fits a row can be read off them without wrapping the line. They
  * are fractional and need not agree with the integer cell grid.
  */
final private[layout] class NominalAdvances private (advances: Array[Float]):

  def advanceOf(char: Char): Float = advances(char - NominalAdvances.FirstPrintable)

  /** Whether `text` is one row of `widthPx`: it is printable ASCII and its summed advance clears the width by the
    * margin within which the wrap itself declines to decide and measures the row exactly. Anything else is not said to
    * be one row, however short.
    */
  def fitsOneRow(text: String, widthPx: Int): Boolean =
    @tailrec
    def total(index: Int, sum: Float): Float =
      if index >= text.length then sum
      else
        val slot = text.charAt(index) - NominalAdvances.FirstPrintable
        if slot < 0 || slot >= advances.length then NominalAdvances.NotPrintable
        else total(index + 1, sum + advances(slot))
    val width = total(0, 0.0f)
    width >= 0.0f && width + ParagraphMeasurement.DecisionGuardPx < widthPx.toFloat

private[layout] object NominalAdvances:

  /** What a font and rendering context are measured under; both compare structurally. */
  final case class Key(font: Font, frc: FontRenderContext)

  private val FirstPrintable = 0x20
  private val LastPrintable  = 0x7e
  private val NotPrintable   = -1.0f

  /** The table for `font`, or `None` when something may set a character at an advance other than its own. */
  def of(font: Font, frc: FontRenderContext): Option[NominalAdvances] =
    val shapedText = ShapedTextCache()
    val probes     = shapedText.probes
    if GlyphAdvances
          .layoutAdvanceMatchesSum(font, frc, probes) && !GlyphAdvances.shapingMaySubstitute(font, frc, probes)
    then measured(font, frc, shapedText)
    else None

  private def measured(font: Font, frc: FontRenderContext, shapedText: ShapedTextCache): Option[NominalAdvances] =
    val resolver = TextCaretMeasurement.singleFontResolver(font)
    val each = (FirstPrintable to LastPrintable).map { code =>
      GlyphAdvances.measure(code.toChar.toString, 0, 1, 0, resolver, frc, shapedText)
    }
    val advances = each.map(_.caretsFrom(0, 1)(1)).toArray
    Option.when(each.forall(_.isContextFree(0, 1)) && advances.forall(_ > 0.0f))(new NominalAdvances(advances))
