package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.richtext.ParagraphRole
import com.serenity.state.models.TextVisualLine

/** Throwaway spike access (#1812) to Serenity's own wrap and measurement, which are `private[layout]`. Living in the
  * same package is the only reason this file sits here; nothing in the root project depends on it.
  */
object SpikeLayoutBridge:

  /** Serenity's cold wrap of one logical line: `ParagraphMeasurement` + `GlyphAdvances` + UAX#14 breaks. */
  def wrapLine(text: String, bufferLine: Int, panelWidthPx: Int, font: Font, frc: FontRenderContext)
    : Vector[TextVisualLine] =
    TextLayoutSnapshot.wrapRows(key(text, panelWidthPx, font, frc), bufferLine, Int.MaxValue).rows

  /** Every UTF-16 offset's caret x from one `GlyphAdvances` sweep, or `None` where a character needs exact shaping. */
  def glyphAdvanceCarets(text: String, font: Font, frc: FontRenderContext): Option[IArray[Float]] =
    val advances = GlyphAdvances.measure(text, 0, text.length, 0, TextCaretMeasurement.singleFontResolver(font), frc)
    Option.when(advances.isContextFree(0, text.length))(advances.caretsFrom(0, text.length))

  private val cellMetrics = java.util.concurrent.ConcurrentHashMap[Font, CellMetrics]()

  private def key(text: String, panelWidthPx: Int, font: Font, frc: FontRenderContext): WrappedLineKey =
    WrappedLineKey(
      text,
      panelWidthPx,
      TextCaretMeasurement.singleFontResolver(font),
      frc,
      measuredLayout = true,
      cellMetrics.computeIfAbsent(font, CellMetrics.fromFont(_)),
      baseColumn = 0,
      ParagraphRole.Body,
      dropCapGlyphWidthPx = 0.0f
    )
