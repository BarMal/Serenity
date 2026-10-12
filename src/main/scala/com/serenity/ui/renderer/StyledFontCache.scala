package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.{FontRenderContext, LineMetrics}
import java.awt.geom.AffineTransform
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

import com.serenity.ui.theme.TextStyle

/** The AWT font each `(base font, style)` pair paints with, derived once and handed back by identity afterwards, and
  * the geometry of the underline a style asks for.
  *
  * Deriving per run rebuilt the same handful of fonts every frame. Keys are values (`Font` and `TextStyle` compare
  * structurally), so a changed base font simply misses and nothing here goes stale; the size bounds only stop a long
  * session of ever-different styles from growing the maps without limit.
  */
final class StyledFontCache(maxBaseFonts: Int = 16, maxStylesPerBaseFont: Int = 128):
  import StyledFontCache.*

  private val fonts      = new ConcurrentHashMap[Font, ConcurrentHashMap[TextStyle, Font]]()
  private val underlines = new ConcurrentHashMap[Font, Underline]()
  private val derived    = new AtomicLong(0)

  /** How many fonts were derived, as opposed to served from the cache or taken as the base itself. */
  def derivations: Long = derived.get()

  def fontFor(base: Font, style: TextStyle): Font =
    if leavesBaseUntouched(base, style) then base
    else
      if fonts.size >= maxBaseFonts && !fonts.containsKey(base) then fonts.clear()
      val styles = fonts.computeIfAbsent(base, _ => new ConcurrentHashMap[TextStyle, Font]())
      if styles.size >= maxStylesPerBaseFont && !styles.containsKey(style) then styles.clear()
      styles.computeIfAbsent(
        style,
        _ =>
          val _ = derived.incrementAndGet()
          TextStyle.styledFont(base, style)
      )

  def underlineOf(font: Font): Underline =
    if underlines.size >= maxBaseFonts * maxStylesPerBaseFont then underlines.clear()
    underlines.computeIfAbsent(
      font,
      _ =>
        FontFirstUse.settle(font, MeasuringContext)
        Underline.of(font.getLineMetrics("", MeasuringContext))
    )

object StyledFontCache:

  /** Shared by every surface that is not handed its own: a styled font is a pure function of its key. */
  val shared: StyledFontCache = new StyledFontCache()

  /** An underline's offset and thickness belong to the font, not to the surface that paints it. */
  private val MeasuringContext = FontRenderContext(AffineTransform(), true, true)

  /** Distance below the baseline to the underline's top, and its thickness, in logical pixels. */
  final case class Underline(offsetPx: Float, thicknessPx: Float)

  object Underline:
    def of(metrics: LineMetrics): Underline = Underline(metrics.getUnderlineOffset, metrics.getUnderlineThickness)

  private val Normal = TextStyle.normal

  /** A plain style over a plain base font is the base font itself. */
  private def leavesBaseUntouched(base: Font, style: TextStyle): Boolean =
    style == Normal && base.getStyle == Font.PLAIN && base.getSize2D >= 1.0f
