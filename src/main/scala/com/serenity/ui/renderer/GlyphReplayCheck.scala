package com.serenity.ui.renderer

import java.awt.{Color, Font, Graphics2D, RenderingHints}
import java.awt.font.FontRenderContext
import java.awt.image.BufferedImage
import java.util.concurrent.ConcurrentHashMap

/** Whether a font's glyph codes can be turned back into the glyphs `drawString` draws. A shaped stretch keeps only
  * codes and positions and rebuilds its glyphs from them on every repaint; JetBrains Runtime 25 reads the codes of a
  * composite font as other glyphs, so a whole document comes out as garbage. Each font, at each render context, is
  * therefore drawn once both ways on first use and replayed only if the pixels are identical.
  *
  * The comparison is a parameter so a test can state a verdict without needing a runtime that gives it.
  */
final class GlyphReplayCheck(
    matchesDrawString: (Font, FontRenderContext) => Boolean = GlyphReplayCheck.replayMatchesDrawString
):
  import GlyphReplayCheck.*

  private val verdicts   = new ConcurrentHashMap[Font, ConcurrentHashMap[FontRenderContext, Verdict]]()
  private val noVerdicts = new ConcurrentHashMap[FontRenderContext, Verdict]()

  def faithful(font: Font, fontRenderContext: FontRenderContext): Boolean =
    verdicts.getOrDefault(font, noVerdicts).getOrDefault(fontRenderContext, Verdict.Unknown) match
      case Verdict.Faithful   => true
      case Verdict.Unfaithful => false
      case Verdict.Unknown    => judge(font, fontRenderContext)

  private def judge(font: Font, fontRenderContext: FontRenderContext): Boolean =
    val faithful = matchesDrawString(font, fontRenderContext)
    if verdicts.size >= MaxFonts then verdicts.clear()
    val _ = verdicts
      .computeIfAbsent(font, _ => new ConcurrentHashMap[FontRenderContext, Verdict]())
      .put(fontRenderContext, if faithful then Verdict.Faithful else Verdict.Unfaithful)
    faithful

object GlyphReplayCheck:

  private enum Verdict:
    case Unknown, Faithful, Unfaithful

  private val MaxFonts = 4096

  private val ProbeText = "Hoxi AVATAR fi To {}[] 0123"
  private val MarginPx  = 8

  /** Draws [[ProbeText]] with `drawString` and from a glyph vector rebuilt from the codes of its shaped stretch, at the
    * render context's transform and hints, and compares every pixel.
    */
  def replayMatchesDrawString(font: Font, fontRenderContext: FontRenderContext): Boolean =
    ShapedStretch.shape(ProbeText, font, fontRenderContext).glyphsOf(0, ProbeText.length).forall { replayed =>
      sameRaster(
        rendered(font, fontRenderContext)(_.drawString(ProbeText, 0.0f, 0.0f)),
        rendered(font, fontRenderContext)(_.drawGlyphVector(replayed, 0.0f, 0.0f))
      )
    }

  def sameRaster(a: BufferedImage, b: BufferedImage): Boolean =
    a.getWidth == b.getWidth && a.getHeight == b.getHeight && java.util.Arrays.equals(pixelsOf(a), pixelsOf(b))

  private def pixelsOf(image: BufferedImage): Array[Int] =
    val width  = image.getWidth
    val height = image.getHeight
    image.getRGB(0, 0, width, height, new Array[Int](width * height), 0, width)

  /** The text's origin sits at the margin below and right of the corner, so no glyph is clipped by the image edge. */
  private def rendered(font: Font, fontRenderContext: FontRenderContext)(draw: Graphics2D => Unit): BufferedImage =
    val transform = fontRenderContext.getTransform
    val bounds    = font.getStringBounds(ProbeText, fontRenderContext)
    val widthPx   = math.ceil((bounds.getWidth + 2 * MarginPx) * math.abs(transform.getScaleX)).toInt.max(1)
    val heightPx  = math.ceil((bounds.getHeight + 2 * MarginPx) * math.abs(transform.getScaleY)).toInt.max(1)
    val image     = new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_ARGB)
    val g         = image.createGraphics()
    try
      g.setRenderingHint(
        RenderingHints.KEY_TEXT_ANTIALIASING,
        if fontRenderContext.isAntiAliased then RenderingHints.VALUE_TEXT_ANTIALIAS_ON
        else RenderingHints.VALUE_TEXT_ANTIALIAS_OFF
      )
      g.setRenderingHint(
        RenderingHints.KEY_FRACTIONALMETRICS,
        if fontRenderContext.usesFractionalMetrics then RenderingHints.VALUE_FRACTIONALMETRICS_ON
        else RenderingHints.VALUE_FRACTIONALMETRICS_OFF
      )
      g.transform(transform)
      g.translate(MarginPx.toDouble, MarginPx - bounds.getY)
      g.setFont(font)
      g.setColor(Color.WHITE)
      draw(g)
    finally g.dispose()
    image
