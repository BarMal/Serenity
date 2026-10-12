package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.{FontRenderContext, TextAttribute, TextLayout}
import java.text.AttributedString
import java.util.concurrent.ConcurrentHashMap

/** Makes the first use of a font, at a render context, happen once and on one thread before anything is measured or
  * shaped with it. AWT's first use of a font is not safe against another thread's first use of it: readings taken while
  * two threads warm the same font have been seen to differ from the settled ones, and every long-lived cache (shaped
  * text, line extents, glyph advances) would keep the wrong one. The warm-up runs under one lock and a thread that
  * finds the font already settled takes no lock and allocates nothing, so only the first use of each font waits.
  *
  * Every path that measures or shapes text calls [[settle]] with the font it is about to use; a path that does not
  * (painting through `Graphics2D` directly) can still race a measurement that is the font's first use.
  */
final class FontFirstUse:
  import FontFirstUse.*

  private val settled = new ConcurrentHashMap[Font, ConcurrentHashMap[FontRenderContext, java.lang.Boolean]]()
  private val lock    = new Object

  def isSettled(font: Font, frc: FontRenderContext): Boolean =
    settled.getOrDefault(font, NoContexts).containsKey(frc)

  def settle(font: Font, frc: FontRenderContext): Unit = settle(font, frc, warmUp)

  private[ui] def settle(font: Font, frc: FontRenderContext, firstUse: (Font, FontRenderContext) => Unit): Unit =
    if !isSettled(font, frc) then
      lock.synchronized {
        if !isSettled(font, frc) then
          firstUse(font, frc)
          if settled.size >= MaxSettled then settled.clear()
          val _ = settled
            .computeIfAbsent(font, _ => new ConcurrentHashMap[FontRenderContext, java.lang.Boolean]())
            .put(frc, java.lang.Boolean.TRUE)
      }

  def settleAll(fonts: Iterable[Font], frc: FontRenderContext): Unit = fonts.foreach(settle(_, frc))

object FontFirstUse:

  private val MaxSettled = 4096

  private val NoContexts = new ConcurrentHashMap[FontRenderContext, java.lang.Boolean]()

  private val WarmUpText = "Hoxi AVATAR fi To"

  /** One for the process: the fonts AWT has warmed are the process's. */
  val shared: FontFirstUse = new FontFirstUse()

  def settle(font: Font, frc: FontRenderContext): Unit = shared.settle(font, frc)

  def settleAll(fonts: Iterable[Font], frc: FontRenderContext): Unit = shared.settleAll(fonts, frc)

  def isSettled(font: Font, frc: FontRenderContext): Boolean = shared.isSettled(font, frc)

  def warmUp(font: Font, frc: FontRenderContext): Unit =
    val chars = WarmUpText.toCharArray
    val _     = font.getLineMetrics(WarmUpText, frc)
    val _     = font.getStringBounds(WarmUpText, frc)
    val nomnl = font.createGlyphVector(frc, chars)
    val _     = (0 until nomnl.getNumGlyphs).map(nomnl.getGlyphMetrics(_).getAdvance)
    val laid  = font.layoutGlyphVector(frc, chars, 0, chars.length, Font.LAYOUT_LEFT_TO_RIGHT)
    val _     = laid.getGlyphPositions(0, laid.getNumGlyphs + 1, new Array[Float](2 * (laid.getNumGlyphs + 1)))
    val text  = AttributedString(WarmUpText)
    text.addAttribute(TextAttribute.FONT, font)
    val layout = TextLayout(text.getIterator, frc)
    val _      = layout.getAdvance
