package com.serenity.ui.fonts

import java.awt.Font
import java.awt.font.TextAttribute
import java.util.concurrent.ConcurrentHashMap
import java.util.function.Function as JavaFunction

import com.serenity.text.fallback.{FontSlot, GlyphCoverage}

/** Font coverage for the Java2D backend, read with `Font.canDisplay`. The chain is the primary font, the configured
  * fallbacks, an optional emoji face, and last the logical `Dialog` composite: its fontconfig-driven component fonts
  * reach whatever the system has installed, so a codepoint no explicit fallback covers still has a font to go to.
  */
final class Java2DGlyphCoverage private (fonts: Vector[Font], override val emojiSlot: Option[FontSlot])
    extends GlyphCoverage:

  // A composite font's canDisplay walks its component fonts, loading them on first use, hence one memo per slot.
  private val memos: Vector[ConcurrentHashMap[Integer, java.lang.Boolean]] =
    fonts.map(_ => ConcurrentHashMap[Integer, java.lang.Boolean]())

  private val probes: Vector[JavaFunction[Integer, java.lang.Boolean]] =
    fonts.map { font =>
      val probe: JavaFunction[Integer, java.lang.Boolean] =
        codePoint => java.lang.Boolean.valueOf(font.canDisplay(codePoint.intValue))
      probe
    }

  override def slotCount: Int = fonts.length

  override def covers(slot: FontSlot, codePoint: Int): Boolean =
    val index = slot.index
    index >= 0 && index < fonts.length &&
    memos(index).computeIfAbsent(Integer.valueOf(codePoint), probes(index)).booleanValue

  def fontFor(slot: FontSlot): Option[Font] = fonts.lift(slot.index)

object Java2DGlyphCoverage:

  def apply(primary: Font, fallbacks: Vector[Font], emoji: Option[Font]): Java2DGlyphCoverage =
    val system = Font(Font.DIALOG, primary.getStyle, 1)
    val chain  = primary +: (fallbacks ++ emoji.toVector :+ system).map(matching(primary, _))
    new Java2DGlyphCoverage(chain, emoji.map(_ => FontSlot(1 + fallbacks.length)))

  /** A fallback drawn next to `primary` should not change size or weight mid-line, nor drop its ligature setting. */
  private def matching(primary: Font, fallback: Font): Font =
    val sized = fallback.deriveFont(primary.getStyle, primary.getSize2D)
    primary.getAttributes.get(TextAttribute.LIGATURES) match
      case ligatures: Integer =>
        sized.deriveFont(java.util.Map.of[TextAttribute, Integer](TextAttribute.LIGATURES, ligatures))
      case _ => sized
