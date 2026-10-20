package com.serenity.ui.fonts

import java.awt.Font

import com.serenity.text.fallback.{FontSlot, GlyphCoverage}

/** Font coverage for the Java2D backend, read with `Font.canDisplay`. The chain is the primary font, the configured
  * fallbacks, an optional emoji face, and last the logical `Dialog` composite: its fontconfig-driven component fonts
  * reach whatever the system has installed, so a codepoint no explicit fallback covers still has a font to go to.
  */
final class Java2DGlyphCoverage private (fonts: Vector[Font], override val emojiSlot: Option[FontSlot])
    extends GlyphCoverage:

  // A composite font's canDisplay walks its component fonts, loading them on first use, hence the memo. A few pages
  // of 256 codepoints per slot hold what text asks about; older pages are asked again.
  private val pages = GlyphCoveragePages(Java2DGlyphCoverage.PagesPerSlot * fonts.length)

  override def slotCount: Int = fonts.length

  override def covers(slot: FontSlot, codePoint: Int): Boolean =
    val index = slot.index
    index >= 0 && index < fonts.length && pages.covers(index, codePoint, fonts(index))

  def fontFor(slot: FontSlot): Option[Font] = fonts.lift(slot.index)

object Java2DGlyphCoverage:

  private val PagesPerSlot = 8

  /** The coverage of `primary` and the chain `fallback` describes. */
  def of(primary: Font, fallback: FontFallback): Java2DGlyphCoverage =
    new Java2DGlyphCoverage(primary +: fallback.fallbackFonts(primary), None)

  def apply(primary: Font, fallbacks: Vector[Font], emoji: Option[Font]): Java2DGlyphCoverage =
    val system = Font(Font.DIALOG, primary.getStyle, 1)
    val chain  = primary +: (fallbacks ++ emoji.toVector :+ system).map(FontFallback.matching(primary, _))
    new Java2DGlyphCoverage(chain, emoji.map(_ => FontSlot(1 + fallbacks.length)))
