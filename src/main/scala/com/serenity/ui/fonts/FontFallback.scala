package com.serenity.ui.fonts

import java.awt.Font
import java.awt.font.TextAttribute

import com.serenity.text.fallback.FontSlot

/** Which fonts draw a codepoint the primary font has no glyph for: the configured `families` in order, then the logical
  * `Dialog` font, whose fontconfig-driven component fonts reach whatever the system has installed. The chain is a pure
  * function of this value and the primary font, so measuring and painting build the same fonts from it.
  *
  * Slot 0 is the primary; slot `i` is `families(i - 1)`; the last slot is `Dialog`.
  */
final case class FontFallback(families: Vector[String] = Vector.empty):

  def slotCount: Int = families.length + 2

  /** The family of slots 1 and up, in order. */
  def slotFamilies: Vector[String] = families :+ Font.DIALOG

  private def familyAt(slot: FontSlot): String =
    families.lift(slot.index - 1).getOrElse(Font.DIALOG)

  /** The font that draws `slot` next to `primary`, at its style, size and ligature setting so a fallback glyph does not
    * change size or weight mid-line.
    */
  def fontFor(slot: FontSlot, primary: Font): Font =
    if slot == FontSlot.Primary then primary
    else FontFallback.matching(primary, Font(familyAt(slot), primary.getStyle, 1))

  /** Slots 1 and up, in order, as fonts matching `primary`. */
  def fallbackFonts(primary: Font): Vector[Font] =
    Vector.tabulate(slotCount - 1)(index => fontFor(FontSlot(index + 1), primary))

object FontFallback:

  /** No configured families: only the system's own resolver stands behind the primary. */
  val Default: FontFallback = FontFallback()

  def matching(primary: Font, fallback: Font): Font =
    val sized = fallback.deriveFont(primary.getStyle, primary.getSize2D)
    primary.getAttributes.get(TextAttribute.LIGATURES) match
      case ligatures: Integer =>
        sized.deriveFont(java.util.Map.of[TextAttribute, Integer](TextAttribute.LIGATURES, ligatures))
      case _ => sized
