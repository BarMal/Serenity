package com.serenity.ui.fonts

import java.awt.Font
import java.awt.font.TextAttribute

import com.serenity.text.fallback.FontSlot

/** Which fonts draw a codepoint the primary font has no glyph for: the configured `families` in order, the bundled
  * `emojiFamily` if there is one, then the logical `Dialog` font, whose fontconfig-driven component fonts reach
  * whatever the system has installed. The chain is a pure function of this value and the primary font, so measuring and
  * painting build the same fonts from it.
  *
  * Slot 0 is the primary; slot `i` is `families(i - 1)`; the emoji face, when present, follows the configured families;
  * the last slot is `Dialog`.
  */
final case class FontFallback(families: Vector[String] = Vector.empty, emojiFamily: Option[String] = None):

  def slotCount: Int = slotFamilies.length + 1

  /** The family of slots 1 and up, in order. */
  def slotFamilies: Vector[String] = (families ++ emojiFamily) :+ Font.DIALOG

  /** The slot of the emoji face, preferred for clusters that ask for emoji presentation. */
  def emojiSlot: Option[FontSlot] = emojiFamily.map(_ => FontSlot(families.length + 1))

  private def familyAt(slot: FontSlot): String =
    slotFamilies.lift(slot.index - 1).getOrElse(Font.DIALOG)

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

  /** `fallback` at `primary`'s style and size, with `primary`'s ligature setting unless `ligaturesOn` forces them on:
    * the JDK joins an emoji sequence into one glyph only in a font with ligatures on.
    */
  def matching(primary: Font, fallback: Font, ligaturesOn: Boolean = false): Font =
    val sized = fallback.deriveFont(primary.getStyle, primary.getSize2D)
    val setting =
      if ligaturesOn then Some(TextAttribute.LIGATURES_ON)
      else
        primary.getAttributes.get(TextAttribute.LIGATURES) match
          case ligatures: Integer => Some(ligatures)
          case _                  => None
    setting.fold(sized)(ligatures =>
      sized.deriveFont(java.util.Map.of[TextAttribute, Integer](TextAttribute.LIGATURES, ligatures))
    )
