package com.serenity.ui.skia

import com.serenity.text.fallback.{FontSlot, GlyphCoverage}
import com.serenity.ui.fonts.{FontFallback, GlyphCoveragePages}
import com.serenity.ui.renderer.FontSpec
import org.jetbrains.skia.Typeface

/** Which slots of a fallback chain have a glyph for a codepoint, read from the Skia typefaces the chain is shaped and
  * painted with. A slot covers what its typeface maps to a glyph. A slot of a logical family (`Dialog`, `SansSerif`,
  * `Serif`, `Monospaced`) is a composite in AWT, which reaches whatever font the system holds for what its own face
  * lacks; here it also covers what the font manager can find a font for. Control and format characters (tab, soft
  * hyphen, bidi marks) draw nothing and are covered by every slot, as AWT reported them, so a line that holds one is
  * not cut into runs for it.
  *
  * The answers are kept as pages of the codepoints asked about, so the typefaces are asked once for each.
  */
final class SkiaGlyphCoverage private (slots: Vector[GlyphCoveragePages.Has], override val emojiSlot: Option[FontSlot])
    extends GlyphCoverage:

  private val pages = GlyphCoveragePages(SkiaGlyphCoverage.PagesPerSlot * slots.length)

  override def slotCount: Int = slots.length

  override def covers(slot: FontSlot, codePoint: Int): Boolean =
    val index = slot.index
    index >= 0 && index < slots.length && pages.covers(index, codePoint, slots(index))

object SkiaGlyphCoverage:

  private val PagesPerSlot = 8

  /** A slot's typeface, and whether its family is a logical one that reaches the system's other fonts. */
  final case class Slot(typeface: Typeface, composite: Boolean)

  private def drawsNothing(codePoint: Int): Boolean =
    val kind = Character.getType(codePoint)
    kind == Character.CONTROL || kind == Character.FORMAT

  /** The coverage of `slots`, the primary first. `system` finds a font for what a composite slot's typeface lacks. */
  def of(slots: Vector[Slot], emojiSlot: Option[FontSlot], system: FallbackFonts): SkiaGlyphCoverage =
    val has = slots.map { slot =>
      val typeface = slot.typeface
      if slot.composite then
        new GlyphCoveragePages.Has:
          def covers(codePoint: Int): Boolean =
            drawsNothing(codePoint) || typeface.getUTF32Glyph(codePoint) != 0 ||
              system.typefaceFor(typeface, codePoint).isDefined
      else
        new GlyphCoveragePages.Has:
          def covers(codePoint: Int): Boolean = drawsNothing(codePoint) || typeface.getUTF32Glyph(codePoint) != 0
    }
    new SkiaGlyphCoverage(has, emojiSlot)

  /** The coverage of the chain `fallback` describes at `primary`, each slot's typeface from `typefaceOf`. */
  def ofChain(
    primary: FontSpec,
    fallback: FontFallback,
    typefaceOf: FontSpec => Typeface,
    system: FallbackFonts
  ): SkiaGlyphCoverage =
    val primaryFont = primary.toAwt
    val slots = Vector.tabulate(fallback.slotCount) { index =>
      val spec = FontSpec.fromAwt(fallback.fontFor(FontSlot(index), primaryFont))
      Slot(typefaceOf(spec), SkiaTypefaces.isLogical(spec.family))
    }
    of(slots, fallback.emojiSlot, system)
