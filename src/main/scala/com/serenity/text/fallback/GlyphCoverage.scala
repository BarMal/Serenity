package com.serenity.text.fallback

/** A position in a font fallback chain: 0 is the primary font, and the last slot is the backend's system resolver. */
opaque type FontSlot = Int

object FontSlot:
  val Primary: FontSlot = 0

  def apply(index: Int): FontSlot = index

  extension (slot: FontSlot) def index: Int = slot

/** Which slots of a fallback chain have a glyph for a codepoint. The one backend-specific part of fallback: each
  * backend answers from the same fonts its shaper will use, so the runs it measures are the runs it paints.
  */
trait GlyphCoverage:
  def slotCount: Int

  /** The slot holding the emoji face, preferred for clusters that ask for emoji presentation. */
  def emojiSlot: Option[FontSlot]

  def covers(slot: FontSlot, codePoint: Int): Boolean
