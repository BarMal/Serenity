package com.serenity.text.fallback

import com.ibm.icu.lang.{UCharacter, UProperty, UScript}
import com.serenity.text.TextEditing

/** UTF-16 range `[from, until)` of a line drawn with one slot of the fallback chain. */
final case class FallbackRun(from: Int, until: Int, slot: FontSlot)

/** Cuts a line into runs by font coverage. Slots are chosen per grapheme cluster, so a cluster is never split:
  *   - a cluster asking for emoji presentation (VS16, Emoji_Presentation, or a pictographic ZWJ sequence) takes the
  *     emoji slot when that slot covers it;
  *   - a Common or Inherited cluster (spaces, punctuation, digits) stays on the previous cluster's slot when that slot
  *     covers it, so neutrals inside CJK or Arabic text do not flap back to the primary font;
  *   - otherwise the first slot covering every codepoint, ignoring default-ignorables such as ZWJ and selectors, then
  *     the first slot covering the base, with VS15 trying the non-emoji slots first;
  *   - a cluster no slot covers stays in the primary, whose missing glyph then does not split the run.
  */
object FallbackItemiser:

  private val ZeroWidthJoiner           = 0x200d
  private val TextPresentationSelector  = 0xfe0e
  private val EmojiPresentationSelector = 0xfe0f

  private enum Presentation:
    case Text, Emoji, Unspecified

  def itemise(text: String, from: Int, until: Int, coverage: GlyphCoverage): Vector[FallbackRun] =
    val start = math.max(0, from)
    val end   = math.min(until, text.length)
    if start >= end then Vector.empty
    else if primaryCoversPlainly(text, start, end, coverage) then Vector(FallbackRun(start, end, FontSlot.Primary))
    else itemiseClusters(text, start, end, coverage)

  /** The common case, checked per codepoint without walking clusters so it allocates nothing beyond the one run. */
  @annotation.tailrec
  private def primaryCoversPlainly(text: String, index: Int, end: Int, coverage: GlyphCoverage): Boolean =
    if index >= end then true
    else
      val codePoint = text.codePointAt(index)
      val plain =
        codePoint != ZeroWidthJoiner && codePoint != EmojiPresentationSelector &&
          !UCharacter.hasBinaryProperty(codePoint, UProperty.EMOJI_PRESENTATION) &&
          (isDefaultIgnorable(codePoint) || coverage.covers(FontSlot.Primary, codePoint))
      plain && primaryCoversPlainly(text, index + Character.charCount(codePoint), end, coverage)

  private def itemiseClusters(text: String, start: Int, end: Int, coverage: GlyphCoverage): Vector[FallbackRun] =
    @annotation.tailrec
    def loop(
      clusterStart: Int,
      runStart: Int,
      runSlot: Option[FontSlot],
      runs: Vector[FallbackRun]
    ): Vector[FallbackRun] =
      if clusterStart >= end then runSlot.fold(runs)(slot => runs :+ FallbackRun(runStart, end, slot))
      else
        val clusterEnd = math.min(end, TextEditing.nextGraphemeBoundary(text, clusterStart))
        val slot       = slotFor(text, clusterStart, clusterEnd, runSlot, coverage)
        runSlot match
          case Some(current) if current == slot => loop(clusterEnd, runStart, runSlot, runs)
          case Some(current) =>
            loop(clusterEnd, clusterStart, Some(slot), runs :+ FallbackRun(runStart, clusterStart, current))
          case None => loop(clusterEnd, clusterStart, Some(slot), runs)
    loop(start, start, None, Vector.empty)

  private def slotFor(
    text: String,
    start: Int,
    end: Int,
    previous: Option[FontSlot],
    coverage: GlyphCoverage
  ): FontSlot =
    val base                          = text.codePointAt(start)
    val presentation                  = presentationOf(text, start, end, base)
    def coversCluster(slot: FontSlot) = coversAll(text, start, end, slot, coverage)
    def isEmojiSlot(slot: FontSlot)   = coverage.emojiSlot.contains(slot)
    val preferred: Option[FontSlot] = presentation match
      case Presentation.Emoji => coverage.emojiSlot.filter(coversCluster)
      case Presentation.Text  => previous.filter(slot => isNeutral(base) && !isEmojiSlot(slot) && coversCluster(slot))
      case Presentation.Unspecified => previous.filter(slot => isNeutral(base) && coversCluster(slot))
    val candidates = Vector.tabulate(coverage.slotCount)(FontSlot(_))
    val ordered = presentation match
      case Presentation.Text =>
        val (emojiSlots, textSlots) = candidates.partition(isEmojiSlot)
        textSlots ++ emojiSlots
      case Presentation.Emoji | Presentation.Unspecified => candidates
    preferred
      .orElse(ordered.find(coversCluster))
      .orElse(ordered.find(coverage.covers(_, base)))
      .getOrElse(FontSlot.Primary)

  private def presentationOf(text: String, start: Int, end: Int, base: Int): Presentation =
    val cluster = text.substring(start, end)
    if cluster.indexOf(TextPresentationSelector) >= 0 then Presentation.Text
    else if cluster.indexOf(EmojiPresentationSelector) >= 0 then Presentation.Emoji
    else if UCharacter.hasBinaryProperty(base, UProperty.EMOJI_PRESENTATION) then Presentation.Emoji
    else if cluster.indexOf(ZeroWidthJoiner) >= 0 && UCharacter.hasBinaryProperty(base, UProperty.EXTENDED_PICTOGRAPHIC)
    then Presentation.Emoji
    else Presentation.Unspecified

  @annotation.tailrec
  private def coversAll(text: String, index: Int, end: Int, slot: FontSlot, coverage: GlyphCoverage): Boolean =
    if index >= end then true
    else
      val codePoint = text.codePointAt(index)
      (isDefaultIgnorable(codePoint) || coverage.covers(slot, codePoint)) &&
      coversAll(text, index + Character.charCount(codePoint), end, slot, coverage)

  private def isDefaultIgnorable(codePoint: Int): Boolean =
    UCharacter.hasBinaryProperty(codePoint, UProperty.DEFAULT_IGNORABLE_CODE_POINT)

  private def isNeutral(codePoint: Int): Boolean =
    val script = UScript.getScript(codePoint)
    script == UScript.COMMON || script == UScript.INHERITED
