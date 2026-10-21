package com.serenity.ui.layout

import com.ibm.icu.lang.{UCharacter, UProperty}

/** East-Asian-Width classification for the codepoints that matter to a monospace terminal grid, backed by ICU4J's
  * Unicode East Asian Width property (UAX#11) rather than a hand-rolled range table. `Wide` and `Fullwidth` count as
  * two cells; `Narrow`, `Halfwidth`, `Neutral` and `Ambiguous` all count as one -- collapsing Ambiguous into narrow
  * matches what most terminal emulators default to (#1277), and is the same choice the old hand-rolled table made for
  * the codepoints it covered at all. Unlike that table, this also gets the wide singletons that sit outside the old
  * table's blocks right (U+231A, U+1F004, U+1F0CF and friends), since the property covers every codepoint rather than a
  * fixed list of ranges.
  *
  * This lives in the layout package, not the terminal one, because both ends of the grid have to agree on it:
  * `TerminalScreenBuffer` advances the cells it paints by this width, and `TextLayoutSnapshot` measures caret stops and
  * wrap points by it whenever it is laying out on a display-width-aware grid ([[CellMetrics.displayWidthAware]]). When
  * the two disagree, the caret drifts one cell per wide glyph and wrapped rows break mid-glyph.
  *
  * What takes cells is a grapheme cluster, not a codepoint: [[ofCluster]] gives an emoji cluster (a variation selector
  * 16 on an emoji base, a zero-width-joiner join, an emoji modifier, a tag sequence, a regional-indicator pair) two
  * cells whatever its parts are, and any other cluster the sum of its codepoints' [[of]]. A caller that steps through
  * text a codepoint at a time is right while [[hasClusterChars]] is false and steps by [[clusterEnd]] when it is not.
  */
object CharWidth:

  /** The first codepoint that is a combining mark (U+0300), and below which nothing is wide either: text under it is
    * one cell a character.
    */
  private val FirstNonTrivial = 0x0300

  private val ZeroWidthNonJoiner        = 0x200c
  private val ZeroWidthJoiner           = 0x200d
  private val EmojiPresentationSelector = 0xfe0f
  private val KeycapEnclosure           = 0x20e3
  private val FirstTag                  = 0xe0020
  private val LastTag                   = 0xe007f

  /** The number of terminal cells a codepoint occupies on its own: none for a combining mark, which composes onto the
    * character before it, or for a zero-width joiner or non-joiner, 2 for `Wide`/`Fullwidth` East Asian Width, 1 for
    * everything else. [[ofCluster]] is what a grapheme cluster takes.
    */
  def of(codePoint: Int): Int =
    if codePoint < FirstNonTrivial then 1
    else if codePoint == ZeroWidthNonJoiner || codePoint == ZeroWidthJoiner || isCombiningMark(codePoint) then 0
    else
      UCharacter.getIntPropertyValue(codePoint, UProperty.EAST_ASIAN_WIDTH) match
        case UCharacter.EastAsianWidth.WIDE | UCharacter.EastAsianWidth.FULLWIDTH => 2
        case _                                                                    => 1

  /** Whether `codePoint` takes no cell of its own: a non-spacing, spacing-combining or enclosing mark. */
  def isCombiningMark(codePoint: Int): Boolean =
    Character.getType(codePoint) match
      case Character.NON_SPACING_MARK | Character.COMBINING_SPACING_MARK | Character.ENCLOSING_MARK => true
      case _                                                                                        => false

  /** Whether `text` holds a character that can make an emoji cluster: a joiner, a variation selector 16, a keycap
    * enclosure, or an astral codepoint (an emoji modifier, a regional indicator or a tag). Text without one is all
    * single-codepoint clusters, so a plain ASCII or CJK row never takes the cluster path.
    */
  def hasClusterChars(text: String): Boolean =
    @annotation.tailrec
    def scan(index: Int): Boolean =
      index < text.length && {
        val char = text.charAt(index)
        char == ZeroWidthJoiner || char == EmojiPresentationSelector || char == KeycapEnclosure ||
        Character.isHighSurrogate(char) || scan(index + 1)
      }
    scan(0)

  /** The index after the grapheme cluster that starts at `from`: its base, then whatever extends it (marks, joiners,
    * variation selectors, emoji modifiers, tag characters), then, after a joiner, the next pictograph and its
    * extensions. A regional indicator takes the one after it. A control character is a cluster of its own. Agrees with
    * ICU's grapheme boundaries for emoji sequences and combining marks.
    */
  def clusterEnd(text: String, from: Int): Int =
    val first      = text.codePointAt(from)
    val afterFirst = from + Character.charCount(first)
    if Character.isISOControl(first) then afterFirst
    else
      val start =
        if isRegionalIndicator(first) && afterFirst < text.length && isRegionalIndicator(text.codePointAt(afterFirst))
        then afterFirst + Character.charCount(text.codePointAt(afterFirst))
        else afterFirst
      extendedEnd(text, start, isExtendedPictographic(first), afterJoiner = false)

  @annotation.tailrec
  private def extendedEnd(text: String, index: Int, joinable: Boolean, afterJoiner: Boolean): Int =
    if index >= text.length then index
    else
      val codePoint = text.codePointAt(index)
      val next      = index + Character.charCount(codePoint)
      if isExtender(codePoint) then extendedEnd(text, next, joinable, codePoint == ZeroWidthJoiner)
      else if joinable && afterJoiner && isExtendedPictographic(codePoint) then extendedEnd(text, next, true, false)
      else index

  /** The cells of the cluster `text[from, until)`: 2 for an emoji cluster, else the sum of its codepoints' [[of]]. */
  def ofCluster(text: String, from: Int, until: Int): Int =
    val first = text.codePointAt(from)
    if from + Character.charCount(first) >= until then of(first)
    else if isEmojiCluster(text, from, until) then 2
    else
      @annotation.tailrec
      def sum(index: Int, cells: Int): Int =
        if index >= until then cells
        else
          val codePoint = text.codePointAt(index)
          sum(index + Character.charCount(codePoint), cells + of(codePoint))
      sum(from, 0)

  /** Whether the cluster `text[from, until)` is one that terminals are not agreed on and that takes two cells: a
    * regional-indicator pair; a variation selector 16 straight after an emoji base (`Emoji=Yes`, which stands in for
    * the list of valid emoji variation sequences, ICU not exposing it); or a pictograph joined to another by a joiner,
    * or with an emoji modifier or a tag character.
    */
  def isEmojiCluster(text: String, from: Int, until: Int): Boolean =
    val first      = text.codePointAt(from)
    val afterFirst = from + Character.charCount(first)
    if afterFirst >= until then false
    else if isRegionalIndicator(first) then isRegionalIndicator(text.codePointAt(afterFirst))
    else if text
          .codePointAt(afterFirst) == EmojiPresentationSelector && UCharacter.hasBinaryProperty(first, UProperty.EMOJI)
    then true
    else isExtendedPictographic(first) && hasEmojiExtension(text, afterFirst, until, afterJoiner = false)

  @annotation.tailrec
  private def hasEmojiExtension(text: String, index: Int, until: Int, afterJoiner: Boolean): Boolean =
    index < until && {
      val codePoint = text.codePointAt(index)
      val next      = index + Character.charCount(codePoint)
      (afterJoiner && isExtendedPictographic(codePoint)) || isEmojiModifier(codePoint) || isTag(codePoint) ||
      hasEmojiExtension(text, next, until, codePoint == ZeroWidthJoiner)
    }

  private def isExtender(codePoint: Int): Boolean =
    codePoint == ZeroWidthNonJoiner || codePoint == ZeroWidthJoiner || isCombiningMark(codePoint) ||
      isEmojiModifier(codePoint) || isTag(codePoint)

  private def isTag(codePoint: Int): Boolean = codePoint >= FirstTag && codePoint <= LastTag

  private def isEmojiModifier(codePoint: Int): Boolean =
    UCharacter.hasBinaryProperty(codePoint, UProperty.EMOJI_MODIFIER)

  private def isExtendedPictographic(codePoint: Int): Boolean =
    UCharacter.hasBinaryProperty(codePoint, UProperty.EXTENDED_PICTOGRAPHIC)

  private def isRegionalIndicator(codePoint: Int): Boolean =
    UCharacter.hasBinaryProperty(codePoint, UProperty.REGIONAL_INDICATOR)
