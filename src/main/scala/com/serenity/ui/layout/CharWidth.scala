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
  */
object CharWidth:

  /** The first codepoint that is a combining mark (U+0300), and below which nothing is wide either: text under it is
    * one cell a character.
    */
  private val FirstNonTrivial = 0x0300

  /** The number of terminal cells a codepoint occupies: none for a combining mark, which composes onto the character
    * before it, 2 for `Wide`/`Fullwidth` East Asian Width, 1 for everything else. This is the one place that says so:
    * the terminal grid, the GUI cell grid, the caret stops and the wrap all count by it.
    */
  def of(codePoint: Int): Int =
    if codePoint < FirstNonTrivial then 1
    else if isCombiningMark(codePoint) then 0
    else
      UCharacter.getIntPropertyValue(codePoint, UProperty.EAST_ASIAN_WIDTH) match
        case UCharacter.EastAsianWidth.WIDE | UCharacter.EastAsianWidth.FULLWIDTH => 2
        case _                                                                    => 1

  /** Whether `codePoint` takes no cell of its own: a non-spacing, spacing-combining or enclosing mark. */
  def isCombiningMark(codePoint: Int): Boolean =
    Character.getType(codePoint) match
      case Character.NON_SPACING_MARK | Character.COMBINING_SPACING_MARK | Character.ENCLOSING_MARK => true
      case _                                                                                        => false
