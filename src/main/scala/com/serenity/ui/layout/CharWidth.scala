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

  /** The number of terminal cells a codepoint occupies: 2 for `Wide`/`Fullwidth` East Asian Width, 1 for everything
    * else.
    */
  def of(codePoint: Int): Int =
    UCharacter.getIntPropertyValue(codePoint, UProperty.EAST_ASIAN_WIDTH) match
      case UCharacter.EastAsianWidth.WIDE | UCharacter.EastAsianWidth.FULLWIDTH => 2
      case _                                                                    => 1
