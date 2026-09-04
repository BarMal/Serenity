package com.serenity.ui.tui

/** East-Asian-Width classification for the ranges that matter to a monospace terminal grid: CJK ideographs, Hangul,
  * Japanese kana, fullwidth forms, and the common emoji blocks. Not a full Unicode East Asian Width table -- combining
  * marks and the long tail of "Ambiguous" width codepoints are treated as narrow, matching what most terminal emulators
  * default to.
  */
object CharWidth:

  private val wideRanges: Vector[(Int, Int)] = Vector(
    0x1100  -> 0x115f,  // Hangul Jamo
    0x2e80  -> 0x303e,  // CJK radicals, Kangxi, CJK symbols/punctuation
    0x3041  -> 0x33ff,  // Hiragana .. CJK compatibility
    0x3400  -> 0x4dbf,  // CJK unified ideographs extension A
    0x4e00  -> 0x9fff,  // CJK unified ideographs
    0xa000  -> 0xa4cf,  // Yi syllables/radicals
    0xac00  -> 0xd7a3,  // Hangul syllables
    0xf900  -> 0xfaff,  // CJK compatibility ideographs
    0xfe30  -> 0xfe4f,  // CJK compatibility forms
    0xff00  -> 0xff60,  // Fullwidth forms
    0xffe0  -> 0xffe6,  // Fullwidth signs
    0x1f300 -> 0x1f64f, // Misc symbols and pictographs, emoticons
    0x1f680 -> 0x1f6ff, // Transport and map symbols
    0x1f900 -> 0x1f9ff, // Supplemental symbols and pictographs
    0x20000 -> 0x3fffd  // CJK unified ideographs extension B and beyond
  )

  /** The number of terminal cells a codepoint occupies: 2 for the wide ranges above, 1 for everything else. */
  def of(codePoint: Int): Int =
    if wideRanges.exists { case (lo, hi) => codePoint >= lo && codePoint <= hi } then 2 else 1
