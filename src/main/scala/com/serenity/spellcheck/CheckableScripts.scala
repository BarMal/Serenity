package com.serenity.spellcheck

import java.lang.Character.UnicodeScript

/** The scripts of ideographic and syllabic text that no hunspell dictionary lists words for: checking a run of them
  * against a dictionary would flag every word. A dictionary that does declare letters of one of them takes it back, so
  * a script is skipped only while nothing installed covers it (#1866).
  */
private[spellcheck] object CheckableScripts:

  private val Uncovered: Set[UnicodeScript] =
    Set(UnicodeScript.HAN, UnicodeScript.HIRAGANA, UnicodeScript.KATAKANA, UnicodeScript.HANGUL)

  /** The first codepoint of any of them (Hangul Jamo); text below it is never cut, which keeps the common word free. */
  private val FirstUncovered = 0x1100

  private val Joiners = Set('\'', '’', 'ʼ', '-', '‐', '‑')

  /** The uncovered scripts that `letters` has a letter of. */
  def declaredBy(letters: Iterable[String]): Set[UnicodeScript] =
    letters.iterator
      .flatMap(_.codePoints().filter(_ >= FirstUncovered).toArray)
      .map(UnicodeScript.of)
      .filter(Uncovered.contains)
      .toSet

  /** The parts of `word` to check: the word itself unless it has letters in an uncovered script, else the runs between
    * them, with the joining hyphens and apostrophes the cut leaves at their ends dropped.
    */
  def checkable(word: ProseWord, declared: Set[UnicodeScript]): List[ProseWord] =
    if word.text.forall(_ < FirstUncovered) then List(word)
    else
      def skipped(codePoint: Int): Boolean =
        val script = UnicodeScript.of(codePoint)
        Uncovered.contains(script) && !declared.contains(script)
      val text = word.text
      val offsets = Iterator
        .iterate(0)(index => index + Character.charCount(text.codePointAt(index)))
        .takeWhile(_ < text.length)
        .toVector
      val kept = offsets.filterNot(index => skipped(text.codePointAt(index)))
      if kept.length == offsets.length then List(word)
      else
        val runs = kept.foldLeft(List.empty[(Int, Int)]) { (found, index) =>
          val end = index + Character.charCount(text.codePointAt(index))
          found match
            case (from, until) :: earlier if until == index => (from, end) :: earlier
            case _                                          => (index, end) :: found
        }
        runs.reverse.flatMap((from, until) => trimmed(text, from, until)).map { (from, until) =>
          ProseWord(text.substring(from, until), word.start + from, word.start + until)
        }

  private def trimmed(text: String, from: Int, until: Int): Option[(Int, Int)] =
    val start = (from until until).find(index => !Joiners.contains(text.charAt(index)))
    val end   = (until until from by -1).find(index => !Joiners.contains(text.charAt(index - 1)))
    for
      first <- start
      last  <- end
      if first < last
    yield (first, last)
