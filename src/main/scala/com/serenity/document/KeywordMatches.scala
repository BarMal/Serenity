package com.serenity.document

import java.util.Locale

/** Where a keyword sits on a line: columns `start` until `end`. */
final case class KeywordOccurrence(start: Int, end: Int):
  def touches(column: Int): Boolean = start <= column && column <= end

/** Finding keywords in prose. A keyword is a run of words, and a word is a maximal run of letters, so whitespace and
  * every other character -- punctuation, digits, underscores -- ends one. A term is therefore never read as a pattern,
  * and "Liz" is found in "Liz's" but not in "Elizabeth".
  */
object KeywordMatches:

  final private case class Word(text: String, start: Int, end: Int)

  /** The form a keyword is stored in: its lower-cased words joined by single spaces. Empty when it has no letters. */
  def normalized(text: String): String = wordsOf(text).map(_.text).mkString(" ")

  /** The occurrences of `term` on `line`, ignoring case, left to right without overlapping. The words of a multi-word
    * term may be separated by any run of non-letters.
    */
  def occurrences(line: String, term: String): List[KeywordOccurrence] =
    val wanted = wordsOf(term).map(_.text)
    if wanted.isEmpty then Nil
    else matchesFrom(wordsOf(line), wanted, Nil).reverse

  /** The run of letters touching `column`, as written. */
  def wordAt(line: String, column: Int): Option[String] =
    wordsOf(line, lowerCase = false).collectFirst {
      case word if KeywordOccurrence(word.start, word.end).touches(column) => word.text
    }

  /** The longest of `terms` with an occurrence on `line` touching `column`. */
  def termAt(line: String, column: Int, terms: Iterable[String]): Option[String] =
    terms
      .filter(term => occurrences(line, term).exists(_.touches(column)))
      .maxByOption(term => wordsOf(term).length -> term.length)

  @scala.annotation.tailrec
  private def matchesFrom(
    words: List[Word],
    wanted: List[String],
    found: List[KeywordOccurrence]
  ): List[KeywordOccurrence] =
    words match
      case Nil => found
      case first :: rest =>
        val candidate = words.take(wanted.length)
        candidate.lastOption.filter(_ => candidate.map(_.text) == wanted) match
          case Some(last) =>
            matchesFrom(words.drop(wanted.length), wanted, KeywordOccurrence(first.start, last.end) :: found)
          case None => matchesFrom(rest, wanted, found)

  private def wordsOf(text: String, lowerCase: Boolean = true): List[Word] =
    val starts = text.indices.filter(i => text.charAt(i).isLetter && (i == 0 || !text.charAt(i - 1).isLetter))
    starts.toList.map { start =>
      val stop  = text.indexWhere(!_.isLetter, start)
      val end   = if stop < 0 then text.length else stop
      val slice = text.substring(start, end)
      Word(if lowerCase then slice.toLowerCase(Locale.ROOT) else slice, start, end)
    }
