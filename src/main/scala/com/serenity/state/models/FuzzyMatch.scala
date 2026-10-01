package com.serenity.state.models

/** Scores how well a typed query matches a `/`-separated relative path, as a file finder ranks them: the query's
  * characters must appear in the path in order, ignoring case. Each matched character scores more when it is in the
  * file name rather than a directory, follows the previous match directly, or starts a word (after `/`, `_`, `-`, `.`
  * or a space, or a camelCase hump); a longer path scores a little less overall.
  */
object FuzzyMatch:

  private val MatchScore        = 16
  private val FileNameBonus     = 16
  private val SegmentStartBonus = 12
  private val ConsecutiveBonus  = 12
  private val Unmatched         = Int.MinValue / 2
  private val Separators        = Set('/', '_', '-', '.', ' ')

  /** `None` when `path` doesn't hold the query's characters in order; otherwise higher is better. */
  def score(query: String, path: String): Option[Int] =
    // Per character rather than `toLowerCase`, which can change a string's length and so misalign `lower` with `path`.
    val needle = query.map(_.toLower)
    val lower  = path.map(_.toLower)
    Option.when(isSubsequence(needle, lower))(bestAlignment(needle, path, lower) - path.length)

  private def isSubsequence(needle: String, haystack: String): Boolean =
    needle
      .foldLeft(Option(0))((from, char) => from.map(haystack.indexOf(char, _)).filter(_ >= 0).map(_ + 1))
      .isDefined

  /** The best total over every way of placing the query's characters in order. Each row holds, for every position, the
    * best score of the query so far with its last character placed exactly there.
    */
  private def bestAlignment(needle: String, path: String, lower: String): Int =
    if needle.isEmpty then 0
    else
      val nameStart  = path.lastIndexOf('/') + 1
      val charScores = IArray.tabulate(path.length)(index => characterScore(path, index, nameStart))
      val first =
        IArray.tabulate(path.length)(index => if lower(index) == needle(0) then charScores(index) else Unmatched)
      val last = needle.drop(1).foldLeft(first) { (previous, char) =>
        // bestBefore(j) is the best placement of the previous character anywhere before position j.
        val bestBefore = previous.scanLeft(Unmatched)(math.max)
        IArray.tabulate(path.length) { index =>
          if index == 0 || lower(index) != char then Unmatched
          else
            val placed = math.max(bestBefore(index - 1), previous(index - 1) + ConsecutiveBonus)
            if placed < 0 then Unmatched else placed + charScores(index)
        }
      }
      last.foldLeft(Unmatched)(math.max)

  private def characterScore(path: String, index: Int, nameStart: Int): Int =
    val inFileName = if index >= nameStart then FileNameBonus else 0
    val startsWord = if startsSegment(path, index) then SegmentStartBonus else 0
    MatchScore + inFileName + startsWord

  private def startsSegment(path: String, index: Int): Boolean =
    index == 0 || Separators(path(index - 1)) || (path(index - 1).isLower && path(index).isUpper)
