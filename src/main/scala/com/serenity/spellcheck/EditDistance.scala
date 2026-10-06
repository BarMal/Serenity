package com.serenity.spellcheck

import scala.annotation.tailrec

/** Damerau-Levenshtein (optimal string alignment) distance: insertions, deletions, substitutions and adjacent
  * transpositions each cost one edit, so a swapped pair of letters is one typo rather than two.
  */
private[spellcheck] object EditDistance:

  /** The distance between `a` and `b` if it is at most `limit`, else `None`, giving up once two consecutive rows of the
    * table are all over it (a transposition reaches back two rows).
    */
  def within(a: String, b: String, limit: Int): Option[Int] =
    if a == b then Some(0)
    else if math.abs(a.length - b.length) > limit then None
    else if math.abs(a.length - b.length) <= 1 && oneEdit(a, b) then Some(1)
    else if limit < 2 then None
    else rows(a, b, limit, 1, Array.fill(b.length + 1)(Int.MaxValue), Array.tabulate(b.length + 1)(identity))

  def between(a: String, b: String): Int =
    within(a, b, math.max(a.length, b.length)).getOrElse(math.max(a.length, b.length))

  /** The allocation-free test that covers almost every candidate a suggestion search rejects: `a` and `b` (different)
    * differ by one substitution, transposition, insertion or deletion; their lengths differ by at most one.
    */
  private def oneEdit(a: String, b: String): Boolean =
    val shared = a.iterator.zip(b.iterator).takeWhile((x, y) => x == y).size
    if a.length == b.length then
      a.regionMatches(shared + 1, b, shared + 1, a.length - shared - 1) ||
      (shared + 1 < a.length && a.charAt(shared) == b.charAt(shared + 1) && a.charAt(shared + 1) == b.charAt(shared) &&
        a.regionMatches(shared + 2, b, shared + 2, a.length - shared - 2))
    else
      val (longer, shorter) = if a.length > b.length then (a, b) else (b, a)
      longer.regionMatches(shared + 1, shorter, shared, shorter.length - shared)

  @tailrec
  private def rows(
    a: String,
    b: String,
    limit: Int,
    i: Int,
    beforePrevious: Array[Int],
    previous: Array[Int]
  ): Option[Int] =
    if i > a.length then Some(previous(b.length)).filter(_ <= limit)
    else
      val current = nextRow(a, b, i, beforePrevious, previous)
      if current.forall(_ > limit) && previous.forall(_ > limit) then None
      else rows(a, b, limit, i + 1, previous, current)

  // A row is filled left to right into an array private to this call; nothing else sees it until it is complete.
  private def nextRow(a: String, b: String, i: Int, beforePrevious: Array[Int], previous: Array[Int]): Array[Int] =
    val row = new Array[Int](b.length + 1)
    row(0) = i
    (1 to b.length).foreach { j =>
      val substitution = previous(j - 1) + (if a.charAt(i - 1) == b.charAt(j - 1) then 0 else 1)
      val transposition =
        if i > 1 && j > 1 && a.charAt(i - 1) == b.charAt(j - 2) && a.charAt(i - 2) == b.charAt(j - 1) then
          beforePrevious(j - 2) + 1
        else Int.MaxValue
      row(j) = math.min(math.min(previous(j) + 1, row(j - 1) + 1), math.min(substitution, transposition))
    }
    row
