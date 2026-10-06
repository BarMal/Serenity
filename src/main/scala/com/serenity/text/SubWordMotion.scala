package com.serenity.text

import scala.annotation.tailrec

import com.serenity.text.TextEditing.{CharacterClass, CharacterSource}

/** Motion by the parts of an identifier (`camelCase`, `PascalCase`, `snake_case`), for the editor's Ctrl+Alt+Arrow
  * keys.
  *
  * A part ends where the case turns from lower case or a digit to upper case (`fooBar`, `utf8Decoder`), before the last
  * capital of an acronym followed by lower case (`XMLHttp`) and after the underscores that follow a part (`foo_`).
  * Leading underscores belong to the part after them. Everything that is not a run of letters, digits and underscores
  * moves as whole-word motion does, so the two commands differ only inside identifiers.
  */
private[text] object SubWordMotion:

  private val Underscore = '_'

  def previous(source: CharacterSource, cursor: Int): Int =
    val runEnd = TextEditing.scanBackwardWhitespaceStart(source, math.max(0, math.min(cursor, source.length)))
    if runEnd <= 0 || !isIdentifierBefore(source, runEnd) then TextEditing.previousWordBoundary(source, runEnd)
    else
      val floor = math.max(
        identifierStartBefore(source, runEnd),
        TextEditing.wordBreakIterator(source).preceding(runEnd)
      )
      lastPartBoundaryBefore(source, runEnd, floor).getOrElse(floor)

  def next(source: CharacterSource, cursor: Int): Int =
    val start = TextEditing.scanForwardWhitespaceEnd(source, math.max(0, math.min(cursor, source.length)))
    if start >= source.length || !isIdentifierAt(source, start) then TextEditing.nextWordBoundary(source, start)
    else
      val ceiling = math.min(identifierEndAfter(source, start), TextEditing.wordBreakIterator(source).following(start))
      firstPartBoundaryAfter(source, start, ceiling).getOrElse(TextEditing.scanForwardWhitespaceEnd(source, ceiling))

  @tailrec
  private def identifierStartBefore(source: CharacterSource, index: Int): Int =
    if index > 0 && isIdentifierBefore(source, index) then
      identifierStartBefore(source, index - Character.charCount(TextEditing.codePointBefore(source, index)))
    else index

  @tailrec
  private def identifierEndAfter(source: CharacterSource, index: Int): Int =
    if index < source.length && isIdentifierAt(source, index) then
      identifierEndAfter(source, index + Character.charCount(TextEditing.codePointAt(source, index)))
    else index

  private def isIdentifierAt(source: CharacterSource, index: Int): Boolean =
    source.charAt(index) == Underscore || TextEditing.characterClassAt(source, index) == CharacterClass.Word

  private def isIdentifierBefore(source: CharacterSource, index: Int): Boolean =
    source.charAt(index - 1) == Underscore || TextEditing.characterClassBefore(source, index) == CharacterClass.Word

  @tailrec
  private def firstPartBoundaryAfter(source: CharacterSource, from: Int, limit: Int): Option[Int] =
    val candidate = from + 1
    if candidate >= limit then None
    else if isPartBoundary(source, candidate) then Some(candidate)
    else firstPartBoundaryAfter(source, candidate, limit)

  @tailrec
  private def lastPartBoundaryBefore(source: CharacterSource, from: Int, limit: Int): Option[Int] =
    val candidate = from - 1
    if candidate <= limit then None
    else if isPartBoundary(source, candidate) then Some(candidate)
    else lastPartBoundaryBefore(source, candidate, limit)

  private def isPartBoundary(source: CharacterSource, index: Int): Boolean =
    val before = source.charAt(index - 1)
    val after  = source.charAt(index)
    val next   = Option.when(index + 1 < source.length)(source.charAt(index + 1))
    if before == Underscore then after != Underscore && followsWord(source, underscoreRunStart(source, index))
    else
      ((before.isLower || before.isDigit) && after.isUpper) ||
      (before.isUpper && after.isUpper && next.exists(_.isLower))

  private def followsWord(source: CharacterSource, runStart: Int): Boolean =
    runStart > 0 && TextEditing.characterClassBefore(source, runStart) == CharacterClass.Word

  @tailrec
  private def underscoreRunStart(source: CharacterSource, index: Int): Int =
    if index > 0 && source.charAt(index - 1) == Underscore then underscoreRunStart(source, index - 1) else index
