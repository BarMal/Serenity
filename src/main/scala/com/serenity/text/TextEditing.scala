package com.serenity.text

object TextEditing:

  /** Minimal indexed character access for word-boundary scanning without requiring a String. */
  trait CharacterSource:
    def length: Int
    def charAt(index: Int): Char

  private enum CharacterClass:
    case Whitespace, Word, Punctuation

  def deleteWordBackward(text: String): String =
    val boundary = previousWordBoundary(text, text.length)
    text.substring(0, boundary) + text.substring(text.length)

  def deleteWordForward(text: String): String =
    val boundary = nextWordBoundary(text, text.length)
    text.substring(0, text.length) + text.substring(boundary)

  def previousWordBoundary(text: String, cursor: Int): Int =
    previousWordBoundary(StringCharacterSource(text), cursor)

  def previousWordBoundary(source: CharacterSource, cursor: Int): Int =
    val idx = clamp(cursor, source.length)
    val segmentEnd =
      if idx > 0 && characterClass(source.charAt(idx - 1)) == CharacterClass.Whitespace then
        scanBackwardClassStart(source, idx, CharacterClass.Whitespace)
      else idx

    if segmentEnd <= 0 then 0
    else scanBackwardClassStart(source, segmentEnd, characterClass(source.charAt(segmentEnd - 1)))

  def nextWordBoundary(text: String, cursor: Int): Int =
    nextWordBoundary(StringCharacterSource(text), cursor)

  def nextWordBoundary(source: CharacterSource, cursor: Int): Int =
    val length = source.length
    val idx    = clamp(cursor, length)
    val segmentStart =
      if idx < length && characterClass(source.charAt(idx)) == CharacterClass.Whitespace then
        scanForwardClassEnd(source, idx, CharacterClass.Whitespace)
      else idx

    if segmentStart >= length then length
    else
      val segmentEnd = scanForwardClassEnd(source, segmentStart, characterClass(source.charAt(segmentStart)))
      if segmentEnd < length && characterClass(source.charAt(segmentEnd)) == CharacterClass.Whitespace then
        scanForwardClassEnd(source, segmentEnd, CharacterClass.Whitespace)
      else segmentEnd

  def previousGraphemeBoundary(text: String, cursor: Int): Int =
    previousGraphemeBoundary(StringCharacterSource(text), cursor)

  def previousGraphemeBoundary(source: CharacterSource, cursor: Int): Int =
    val idx = clamp(cursor, source.length)
    if idx <= 0 then 0
    else rewindGraphemeStart(source, previousCodePointStart(source, idx))

  def nextGraphemeBoundary(text: String, cursor: Int): Int =
    nextGraphemeBoundary(StringCharacterSource(text), cursor)

  def nextGraphemeBoundary(source: CharacterSource, cursor: Int): Int =
    val idx = clamp(cursor, source.length)
    if idx >= source.length then source.length
    else consumeGraphemeEnd(source, nextCodePointEnd(source, idx))

  def graphemeBoundaryBeforeOrAt(text: String, cursor: Int): Int =
    graphemeBoundaryBeforeOrAt(StringCharacterSource(text), cursor)

  def graphemeBoundaryBeforeOrAt(source: CharacterSource, cursor: Int): Int =
    val idx = clamp(cursor, source.length)
    if idx <= 0 then 0
    else
      val previous = previousGraphemeBoundary(source, idx)
      if nextGraphemeBoundary(source, previous) == idx then idx
      else previous

  def graphemeBoundaryAfterOrAt(text: String, cursor: Int): Int =
    graphemeBoundaryAfterOrAt(StringCharacterSource(text), cursor)

  def graphemeBoundaryAfterOrAt(source: CharacterSource, cursor: Int): Int =
    val idx = clamp(cursor, source.length)
    if idx >= source.length then source.length
    else
      val next = nextGraphemeBoundary(source, idx)
      if previousGraphemeBoundary(source, next) == idx then idx
      else next

  private def clamp(cursor: Int, length: Int): Int =
    math.max(0, math.min(cursor, length))

  private def characterClass(char: Char): CharacterClass =
    if char.isWhitespace then CharacterClass.Whitespace
    else
      Character.getType(char) match
        case Character.UPPERCASE_LETTER | Character.LOWERCASE_LETTER | Character.TITLECASE_LETTER |
            Character.MODIFIER_LETTER | Character.OTHER_LETTER | Character.DECIMAL_DIGIT_NUMBER |
            Character.LETTER_NUMBER | Character.OTHER_NUMBER | Character.NON_SPACING_MARK |
            Character.COMBINING_SPACING_MARK =>
          CharacterClass.Word
        case _ =>
          CharacterClass.Punctuation

  private def codePointAt(source: CharacterSource, index: Int): Int =
    val first = source.charAt(index)
    if Character.isHighSurrogate(first) && index + 1 < source.length then
      val second = source.charAt(index + 1)
      if Character.isLowSurrogate(second) then Character.toCodePoint(first, second)
      else first.toInt
    else first.toInt

  private def previousCodePointStart(source: CharacterSource, idx: Int): Int =
    if idx >= 2 && Character.isLowSurrogate(source.charAt(idx - 1)) &&
        Character.isHighSurrogate(source.charAt(idx - 2))
    then idx - 2
    else math.max(0, idx - 1)

  private def nextCodePointEnd(source: CharacterSource, idx: Int): Int =
    if idx + 1 < source.length && Character.isHighSurrogate(source.charAt(idx)) &&
        Character.isLowSurrogate(source.charAt(idx + 1))
    then idx + 2
    else math.min(source.length, idx + 1)

  @annotation.tailrec
  private def rewindGraphemeStart(source: CharacterSource, idx: Int): Int =
    val previous =
      if idx > 0 then
        val previousStart = previousCodePointStart(source, idx)
        Option.when(codePointAt(source, previousStart) == ZeroWidthJoiner && previousStart > 0) {
          previousCodePointStart(source, previousStart)
        }
      else None

    val currentCodePoint = codePointAt(source, idx)
    if idx > 0 && isGraphemeExtender(currentCodePoint) then
      rewindGraphemeStart(source, previousCodePointStart(source, idx))
    else
      previous match
        case Some(joinedStart) => rewindGraphemeStart(source, joinedStart)
        case None              => idx

  @annotation.tailrec
  private def consumeGraphemeEnd(source: CharacterSource, idx: Int): Int =
    if idx >= source.length then source.length
    else
      val codePoint = codePointAt(source, idx)
      if isGraphemeExtender(codePoint) then consumeGraphemeEnd(source, nextCodePointEnd(source, idx))
      else if codePoint == ZeroWidthJoiner && nextCodePointEnd(source, idx) < source.length then
        consumeGraphemeEnd(source, nextCodePointEnd(source, nextCodePointEnd(source, idx)))
      else idx

  private val ZeroWidthJoiner    = 0x200d
  private val EmojiModifierStart = 0x1f3fb
  private val EmojiModifierEnd   = 0x1f3ff

  private def isGraphemeExtender(codePoint: Int): Boolean =
    isEmojiModifier(codePoint) ||
      (Character.getType(codePoint) match
        case Character.NON_SPACING_MARK | Character.COMBINING_SPACING_MARK | Character.ENCLOSING_MARK => true
        case _                                                                                        => false)

  private def isEmojiModifier(codePoint: Int): Boolean =
    codePoint >= EmojiModifierStart && codePoint <= EmojiModifierEnd

  @annotation.tailrec
  private def scanBackwardClassStart(source: CharacterSource, idx: Int, targetClass: CharacterClass): Int =
    if idx > 0 && characterClass(source.charAt(idx - 1)) == targetClass then
      scanBackwardClassStart(source, idx - 1, targetClass)
    else idx

  @annotation.tailrec
  private def scanForwardClassEnd(source: CharacterSource, idx: Int, targetClass: CharacterClass): Int =
    if idx < source.length && characterClass(source.charAt(idx)) == targetClass then
      scanForwardClassEnd(source, idx + 1, targetClass)
    else idx

  final private case class StringCharacterSource(text: String) extends CharacterSource:
    override def length: Int =
      text.length

    override def charAt(index: Int): Char =
      text.charAt(index)
