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
