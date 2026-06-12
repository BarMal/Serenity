package com.serenity.text

object TextEditing:

  private enum CharacterClass:
    case Whitespace, Word, Punctuation

  def deleteWordBackward(text: String): String =
    val boundary = previousWordBoundary(text, text.length)
    text.substring(0, boundary) + text.substring(text.length)

  def deleteWordForward(text: String): String =
    val boundary = nextWordBoundary(text, text.length)
    text.substring(0, text.length) + text.substring(boundary)

  def previousWordBoundary(text: String, cursor: Int): Int =
    val idx = clamp(cursor, text.length)
    val segmentEnd =
      if idx > 0 && characterClass(text.charAt(idx - 1)) == CharacterClass.Whitespace then
        scanBackwardClassStart(text, idx, CharacterClass.Whitespace)
      else idx

    if segmentEnd <= 0 then 0
    else scanBackwardClassStart(text, segmentEnd, characterClass(text.charAt(segmentEnd - 1)))

  def nextWordBoundary(text: String, cursor: Int): Int =
    val length = text.length
    val idx    = clamp(cursor, length)
    val segmentStart =
      if idx < length && characterClass(text.charAt(idx)) == CharacterClass.Whitespace then
        scanForwardClassEnd(text, idx, CharacterClass.Whitespace)
      else idx

    if segmentStart >= length then length
    else
      val segmentEnd = scanForwardClassEnd(text, segmentStart, characterClass(text.charAt(segmentStart)))
      if segmentEnd < length && characterClass(text.charAt(segmentEnd)) == CharacterClass.Whitespace then
        scanForwardClassEnd(text, segmentEnd, CharacterClass.Whitespace)
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
  private def scanBackwardClassStart(text: String, idx: Int, targetClass: CharacterClass): Int =
    if idx > 0 && characterClass(text.charAt(idx - 1)) == targetClass then
      scanBackwardClassStart(text, idx - 1, targetClass)
    else idx

  @annotation.tailrec
  private def scanForwardClassEnd(text: String, idx: Int, targetClass: CharacterClass): Int =
    if idx < text.length && characterClass(text.charAt(idx)) == targetClass then
      scanForwardClassEnd(text, idx + 1, targetClass)
    else idx
