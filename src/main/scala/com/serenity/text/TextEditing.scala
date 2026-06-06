package com.serenity.text

object TextEditing:

  def deleteWordBackward(text: String): String =
    val boundary = previousWordBoundary(text, text.length)
    text.substring(0, boundary) + text.substring(text.length)

  def deleteWordForward(text: String): String =
    val boundary = nextWordBoundary(text, text.length)
    text.substring(0, text.length) + text.substring(boundary)

  def previousWordBoundary(text: String, cursor: Int): Int =
    val idx = clamp(cursor, text.length)
    if idx > 0 && text.charAt(idx - 1).isWhitespace then
      scanBackwardNonWhitespaceStart(text, scanBackwardWhitespaceStart(text, idx))
    else scanBackwardNonWhitespaceStart(text, idx)

  def nextWordBoundary(text: String, cursor: Int): Int =
    val length = text.length
    val idx    = clamp(cursor, length)
    if idx < length && text.charAt(idx).isWhitespace then
      scanForwardNonWhitespaceEnd(text, scanForwardWhitespaceEnd(text, idx))
    else scanForwardWhitespaceEnd(text, scanForwardNonWhitespaceEnd(text, idx))

  private def clamp(cursor: Int, length: Int): Int =
    math.max(0, math.min(cursor, length))

  @annotation.tailrec
  private def scanBackwardWhitespaceStart(text: String, idx: Int): Int =
    if idx > 0 && text.charAt(idx - 1).isWhitespace then scanBackwardWhitespaceStart(text, idx - 1)
    else idx

  @annotation.tailrec
  private def scanBackwardNonWhitespaceStart(text: String, idx: Int): Int =
    if idx > 0 && !text.charAt(idx - 1).isWhitespace then scanBackwardNonWhitespaceStart(text, idx - 1)
    else idx

  @annotation.tailrec
  private def scanForwardWhitespaceEnd(text: String, idx: Int): Int =
    if idx < text.length && text.charAt(idx).isWhitespace then scanForwardWhitespaceEnd(text, idx + 1)
    else idx

  @annotation.tailrec
  private def scanForwardNonWhitespaceEnd(text: String, idx: Int): Int =
    if idx < text.length && !text.charAt(idx).isWhitespace then scanForwardNonWhitespaceEnd(text, idx + 1)
    else idx
