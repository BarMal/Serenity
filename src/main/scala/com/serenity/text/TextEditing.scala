package com.serenity.text

object TextEditing:

  def deleteWordBackward(text: String): String =
    val boundary = previousWordBoundary(text, text.length)
    text.substring(0, boundary) + text.substring(text.length)

  def deleteWordForward(text: String): String =
    val boundary = nextWordBoundary(text, text.length)
    text.substring(0, text.length) + text.substring(boundary)

  def previousWordBoundary(text: String, cursor: Int): Int =
    var idx = clamp(cursor, text.length)
    if idx > 0 && text.charAt(idx - 1).isWhitespace then
      while idx > 0 && text.charAt(idx - 1).isWhitespace do idx -= 1
      while idx > 0 && !text.charAt(idx - 1).isWhitespace do idx -= 1
    else
      while idx > 0 && !text.charAt(idx - 1).isWhitespace do idx -= 1
    idx

  def nextWordBoundary(text: String, cursor: Int): Int =
    val length = text.length
    var idx    = clamp(cursor, length)
    if idx < length && text.charAt(idx).isWhitespace then
      while idx < length && text.charAt(idx).isWhitespace do idx += 1
      while idx < length && !text.charAt(idx).isWhitespace do idx += 1
    else
      while idx < length && !text.charAt(idx).isWhitespace do idx += 1
      while idx < length && text.charAt(idx).isWhitespace do idx += 1
    idx

  private def clamp(cursor: Int, length: Int): Int =
    math.max(0, math.min(cursor, length))
