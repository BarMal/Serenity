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
      scanBackwardNonWhitespace(scanBackwardWhitespace(idx, text), text)
    else scanBackwardNonWhitespace(idx, text)

  def nextWordBoundary(text: String, cursor: Int): Int =
    val length = text.length
    val idx    = clamp(cursor, length)
    if idx < length && text.charAt(idx).isWhitespace then
      scanForwardNonWhitespace(scanForwardWhitespace(idx, text), text)
    else scanForwardWhitespace(scanForwardNonWhitespace(idx, text), text)

  private def clamp(cursor: Int, length: Int): Int =
    math.max(0, math.min(cursor, length))

  private def scanBackwardWhitespace(index: Int, text: String): Int =
    if index > 0 && text.charAt(index - 1).isWhitespace then scanBackwardWhitespace(index - 1, text)
    else index

  private def scanBackwardNonWhitespace(index: Int, text: String): Int =
    if index > 0 && !text.charAt(index - 1).isWhitespace then scanBackwardNonWhitespace(index - 1, text)
    else index

  private def scanForwardWhitespace(index: Int, text: String): Int =
    if index < text.length && text.charAt(index).isWhitespace then scanForwardWhitespace(index + 1, text)
    else index

  private def scanForwardNonWhitespace(index: Int, text: String): Int =
    if index < text.length && !text.charAt(index).isWhitespace then scanForwardNonWhitespace(index + 1, text)
    else index
