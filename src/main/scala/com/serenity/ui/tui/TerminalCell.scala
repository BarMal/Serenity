package com.serenity.ui.tui

import java.awt.Color

import com.serenity.ui.theme.TextStyle

/** A cell's relationship to the glyph occupying it. A wide glyph (CJK, most emoji) is stored once, at its leading
  * column, and reserves the column to its right as [[Continuation]] so that column math and damage-diffing stay cell-
  * aligned; [[Continuation]] carries no printable content of its own.
  */
enum CellSpan:
  case Narrow, Wide, Continuation

/** One cell of a [[TerminalScreenBuffer]]: a single Unicode codepoint (not a full grapheme cluster) plus the colour and
  * style it was drawn with.
  */
final case class TerminalCell(
    codePoint: Int,
    fg: Color,
    bg: Color,
    style: TextStyle,
    span: CellSpan
):
  /** The printable text for this cell -- empty for [[CellSpan.Continuation]], which is never drawn on its own. */
  def text: String =
    if span == CellSpan.Continuation then "" else new String(Character.toChars(codePoint))

object TerminalCell:

  def blank(fg: Color, bg: Color): TerminalCell =
    TerminalCell(' '.toInt, fg, bg, TextStyle.normal, CellSpan.Narrow)

  def continuation(fg: Color, bg: Color, style: TextStyle): TerminalCell =
    TerminalCell(' '.toInt, fg, bg, style, CellSpan.Continuation)
