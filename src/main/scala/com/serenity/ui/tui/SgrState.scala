package com.serenity.ui.tui

import java.awt.Color

/** The SGR attributes a terminal holds after [[TerminalAnsiDiff]] has drawn a cell, so the next cell is reached by
  * sending only what differs. A background with alpha 0 is stored as [[SgrState.NativeBackground]]: it is drawn with
  * SGR 49 (the terminal's own background) and every such background is the same state.
  */
final case class SgrState(fg: Color, bg: Color, bold: Boolean, italic: Boolean, underline: Boolean)

object SgrState:

  val NativeBackground: Color = new Color(0, 0, 0, 0)

  private val Esc = 0x1b.toChar

  def of(cell: TerminalCell): SgrState =
    SgrState(
      cell.fg,
      if cell.bg.getAlpha == 0 then NativeBackground else cell.bg,
      cell.style.isBold,
      cell.style.isItalic,
      cell.style.isUnderlined
    )

  /** The sequence that takes a terminal from `from` to `to`, or `""` when they are equal. `from` is `None` when the
    * terminal's attributes are unknown, which needs a full reset-and-set. When both forms are possible the shorter one
    * wins: switching several attributes off at once is cheaper as a reset than as one "off" code each.
    */
  def transition(from: Option[SgrState], to: SgrState): String =
    val params = from match
      case Some(held) if held == to => Nil
      case Some(held)               => shorter(deltaParams(held, to), resetParams(to))
      case None                     => resetParams(to)
    if params.isEmpty then "" else s"$Esc[${params.mkString(";")}m"

  private def shorter(delta: List[String], reset: List[String]): List[String] =
    if reset.mkString(";").length < delta.mkString(";").length then reset else delta

  private def deltaParams(from: SgrState, to: SgrState): List[String] =
    List(
      toggle(from.bold, to.bold, on = "1", off = "22"),
      toggle(from.italic, to.italic, on = "3", off = "23"),
      toggle(from.underline, to.underline, on = "4", off = "24"),
      Option.when(from.fg != to.fg)(foregroundParam(to.fg)),
      Option.when(from.bg != to.bg)(backgroundParam(to.bg))
    ).flatten

  private def resetParams(to: SgrState): List[String] =
    List(
      Some("0"),
      Option.when(to.bold)("1"),
      Option.when(to.italic)("3"),
      Option.when(to.underline)("4"),
      Some(foregroundParam(to.fg)),
      Some(backgroundParam(to.bg))
    ).flatten

  private def toggle(was: Boolean, now: Boolean, on: String, off: String): Option[String] =
    Option.when(was != now)(if now then on else off)

  private def foregroundParam(fg: Color): String = s"38;2;${fg.getRed};${fg.getGreen};${fg.getBlue}"

  private def backgroundParam(bg: Color): String =
    if bg.getAlpha == 0 then "49" else s"48;2;${bg.getRed};${bg.getGreen};${bg.getBlue}"
