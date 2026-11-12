package com.serenity.ui.layout

object FallbackHeaderText:

  val Hint: String = "Esc dismisses"

  private val Gap      = "  "
  private val Ellipsis = "…"

  /** The message cut from its end, so the hint stays for as long as the row can hold both; the hint goes first when
    * even a one-character message would not fit beside it.
    */
  def fit(message: String, hint: String, width: Int): String =
    val room = width - hint.length - Gap.length
    if width <= 0 then ""
    else if message.length + Gap.length + hint.length <= width then s"$message$Gap$hint"
    else if room >= 2 then s"${message.take(room - 1)}$Ellipsis$Gap$hint"
    else if message.length <= width then message
    else s"${message.take(width - 1)}$Ellipsis"
