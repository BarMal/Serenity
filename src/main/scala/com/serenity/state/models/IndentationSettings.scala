package com.serenity.state.models

/** How indentation is typed and how wide a tab character is drawn. `tabSize` is both: the number of spaces one level of
  * indentation inserts, and the distance between the tab stops a tab character advances to.
  */
final case class IndentationSettings(
    tabSize: Int = IndentationSettings.DefaultTabSize,
    insertSpaces: Boolean = true
):

  def normalized: IndentationSettings =
    copy(tabSize = tabSize.max(IndentationSettings.MinTabSize).min(IndentationSettings.MaxTabSize))

  /** What Tab inserts, and what Shift+Tab takes away one of. */
  def indentUnit: String =
    if insertSpaces then " " * normalized.tabSize else "\t"

object IndentationSettings:

  val MinTabSize: Int     = 1
  val MaxTabSize: Int     = 16
  val DefaultTabSize: Int = 4

  val Default: IndentationSettings = IndentationSettings()
