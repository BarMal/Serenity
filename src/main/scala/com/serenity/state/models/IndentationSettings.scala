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

  /** How the status line names these settings, such as `Spaces: 4`. */
  def statusLabel: String =
    if insertSpaces then s"Spaces: $tabSize" else s"Tab Size: $tabSize"

  /** What Tab inserts, and what Shift+Tab takes away one of. */
  def indentUnit: String =
    if insertSpaces then " " * normalized.tabSize else "\t"

object IndentationSettings:

  val MinTabSize: Int     = 1
  val MaxTabSize: Int     = 16
  val DefaultTabSize: Int = 4

  val Default: IndentationSettings = IndentationSettings()

  /** A setting as a session stores it, such as `spaces:4` or `tabs:8`. */
  def configKey(settings: IndentationSettings): String =
    s"${if settings.insertSpaces then "spaces" else "tabs"}:${settings.tabSize}"

  def fromConfigKey(key: String): Option[IndentationSettings] =
    key.split(":") match
      case Array(style @ ("spaces" | "tabs"), size) =>
        size.toIntOption
          .filter(width => width >= MinTabSize && width <= MaxTabSize)
          .map(IndentationSettings(_, insertSpaces = style == "spaces"))
      case _ => None
