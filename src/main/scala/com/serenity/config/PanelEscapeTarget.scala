package com.serenity.config

/** Where Escape from a focused panel sends focus: the editor's active pane, or whatever had focus before the panel. */
enum PanelEscapeTarget(val configKey: String):
  case Editor   extends PanelEscapeTarget("editor")
  case Previous extends PanelEscapeTarget("previous")

object PanelEscapeTarget:

  def fromConfigKey(value: String): Option[PanelEscapeTarget] =
    PanelEscapeTarget.values.find(_.configKey == value.trim.toLowerCase)
