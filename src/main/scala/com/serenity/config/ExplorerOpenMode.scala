package com.serenity.config

import java.util.Locale

/** How a click in the explorer opens a file. `Preview` opens it in a temporary tab that the next preview replaces. */
enum ExplorerOpenMode(val configKey: String):
  case DoubleClick extends ExplorerOpenMode("double-click")
  case SingleClick extends ExplorerOpenMode("single-click")
  case Preview     extends ExplorerOpenMode("preview")

object ExplorerOpenMode:

  def fromConfigKey(value: String): Option[ExplorerOpenMode] =
    ExplorerOpenMode.values.find(_.configKey == value.trim.toLowerCase(Locale.ROOT))
