package com.serenity.state.models

import com.serenity.config.*
import com.serenity.ui.layout.Layout
import com.serenity.ui.theme.Theme

/** State that round-trips through `session/SessionState.scala`. */
final case class Persisted(
    layout: Layout,
    buffers: Map[BufferId, Buffer],
    focus: Focus,
    bufferOrder: List[BufferId] = List.empty, // Tracks buffer creation and navigation order
    theme: Theme = Theme.default,
    config: AppConfig = AppConfig.default,
    recentFiles: List[java.nio.file.Path] = Nil,
    // Tagged with the AppMode active when each file was opened, so the mode/tab widget can offer "recent projects
    // opened in this mode" (issue #1307) rather than reusing the mode-agnostic `recentFiles` list above.
    recentFilesByMode: Map[AppMode, List[java.nio.file.Path]] = Map.empty
)

object Persisted:

  /** Records `path` under `mode`'s own recent list: moved to the front if already present, capped at 20, other modes'
    * lists untouched.
    */
  def trackRecentFile(
    current: Map[AppMode, List[java.nio.file.Path]],
    mode: AppMode,
    path: java.nio.file.Path
  ): Map[AppMode, List[java.nio.file.Path]] =
    val existing = current.getOrElse(mode, Nil)
    current.updated(mode, (path :: existing.filterNot(_ == path)).take(20))
