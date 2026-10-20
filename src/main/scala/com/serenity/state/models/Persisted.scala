package com.serenity.state.models

import com.serenity.command.CommandId
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
    recentFilesByMode: Map[AppMode, List[java.nio.file.Path]] = Map.empty,
    recentFolders: List[java.nio.file.Path] = Nil,
    // The command runner's recents (issue #1048), keyed by `Command.name` (as a `CommandId`, issue #1693) and valued
    // by an incrementing recency generation. Saved per session (#1719): commands used in one session should not lead
    // the palette in another.
    commandUsage: Map[CommandId, Int] = Map.empty
)

object Persisted:

  val RecentLimit: Int = 20

  /** Records `folder` at the front of the recent folders by its absolute, normalised path, so a folder reached by two
    * spellings is one entry, capped at [[RecentLimit]] as the recent files are.
    */
  def trackRecentFolder(current: List[java.nio.file.Path], folder: java.nio.file.Path): List[java.nio.file.Path] =
    val normalised = folder.toAbsolutePath.normalize
    (normalised :: current.filterNot(_ == normalised)).take(RecentLimit)

  /** Records `path` under `mode`'s own recent list: moved to the front if already present, capped at [[RecentLimit]],
    * other modes' lists untouched.
    */
  def trackRecentFile(
    current: Map[AppMode, List[java.nio.file.Path]],
    mode: AppMode,
    path: java.nio.file.Path
  ): Map[AppMode, List[java.nio.file.Path]] =
    val existing = current.getOrElse(mode, Nil)
    current.updated(mode, (path :: existing.filterNot(_ == path)).take(RecentLimit))
