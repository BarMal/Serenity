package com.serenity.state.models

/** Builds the mode/tab corner widget's "recent in this mode" list (issue #1307) from `Persisted.recentFilesByMode` for
  * whichever `AppMode` is currently active.
  */
object RecentFilesInModeContent:

  def build(state: AppState): SurfaceContent.RecentFilesInMode =
    val mode = state.persisted.config.appMode
    SurfaceContent.RecentFilesInMode(mode, state.persisted.recentFilesByMode.getOrElse(mode, Nil))
