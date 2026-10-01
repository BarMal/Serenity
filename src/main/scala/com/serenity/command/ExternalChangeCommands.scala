package com.serenity.command

import com.serenity.state.models.BufferId

/** The two ways out of a file that changed on disk under unsaved edits (#1623), as commands a prompt can run. */
object ExternalChangeCommands:

  def reloadFromDisk(bufferId: BufferId): Command =
    Command.typed(
      "reload-from-disk",
      "Discard unsaved edits and reload the file from disk",
      CommandIntent.File(FileIntent.ReloadFromDisk(bufferId)),
      CommandCategory.File
    )

  def overwriteOnDisk(bufferId: BufferId): Command =
    Command.typed(
      "overwrite-on-disk",
      "Save over the file even though it changed on disk",
      CommandIntent.File(FileIntent.OverwriteOnDisk(bufferId)),
      CommandCategory.File
    )
