package com.serenity.command

import com.serenity.session.SessionId

/** What picking a session in the session pickers runs. */
object SessionCommands:

  def openNamedSession(sessionId: SessionId): Command =
    Command.typed(
      "open-named-session",
      "Replace the current session with a saved one.",
      CommandIntent.Session(SessionIntent.OpenNamedSession(sessionId)),
      CommandCategory.File
    )

  def renameNamedSession(sessionId: SessionId, currentName: String): Command =
    Command.typed(
      "rename-named-session",
      "Give a saved session a new name.",
      CommandIntent.Session(SessionIntent.RenameNamedSession(sessionId, currentName)),
      CommandCategory.File
    )
