package com.serenity.command

/** The palette's way into safe mode, and what the startup offer runs when it is accepted. */
object SafeModeCommands:

  val restart: Command =
    Command.typed(
      "restart-safe-mode",
      "Quit, then start again with default settings and without the session, language servers or project tasks.",
      CommandIntent.Lifecycle(LifecycleIntent.RestartInSafeMode),
      CommandCategory.File,
      label = "Restart in Safe Mode"
    )
