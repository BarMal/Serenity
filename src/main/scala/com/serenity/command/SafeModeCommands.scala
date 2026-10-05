package com.serenity.command

import com.serenity.state.models.RestartMode

/** The palette's way into and out of safe mode, and what the safe-mode prompt runs when accepted. */
object SafeModeCommands:

  val restart: Command =
    Command.typed(
      "restart-safe-mode",
      "Quit, then start again with default settings and without the session, language servers or project tasks.",
      CommandIntent.Lifecycle(LifecycleIntent.Restart(RestartMode.InSafeMode)),
      CommandCategory.File,
      label = "Restart in Safe Mode"
    )

  val restartNormally: Command =
    Command.typed(
      "restart-normally",
      "Quit, then start again with your settings and session.",
      CommandIntent.Lifecycle(LifecycleIntent.Restart(RestartMode.Normally)),
      CommandCategory.File,
      label = "Restart Normally"
    )
