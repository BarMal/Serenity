package com.serenity.state.models

/** How the editor starts again when a restart is asked for in the running session. */
enum RestartMode:
  case InSafeMode
  case Normally
