package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.IO

/** The config file to watch for outside edits, and what to run when it may have changed. `reload` decides for itself
  * whether the change is real, so it is safe to call as often as the watcher or a focus change suggests.
  */
final case class ConfigFileWatch(file: Path, reload: IO[Unit])
