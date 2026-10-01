package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.IO
import com.serenity.io.FileUtils
import com.serenity.project.{ProjectPresence, ProjectTaskDetector}
import com.serenity.state.models.AppState

/** Where project detection starts: the focused buffer's file, or the working directory when it has none. */
private[manager] object ProjectTaskStart:

  def path(state: AppState): IO[Path] =
    state.focusedBufferId
      .flatMap(state.persisted.buffers.get)
      .flatMap(_.document.filePath)
      .fold(FileUtils.getCurrentDirectory)(IO.pure)

  // A failed check leaves project commands runnable: running one reports what went wrong in its own terminal panel.
  def presence(state: AppState): IO[ProjectPresence] =
    path(state)
      .flatMap(start => IO.blocking(ProjectTaskDetector.presence(start)))
      .handleError(_ => ProjectPresence.Unchecked)
