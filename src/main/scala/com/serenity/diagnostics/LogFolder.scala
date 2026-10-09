package com.serenity.diagnostics

import java.nio.file.{Files, Path}

import cats.effect.IO

/** "Open Logs Folder": the platform's file manager, pointed at the log directory. */
object LogFolder:

  def openCommand(platform: Platform, directory: Path): List[String] =
    platform match
      case Platform.Windows => List("explorer.exe", directory.toString)
      case Platform.MacOs   => List("open", directory.toString)
      case Platform.Other   => List("xdg-open", directory.toString)

  /** Starts the file manager and returns without waiting: Explorer exits non-zero even when it opened the folder. */
  def open(directory: Path): IO[Unit] =
    IO.blocking {
      Files.createDirectories(directory)
      val _ = ProcessBuilder(openCommand(Platform.current, directory)*)
        .redirectErrorStream(true)
        .redirectOutput(ProcessBuilder.Redirect.DISCARD)
        .start()
    }
