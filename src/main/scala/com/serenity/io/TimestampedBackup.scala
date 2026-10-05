package com.serenity.io

import java.nio.file.{Files, Path}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/** Keeping a file or folder aside under a name that says when, instead of deleting or overwriting it. */
object TimestampedBackup:

  private val Stamp = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss")

  def suffix(at: LocalDateTime): String =
    s"backup-${Stamp.format(at)}"

  def siblingOf(path: Path, at: LocalDateTime): Path =
    path.resolveSibling(s"${path.getFileName}.${suffix(at)}")

  /** Moves `path` to `target`, failing with `FileAlreadyExistsException` rather than replacing what is there. */
  def moveAside(path: Path, target: Path): Path =
    Files.move(path, target)
