package com.serenity.io

import java.nio.file.{Files, Path}
import java.time.Instant

/** Keeping a file or folder aside under a name that says when, instead of deleting or overwriting it. The name follows
  * the session salvage's `<file>.<kind>-<epochMillis>`, with the kind `reset`.
  */
object TimestampedBackup:

  def suffix(at: Instant): String =
    s"reset-${at.toEpochMilli}"

  def siblingOf(path: Path, at: Instant): Path =
    path.resolveSibling(s"${path.getFileName}.${suffix(at)}")

  /** Moves `path` to `target`, failing with `FileAlreadyExistsException` rather than replacing what is there. */
  def moveAside(path: Path, target: Path): Path =
    Files.move(path, target)
