package com.serenity.config

import java.nio.file.{FileAlreadyExistsException, Files, Path}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

import scala.util.control.NonFatal

private[config] object ConfigBackup:

  private val stamp = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmssSSS'Z'").withZone(ZoneOffset.UTC)

  /** Never replaces an earlier backup: a second bad launch or save must not destroy the only copy of what the user
    * wrote. `None` when the copy could not be made, in which case the caller must not go on to overwrite the file.
    */
  def apply(path: Path, at: Instant): Option[Path] =
    try
      val target = path.resolveSibling(s"${path.getFileName}.bak-${stamp.format(at)}")
      Files.copy(path, target)
      Some(target)
    catch
      case _: FileAlreadyExistsException => None
      case NonFatal(_)                   => None
