package com.serenity.io

import java.nio.file.{Files, LinkOption, Path}
import java.util.Comparator

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

import cats.effect.IO

object DirectoryTree:

  private val Passes = 3

  /** Removes `root` and everything under it, and never fails: it runs as a `Resource` release or a shutdown hook, where
    * an error would only hide the one that mattered. Symbolic links are removed, not followed.
    */
  def deleteRecursively(root: Path): IO[Unit] =
    IO.blocking(deleteBlocking(root))

  def deleteBlocking(root: Path): Unit =
    removeInPasses(root, Passes)

  // A writer that is still winding down can put a file back between the walk and the directory delete, so a leftover
  // is retried rather than left behind.
  @tailrec
  private def removeInPasses(root: Path, passesLeft: Int): Unit =
    if passesLeft > 0 && Files.exists(root, LinkOption.NOFOLLOW_LINKS) then
      try
        val paths =
          Using.resource(Files.walk(root))(_.sorted(Comparator.reverseOrder[Path]()).iterator().asScala.toList)
        paths.foreach { path =>
          try Files.deleteIfExists(path): Unit
          catch case NonFatal(_) => ()
        }
      catch case NonFatal(_) => ()
      removeInPasses(root, passesLeft - 1)
