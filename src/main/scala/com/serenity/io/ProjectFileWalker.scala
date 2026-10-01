package com.serenity.io

import java.io.{IOException, UncheckedIOException}
import java.nio.file.{Files, LinkOption, Path}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.util.Using

import cats.effect.IO

/** The files under a project root, relative to it and sorted by path. `truncated` when the walk stopped at its limit
  * with files left unlisted.
  */
final case class ProjectFileListing(files: Vector[Path], truncated: Boolean)

/** Lists a project's files for the "Go to File" finder. There is no `.gitignore` support anywhere in the app, so the
  * walk instead skips, by name and at any depth, hidden directories (`.git`, `.bloop`, `.metals`, `.bsp`, `.idea`, ...)
  * and the usual build and dependency output.
  */
object ProjectFileWalker:

  val SkippedDirectories: Set[String] = Set("target", "node_modules", "dist", "build")

  /** Breadth-first, so a walk cut short at `limit` keeps the files nearest the root. Only the root itself must be
    * readable: an unreadable directory below it is left out rather than failing the whole listing.
    */
  def list(root: Path, limit: Int): IO[ProjectFileListing] =
    IO.blocking {
      val (rootFiles, rootDirectories) = entries(root)
      val (files, truncated)           = walk(rootDirectories, rootFiles.take(limit + 1), limit)
      ProjectFileListing(files.take(limit).map(root.relativize).sortBy(_.toString), truncated)
    }

  @tailrec
  private def walk(pending: Vector[Path], found: Vector[Path], limit: Int): (Vector[Path], Boolean) =
    if found.size > limit then (found, true)
    else
      pending match
        case directory +: rest =>
          val (files, directories) =
            try entries(directory)
            catch case _: IOException | _: UncheckedIOException => (Vector.empty, Vector.empty)
          walk(rest ++ directories, found ++ files.take(limit + 1 - found.size), limit)
        case _ => (found, false)

  /** `directory`'s files and the subdirectories worth walking, each sorted by name. Symlinked directories are not
    * followed, so a link back up the tree cannot loop the walk.
    */
  private def entries(directory: Path): (Vector[Path], Vector[Path]) =
    val children              = Using.resource(Files.list(directory))(_.iterator.asScala.toVector.sortBy(_.toString))
    val (directories, others) = children.partition(Files.isDirectory(_, LinkOption.NOFOLLOW_LINKS))
    (others.filter(Files.isRegularFile(_)), directories.filter(walked))

  private def walked(directory: Path): Boolean =
    Option(directory.getFileName).map(_.toString).exists(name => !name.startsWith(".") && !SkippedDirectories(name))
