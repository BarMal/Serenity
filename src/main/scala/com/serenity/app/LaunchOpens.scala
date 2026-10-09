package com.serenity.app

import java.nio.file.{Files, Path}

import cats.effect.IO

/** What a launch asked to open, sorted into the folder that becomes the Explorer root and the files to open.
  *
  * The Explorer has one root, so the first folder wins and any later one is set aside, and [[notice]] says so: failing
  * the whole launch would punish a file manager's multi-selection, and dropping a folder without a word would look like
  * a bug.
  */
final case class LaunchOpens(root: Option[Path], files: List[Path], ignoredFolders: List[Path]):

  def notice: Option[String] =
    root.filter(_ => ignoredFolders.nonEmpty).map { kept =>
      s"Only one folder can be the project root. Opened $kept; did not open ${ignoredFolders.mkString(", ")}."
    }

object LaunchOpens:

  def plan(paths: List[Path], isDirectory: Path => Boolean): LaunchOpens =
    val (folders, files) = paths.partition(isDirectory)
    LaunchOpens(folders.headOption, files, folders.drop(1))

  def resolve(paths: List[Path]): IO[LaunchOpens] =
    IO.blocking(plan(paths, path => Files.isDirectory(path)))
