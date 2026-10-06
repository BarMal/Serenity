package com.serenity.release

import java.nio.file.{Files, Path}

import scala.util.Try

/** Writes the CHANGELOG body for a release to a file, for the release workflow to publish (`body_path`).
  *
  * `Test/runMain com.serenity.release.ChangelogNotes CHANGELOG.md 1.2.0 notes.md` writes that version's section;
  * `... CHANGELOG.md --unreleased notes.md` writes the `[Unreleased]` section. Exits 1, with the reasons on stderr,
  * when the file is malformed or the section is missing or empty.
  */
object ChangelogNotes:

  def main(args: Array[String]): Unit =
    write(args.toList).left.foreach { errors =>
      errors.foreach(Console.err.println)
      sys.exit(1)
    }

  def write(args: List[String]): Either[List[String], Path] =
    args match
      case file :: selector :: out :: Nil =>
        for
          content <- read(file)
          notes <-
            if selector == "--unreleased" then ChangelogSections.unreleased(content)
            else ChangelogSections.forVersion(content, selector)
          written <- Try(Files.writeString(Path.of(out), notes)).toEither.left
            .map(error => List(s"cannot write $out: ${error.getMessage}"))
        yield written
      case _ => Left(List("usage: ChangelogNotes <CHANGELOG.md> <X.Y.Z | --unreleased> <output file>"))

  private def read(file: String): Either[List[String], String] =
    Try(Files.readString(Path.of(file))).toEither.left.map(error => List(s"cannot read $file: ${error.getMessage}"))
