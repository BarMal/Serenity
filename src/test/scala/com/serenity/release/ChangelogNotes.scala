package com.serenity.release

import java.nio.file.{Files, Path}

import scala.util.Try

/** Prints the CHANGELOG body for a release, for the release workflow.
  *
  * `Test/runMain com.serenity.release.ChangelogNotes CHANGELOG.md 1.2.0` prints that version's section;
  * `... CHANGELOG.md --unreleased` prints the `[Unreleased]` section. Exits 1, with the reasons on stderr, when the
  * file is malformed or the section is missing or empty.
  */
object ChangelogNotes:

  def main(args: Array[String]): Unit =
    val result = args.toList match
      case file :: "--unreleased" :: Nil => read(file).flatMap(ChangelogSections.unreleased)
      case file :: version :: Nil        => read(file).flatMap(ChangelogSections.forVersion(_, version))
      case _                             => Left(List("usage: ChangelogNotes <CHANGELOG.md> <X.Y.Z | --unreleased>"))

    result match
      case Right(notes) => println(notes)
      case Left(errors) =>
        errors.foreach(Console.err.println)
        sys.exit(1)

  private def read(file: String): Either[List[String], String] =
    Try(Files.readString(Path.of(file))).toEither.left.map(error => List(s"cannot read $file: ${error.getMessage}"))
