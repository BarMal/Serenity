package com.serenity.app

import java.nio.file.{Files, Path}

import cats.effect.{IO, Resource}
import com.serenity.io.DirectoryTree

/** The document a renderer trial scrolls (#2618): long, and with every line different, so each page-down changes what
  * is on screen.
  */
object TrialDocument:

  private val words =
    Vector(
      "quick",
      "brown",
      "render",
      "window",
      "latency",
      "pixel",
      "frame",
      "scroll",
      "serenity",
      "lazy",
      "over",
      "fox"
    )

  private val Lines = 4000

  private def lineAt(index: Int): String =
    val tail = (0 until 10).map(offset => words((index * 7 + offset * 5 + offset * offset) % words.size))
    f"$index%04d ${tail.mkString(" ")}"

  /** The document on disk, in a folder of its own, for as long as the trial runs. */
  def resource: Resource[IO, Path] =
    Resource
      .make(IO.blocking(Files.createTempDirectory("serenity-renderer-trial")))(DirectoryTree.deleteRecursively)
      .evalMap(folder =>
        IO.blocking {
          val file = Files.createTempFile(folder, "trial", ".txt")
          Files.writeString(file, (0 until Lines).map(lineAt).mkString("\n") + "\n")
          file
        }
      )
