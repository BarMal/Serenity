package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.{IO, Resource}
import com.serenity.io.DirectoryTree

/** Where tests make scratch files and folders. Nothing a test creates may land loose in the system temp directory:
  * `Files.createTempDirectory` and `createTempFile` there are never removed, and one `sbt test` used to leave thousands
  * behind.
  *
  * `directory` and `file` suit tests written as plain statements: they land under one per-run root that the build
  * (`Tests.Cleanup`) or, outside sbt, a shutdown hook removes whatever the outcome. `scoped` is for a test that can
  * bracket its use: the folder is removed as soon as the test ends, on success, failure or cancellation.
  */
object TestTemp:

  private val RootProperty = "serenity.test.tempRoot"

  private lazy val root: Path =
    sys.props.get(RootProperty).map(Path.of(_)).filter(Files.isDirectory(_)).getOrElse(ownRoot())

  private def ownRoot(): Path =
    val created = Files.createTempDirectory("srt")
    Runtime.getRuntime.addShutdownHook(new Thread(() => DirectoryTree.deleteBlocking(created)))
    created

  def directory(prefix: String): Path =
    Files.createTempDirectory(root, prefix)

  def file(prefix: String, suffix: String): Path =
    Files.createTempFile(root, prefix, suffix)

  def scoped(prefix: String): Resource[IO, Path] =
    Resource.make(IO.blocking(directory(prefix)))(DirectoryTree.deleteRecursively)

  def within[A](prefix: String)(use: Path => A): A =
    val created = directory(prefix)
    try use(created)
    finally DirectoryTree.deleteBlocking(created)
