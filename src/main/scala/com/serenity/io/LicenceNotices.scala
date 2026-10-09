package com.serenity.io

import java.io.FileNotFoundException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import cats.effect.{IO, Resource}

/** The licence and third-party notices bundled into the jar by `build.sbt` (#2019). */
object LicenceNotices:

  val licenceResource = "/META-INF/serenity/LICENSE"
  val noticesResource = "/META-INF/serenity/THIRD-PARTY-NOTICES.md"
  val documentName    = "Serenity-Licence-and-Notices.md"

  def licence: IO[String] = resourceText(licenceResource)
  def notices: IO[String] = resourceText(noticesResource)

  def defaultDirectory: Path = Path.of(System.getProperty("java.io.tmpdir"), "serenity-licences")

  /** Writes the licence followed by the notices as one file the editor can open, and marks it read-only. Rewritten on
    * every call so an upgraded build never shows the previous build's text.
    */
  def writeReadOnly(directory: Path): IO[Path] =
    for
      licenceText <- licence
      noticesText <- notices
      target = directory.resolve(documentName)
      _ <- IO.blocking(Files.createDirectories(directory))
      _ <- IO.blocking(target.toFile.setWritable(true)).void
      _ <- IO.blocking(Files.writeString(target, s"```text\n${licenceText.trim}\n```\n\n$noticesText"))
      _ <- IO.blocking(target.toFile.setReadOnly()).void
    yield target

  def open(loadFile: Path => IO[Unit]): IO[Unit] = writeReadOnly(defaultDirectory).flatMap(loadFile)

  private def resourceText(path: String): IO[String] =
    Resource
      .fromAutoCloseable(
        IO.blocking(Option(getClass.getResourceAsStream(path))).flatMap { stream =>
          IO.fromOption(stream)(FileNotFoundException(s"Bundled resource missing from the build: $path"))
        }
      )
      .use(stream => IO.blocking(String(stream.readAllBytes(), UTF_8)))
