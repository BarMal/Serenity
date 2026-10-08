package com.serenity.io

import java.io.FileNotFoundException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import cats.effect.{IO, Resource}

/** `docs/PRIVACY.md`, bundled into the jar by `build.sbt` so the in-app statement and the doc are one text. */
object PrivacyStatement:

  val resource     = "/META-INF/serenity/PRIVACY.md"
  val documentName = "Serenity-Privacy-Statement.md"

  def text: IO[String] =
    Resource
      .fromAutoCloseable(
        IO.blocking(Option(getClass.getResourceAsStream(resource))).flatMap { stream =>
          IO.fromOption(stream)(FileNotFoundException(s"Bundled resource missing from the build: $resource"))
        }
      )
      .use(stream => IO.blocking(String(stream.readAllBytes(), UTF_8)))

  /** Rewritten on every call so an upgraded build never shows the previous build's text. */
  def writeReadOnly(directory: Path): IO[Path] =
    for
      statement <- text
      target = directory.resolve(documentName)
      _ <- IO.blocking(Files.createDirectories(directory))
      _ <- IO.blocking(target.toFile.setWritable(true)).void
      _ <- IO.blocking(Files.writeString(target, statement))
      _ <- IO.blocking(target.toFile.setReadOnly()).void
    yield target

  def open(loadFile: Path => IO[Unit]): IO[Unit] = writeReadOnly(LicenceNotices.defaultDirectory).flatMap(loadFile)
