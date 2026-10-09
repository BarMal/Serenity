package com.serenity.io

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import cats.effect.{IO, Resource}
import com.serenity.BuildInfo
import com.serenity.config.ConfigManager
import com.serenity.lsp.client.LspStderrLog
import com.serenity.session.SessionManager

/** The About Serenity document: what is running and where it keeps its files, then the privacy statement, the licence
  * and the third-party notices, as one read-only file the editor can open.
  */
object AboutDocument:

  final case class Info(
      version: String,
      commit: String,
      commitTime: String,
      channel: String,
      java: String,
      os: String,
      configFile: Path,
      sessionDirectory: Path,
      logFile: Path,
      lspLogDirectory: Path
  )

  /** The statement `docs/PRIVACY.md` is bundled under; a build without it says so rather than staying silent. */
  val privacyResource = "/META-INF/serenity/PRIVACY.md"
  val documentName    = "Serenity-About.md"

  private val missingPrivacy =
    "# Privacy\n\nThe privacy statement is not bundled with this build.\n"

  def render(info: Info, licence: String, notices: String, privacy: Option[String]): String =
    List(
      "# About Serenity",
      "",
      s"Version: ${info.version}",
      s"Commit: ${info.commit} (${info.commitTime})",
      s"Channel: ${info.channel}",
      s"Java: ${info.java}",
      s"OS: ${info.os}",
      "",
      s"Config: ${info.configFile}",
      s"Sessions: ${info.sessionDirectory}",
      s"Log: ${info.logFile}",
      s"Language server logs: ${info.lspLogDirectory}",
      "",
      s"Releases: ${ReleasesPage.url}",
      "",
      privacy.getOrElse(missingPrivacy).trim,
      "",
      "# Licence",
      "",
      s"```text\n${licence.trim}\n```",
      "",
      notices
    ).mkString("\n")

  def currentInfo: Info =
    def property(key: String): String = Option(System.getProperty(key)).filter(_.nonEmpty).getOrElse("unknown")
    val serenityDirectory             = SessionManager.defaultSessionRoot()
    Info(
      version = BuildInfo.version,
      commit = BuildInfo.commit,
      commitTime = BuildInfo.commitTime,
      channel = BuildInfo.channel,
      java = s"${property("java.runtime.version")} ${property("java.vm.vendor")}",
      os = s"${property("os.name")} ${property("os.version")} (${property("os.arch")})",
      configFile = ConfigManager.defaultConfigPath,
      sessionDirectory = serenityDirectory,
      logFile = serenityDirectory.resolve("serenity.log"),
      lspLogDirectory = LspStderrLog.defaultDirectory
    )

  /** Rewritten on every call so an upgraded build never shows the previous build's text. */
  def writeReadOnly(directory: Path): IO[Path] =
    for
      licence <- LicenceNotices.licence
      notices <- LicenceNotices.notices
      privacy <- bundledPrivacy
      target = directory.resolve(documentName)
      _ <- IO.blocking(Files.createDirectories(directory))
      _ <- IO.blocking(target.toFile.setWritable(true)).void
      _ <- IO.blocking(Files.writeString(target, render(currentInfo, licence, notices, privacy)))
      _ <- IO.blocking(target.toFile.setReadOnly()).void
    yield target

  def open(loadFile: Path => IO[Unit]): IO[Unit] = writeReadOnly(LicenceNotices.defaultDirectory).flatMap(loadFile)

  private def bundledPrivacy: IO[Option[String]] =
    IO.blocking(Option(getClass.getResourceAsStream(privacyResource))).flatMap {
      case None => IO.pure(None)
      case Some(stream) =>
        Resource
          .fromAutoCloseable(IO.pure(stream))
          .use(open => IO.blocking(Some(String(open.readAllBytes(), UTF_8))))
    }
