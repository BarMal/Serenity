package com.serenity.trust

import java.nio.file.{Files, Path, Paths}
import java.time.Instant

import scala.util.Try

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.io.AtomicFileWriter
import com.typesafe.config.ConfigUtil
import pureconfig.{ConfigReader, ConfigSource}

/** `~/.serenity/trust.conf`. It is kept apart from `config.conf` so that a config shared between machines, as dotfiles
  * often are, cannot carry the decision to run a project's programs along with it.
  */
object ProjectTrustStore:

  val CurrentVersion: Int = 1

  def defaultPath: Path = Paths.get(System.getProperty("user.home"), ".serenity", "trust.conf")

  final private case class TrustedEntry(root: String, at: String) derives ConfigReader
  final private case class ApprovedEntry(root: String, fingerprint: String) derives ConfigReader
  final private case class TrustFile(version: Int, trusted: List[TrustedEntry], approved: List[ApprovedEntry])
      derives ConfigReader

  /** A missing file is no trust. A file that cannot be read is an error for the caller to report, never a reason to
    * trust anything.
    */
  def load(path: Path): IO[Either[String, ProjectTrust]] =
    IO.blocking(Files.exists(path)).flatMap { exists =>
      if !exists then IO.pure(Right(ProjectTrust.empty))
      else IO.blocking(Files.readString(path)).map(decode).handleError(error => Left(error.getMessage))
    }

  def save(path: Path, trust: ProjectTrust): IO[Unit] =
    IO.blocking(Option(path.getParent).foreach(Files.createDirectories(_))) >>
      AtomicFileWriter.writeString(path, encode(trust))

  private def decode(text: String): Either[String, ProjectTrust] =
    for
      file <- ConfigSource.string(text).load[TrustFile].leftMap(_.prettyPrint())
      _ <- Either.cond(
        file.version <= CurrentVersion,
        (),
        s"trust file is version ${file.version}, newer than this Serenity reads ($CurrentVersion)"
      )
      trusted <- file.trusted.traverse(entry =>
        Try(Instant.parse(entry.at)).toEither
          .leftMap(_ => s"trusted root ${entry.root} has an unreadable time '${entry.at}'")
          .map(at => key(entry.root) -> at)
      )
    yield ProjectTrust(
      trusted = trusted.toMap,
      approved = file.approved
        .groupMap(entry => key(entry.root))(entry => CommandFingerprint.fromStored(entry.fingerprint))
        .view
        .mapValues(_.toSet)
        .toMap
    )

  private def key(root: String): ProjectKey = ProjectKey.of(Paths.get(root))

  private def encode(trust: ProjectTrust): String =
    def quoted(value: String): String = ConfigUtil.quoteString(value)
    val trusted = trust.trusted.toList
      .sortBy((root, _) => root.path.toString)
      .map((root, at) => s"  { root = ${quoted(root.path.toString)}, at = ${quoted(at.toString)} }")
    val approved = trust.approved.toList
      .sortBy((root, _) => root.path.toString)
      .flatMap((root, fingerprints) =>
        fingerprints.toList
          .map(_.value)
          .sorted
          .map(fingerprint => s"  { root = ${quoted(root.path.toString)}, fingerprint = ${quoted(fingerprint)} }")
      )
    s"""# Projects whose programs Serenity may run, and commands approved from project files (#2526).
       |version = $CurrentVersion
       |trusted = [
       |${trusted.mkString("\n")}
       |]
       |approved = [
       |${approved.mkString("\n")}
       |]
       |""".stripMargin
