package com.serenity.trust

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.security.MessageDigest
import java.time.Instant
import java.util.HexFormat

import cats.effect.IO

/** A project root as trust sees it: absolute and normalised, so `a/b/..` cannot stand for a different root than `a`. */
opaque type ProjectKey = Path

object ProjectKey:

  def of(path: Path): ProjectKey = path.toAbsolutePath.normalize()

  /** The real path, so a symlink to a root is the same root; falls back to [[of]] for a path that does not exist. */
  def resolve(path: Path): IO[ProjectKey] =
    IO.blocking(path.toRealPath()).attempt.map(_.fold(_ => of(path), of))

  extension (key: ProjectKey)
    def path: Path                         = key
    def covers(other: ProjectKey): Boolean = other.startsWith(key)

/** Where a command a provider wants to run was written down. Only a project-sourced command can have been chosen by
  * whoever wrote the repository, so only it needs its own approval on top of the project's trust.
  */
enum CommandSource:
  case BuiltIn
  case UserLayer
  case Project(file: Path, sha256: String)

opaque type CommandFingerprint = String

object CommandFingerprint:

  /** Covers the file's content and the exact argv, each argument length-prefixed so that `["a b", "c"]` and
    * `["a", "b c"]` differ.
    */
  def of(source: CommandSource.Project, argv: List[String]): CommandFingerprint =
    val framed = (source.sha256 :: argv).map(part => s"${part.length}:$part").mkString
    HexFormat.of.formatHex(MessageDigest.getInstance("SHA-256").digest(framed.getBytes(StandardCharsets.UTF_8)))

  def fromStored(value: String): CommandFingerprint = value

  extension (fingerprint: CommandFingerprint) def value: String = fingerprint

/** Proof that `command` may be started for `root`. Only [[ProjectTrust.grant]] makes one, so code that spawns a
  * project's processes cannot be written without asking.
  */
opaque type TrustGrant = (ProjectKey, List[String])

object TrustGrant:

  private[trust] def apply(root: ProjectKey, command: List[String]): TrustGrant = (root, command)

  extension (grant: TrustGrant)
    def root: ProjectKey       = grant._1
    def command: List[String] = grant._2

enum TrustRefusal:
  case ProjectUntrusted(root: ProjectKey)
  case CommandNotApproved(root: ProjectKey, command: List[String], fingerprint: CommandFingerprint)

/** The one trust decision every feature that runs a project's programs consults (#2526 Q7): language servers, builds,
  * tasks, BSP, git refresh, terminals. An untrusted root gets nothing, built-in commands included (Q4), because
  * starting even a stock language server imports the project's build and so runs its code.
  */
final case class ProjectTrust(
    trusted: Map[ProjectKey, Instant],
    approved: Map[ProjectKey, Set[CommandFingerprint]]
):

  def isTrusted(root: ProjectKey): Boolean =
    trusted.keys.exists(_.covers(root))

  def grant(root: ProjectKey, source: CommandSource, command: List[String]): Either[TrustRefusal, TrustGrant] =
    if !isTrusted(root) then Left(TrustRefusal.ProjectUntrusted(root))
    else
      source match
        case CommandSource.BuiltIn | CommandSource.UserLayer => Right(TrustGrant(root, command))
        case project: CommandSource.Project =>
          val fingerprint = CommandFingerprint.of(project, command)
          if approved.getOrElse(root, Set.empty).contains(fingerprint) then Right(TrustGrant(root, command))
          else Left(TrustRefusal.CommandNotApproved(root, command, fingerprint))

  def trust(root: ProjectKey, at: Instant): ProjectTrust =
    copy(trusted = trusted.updated(root, at))

  def approve(root: ProjectKey, fingerprint: CommandFingerprint): ProjectTrust =
    copy(approved = approved.updated(root, approved.getOrElse(root, Set.empty) + fingerprint))

  /** Forgets the root's trust and every command approved under it. */
  def revoke(root: ProjectKey): ProjectTrust =
    ProjectTrust(trusted - root, approved - root)

object ProjectTrust:

  val empty: ProjectTrust = ProjectTrust(Map.empty, Map.empty)
