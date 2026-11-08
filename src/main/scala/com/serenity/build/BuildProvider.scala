package com.serenity.build

import java.nio.file.Path

import cats.effect.{IO, Resource}
import com.serenity.trust.TrustGrant
import fs2.Stream

/** One way of building a project: a table of conventional commands, a BSP server, a user's task file. Several may match
  * one root; [[BuildCoordinator]] chooses between them.
  */
trait BuildProvider:
  def id: BuildProviderId

  /** Reads files only. Never starts a process, so it is safe for an untrusted project. */
  def detect(root: Path): IO[List[Detection]]

  /** Starting a session may start the project's programs, so it takes the grant that permits it. */
  def open(candidate: BuildCandidate, grant: TrustGrant): Resource[IO, BuildSession]

trait BuildSession:
  def capabilities: ProviderCapabilities
  def targets: IO[List[BuildTargetRef]]

  /** Ends with exactly one [[TaskEvent.Finished]]. Interrupting the stream is how a task is cancelled; a session whose
    * capabilities say it cannot cancel just stops reporting.
    */
  def run(request: TaskRequest): Stream[IO, TaskEvent]

  /** Build definition files changed on disk; a provider with a server reloads it, a stateless one ignores this. */
  def buildFilesChanged(paths: List[Path]): IO[Unit]

  def debug(request: TaskRequest): Option[IO[DapEndpoint]]
