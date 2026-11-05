package com.serenity.trust

import java.nio.file.Path

import cats.effect.std.Mutex
import cats.effect.{Clock, IO, Ref}

/** What features that start a project's programs need of trust: the current decisions, and the one change a notice can
  * make without a settings page, trusting a folder.
  */
trait ProjectTrustAccess:
  def current: IO[ProjectTrust]
  def trustFolder(root: ProjectKey): IO[Unit]

/** The running application's trust, read from and written through to [[ProjectTrustStore]]. */
final class ProjectTrustService private (state: Ref[IO, ProjectTrust], writes: Mutex[IO], file: Path)
    extends ProjectTrustAccess:

  override def current: IO[ProjectTrust] = state.get

  /** The decision holds at once; writing it is serialised so that two trusts in quick succession cannot reach the file
    * in the wrong order.
    */
  override def trustFolder(root: ProjectKey): IO[Unit] =
    writes.lock.surround(
      Clock[IO].realTimeInstant
        .flatMap(at => state.updateAndGet(_.trust(root, at)))
        .flatMap(ProjectTrustStore.save(file, _))
    )

object ProjectTrustService:

  /** `problem` is why the store could not be read. The service then starts with no trust, and trusting a folder
    * replaces the unreadable file.
    */
  final case class Loaded(service: ProjectTrustService, problem: Option[String])

  def load(file: Path): IO[Loaded] =
    for
      stored <- ProjectTrustStore.load(file)
      state  <- Ref.of[IO, ProjectTrust](stored.getOrElse(ProjectTrust.empty))
      writes <- Mutex[IO]
    yield Loaded(new ProjectTrustService(state, writes, file), stored.left.toOption)
