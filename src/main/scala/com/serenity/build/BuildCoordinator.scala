package com.serenity.build

import java.nio.file.Path

import cats.effect.std.Mutex
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.serenity.trust.{ProjectKey, ProjectTrust, TrustGrant, TrustRefusal}
import fs2.Stream
import fs2.concurrent.SignallingRef

/** The one place build work starts from. It asks every provider what it finds at a root, takes the first candidate the
  * project's trust allows and that opens, keeps that session for later requests, and lets a newer request for the same
  * root supersede an older one. Providers decide nothing of this themselves.
  */
final class BuildCoordinator private (
    providers: List[BuildProvider],
    trust: IO[ProjectTrust],
    sessions: Ref[IO, Map[BuildCoordinator.SessionKey, BuildCoordinator.OpenSession]],
    opening: Mutex[IO],
    latest: SignallingRef[IO, Map[ProjectKey, Long]]
):

  import BuildCoordinator.*

  def run(start: Path, kind: TaskKind): Stream[IO, TaskEvent] =
    Stream.eval(choose(start)).flatMap {
      case Left(reason) => Stream.emit(TaskEvent.Finished(TaskOutcome.Unavailable(reason)))
      case Right((root, session)) =>
        Stream.eval(nextVersion(root)).flatMap(version => supervised(root, version, session, kind))
    }

  private def choose(start: Path): IO[Either[String, (ProjectKey, BuildSession)]] =
    for
      detections <- providers.flatTraverse(provider => provider.detect(start).map(_.map(provider -> _)))
      found = detections.collect { case (provider, Detection.Found(candidate)) => provider -> candidate }
      current <- trust
      chosen  <- firstThatOpens(found, current, Nil)
    yield chosen

  private def firstThatOpens(
    remaining: List[(BuildProvider, BuildCandidate)],
    current: ProjectTrust,
    refused: List[String]
  ): IO[Either[String, (ProjectKey, BuildSession)]] =
    remaining match
      case Nil =>
        IO.pure(Left(refused.reverse.headOption.getOrElse("No build was found for this project.")))
      case (provider, candidate) :: rest =>
        ProjectKey.resolve(candidate.root).flatMap { root =>
          current.grant(root, candidate.source, candidate.command) match
            case Left(refusal) => firstThatOpens(rest, current, explain(refusal, candidate) :: refused)
            case Right(grant) =>
              sessionFor(provider, candidate, root, grant).attempt.flatMap {
                case Right(session) => IO.pure(Right(root -> session))
                case Left(error) =>
                  firstThatOpens(rest, current, s"${candidate.label} could not start: ${error.getMessage}" :: refused)
              }
        }

  private def sessionFor(
    provider: BuildProvider,
    candidate: BuildCandidate,
    root: ProjectKey,
    grant: TrustGrant
  ): IO[BuildSession] =
    val key = SessionKey(provider.id, root)
    opening.lock.surround {
      sessions.get.map(_.get(key)).flatMap {
        case Some(open) => IO.pure(open.session)
        case None =>
          provider.open(candidate, grant).allocated.flatMap { (session, release) =>
            sessions.update(_.updated(key, OpenSession(session, release))).as(session)
          }
      }
    }

  private def nextVersion(root: ProjectKey): IO[Long] =
    latest.modify { versions =>
      val version = versions.getOrElse(root, 0L) + 1
      (versions.updated(root, version), version)
    }

  /** The session's events, cut off once a newer request for the root arrives, and always ending in one `Finished`. */
  private def supervised(
    root: ProjectKey,
    version: Long,
    session: BuildSession,
    kind: TaskKind
  ): Stream[IO, TaskEvent] =
    val superseded = latest.discrete.map(_.get(root).exists(_ > version))
    Stream.eval(Ref.of[IO, Boolean](false)).flatMap { finished =>
      session
        .run(TaskRequest(kind, Nil, version))
        .handleErrorWith(error => Stream.emit(TaskEvent.Finished(TaskOutcome.Failed(error.getMessage))))
        .takeThrough(!isFinished(_))
        .evalTap(event => finished.set(true).whenA(isFinished(event)))
        .interruptWhen(superseded) ++
        Stream.eval((finished.get, superseded.head.compile.lastOrError).tupled).flatMap {
          case (true, _)     => Stream.empty
          case (false, true) => Stream.emit(TaskEvent.Finished(TaskOutcome.Cancelled))
          case (false, false) =>
            Stream.emit(TaskEvent.Finished(TaskOutcome.Failed("The build ended without a result.")))
        }
    }

object BuildCoordinator:

  final private[build] case class SessionKey(provider: BuildProviderId, root: ProjectKey)
  final private[build] case class OpenSession(session: BuildSession, release: IO[Unit])

  /** `trust` is read on every request, so a project trusted or revoked while Serenity runs takes effect at once. */
  def resource(providers: List[BuildProvider], trust: IO[ProjectTrust]): Resource[IO, BuildCoordinator] =
    Resource
      .make(
        (
          Ref.of[IO, Map[SessionKey, OpenSession]](Map.empty),
          Mutex[IO],
          SignallingRef.of[IO, Map[ProjectKey, Long]](Map.empty)
        )
          .mapN((sessions, opening, latest) =>
            (new BuildCoordinator(providers, trust, sessions, opening, latest), sessions)
          )
      )((_, sessions) => sessions.get.flatMap(_.values.toList.traverse_(_.release.attempt)))
      .map(_._1)

  private def isFinished(event: TaskEvent): Boolean =
    event match
      case _: TaskEvent.Finished => true
      case _                     => false

  private def explain(refusal: TrustRefusal, candidate: BuildCandidate): String =
    refusal match
      case TrustRefusal.ProjectUntrusted(_) =>
        s"This project is not trusted, so ${candidate.label} was not started."
      case TrustRefusal.CommandNotApproved(_, command, _) =>
        s"${candidate.label} wants to run `${command.mkString(" ")}`, which has not been approved for this project."
