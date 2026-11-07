package com.serenity.process

import java.nio.charset.StandardCharsets

import scala.concurrent.duration.*

import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import org.typelevel.log4cats.Logger

/** Owns every external process the editor starts: how it is launched, how it is stopped (tree and all, within a bound),
  * and the live list shown to the user.
  */
trait ProcessService:
  def spawn(spec: ProcessSpec): Resource[IO, ManagedProcess]
  def run(spec: ProcessSpec, timeout: FiniteDuration): IO[Option[ProcessResult]]
  def launchDetached(spec: ProcessSpec): IO[Unit]
  def running: IO[List[ProcessInfo]]
  def stopAll: IO[Unit]

object ProcessService:

  private val PollInterval = 25.millis

  final private case class Entry(info: ProcessInfo, process: RunningProcess, policy: StopPolicy)

  def create(launcher: ProcessLauncher)(using logger: Logger[IO]): IO[ProcessService] =
    Ref.of[IO, Map[Long, Entry]](Map.empty).map(new Live(launcher, _))

  final private class Live(launcher: ProcessLauncher, registry: Ref[IO, Map[Long, Entry]])(using logger: Logger[IO])
      extends ProcessService:

    def spawn(spec: ProcessSpec): Resource[IO, ManagedProcess] =
      owned(spec).map { entry =>
        val process = entry.process
        ManagedProcess(process.pid, process.stdin, process.stdout, process.stderr, process.exitCode)
      }

    def run(spec: ProcessSpec, timeout: FiniteDuration): IO[Option[ProcessResult]] =
      owned(spec)
        .use { entry =>
          val process = entry.process
          val gather =
            for
              _      <- process.discardStderr.start
              reader <- process.readStdout.start
              code   <- process.exitCode
              out    <- reader.joinWithNever
            yield ProcessResult(code, new String(out, StandardCharsets.UTF_8))
          gather.map(Option(_)).timeoutTo(timeout, IO.none)
        }
        .handleErrorWith { error =>
          logger.warn(s"${spec.owner.name}: could not run ${spec.command.mkString(" ")}: ${error.getMessage}").as(None)
        }

    private def owned(spec: ProcessSpec): Resource[IO, Entry] =
      Resource.make(begin(spec, detached = false))(entry => stop(entry) *> forget(entry.info.pid))

    def launchDetached(spec: ProcessSpec): IO[Unit] =
      for
        entry <- begin(spec, detached = true)
        _     <- (entry.process.exitCode.attempt *> forget(entry.info.pid)).start
      yield ()

    def running: IO[List[ProcessInfo]] = registry.get.map(_.values.toList.map(_.info).sortBy(_.pid))

    def stopAll: IO[Unit] =
      registry.get.flatMap(_.values.filterNot(_.info.detached).toList.parTraverse_(stop))

    private def begin(spec: ProcessSpec, detached: Boolean): IO[Entry] =
      for
        process <- launcher.start(spec)
        now     <- IO.realTimeInstant
        entry = Entry(ProcessInfo(spec.owner, process.pid, spec.command, now, detached), process, spec.stop)
        _ <- registry.update(_.updated(process.pid, entry))
      yield entry

    private def forget(pid: Long): IO[Unit] = registry.update(_ - pid)

    /** Asks the whole tree to exit, forces what stays, and gives up (loudly) after the policy's bounds. The tree is
      * read first: once the root dies its descendants are reparented away and can no longer be found from it.
      */
    private def stop(entry: Entry): IO[Unit] =
      val process = entry.process
      for
        tree <- process.descendants.handleError(_ => Nil)
        all = process :: tree
        _        <- all.traverse_(_.destroy.attempt)
        stubborn <- survivors(all, entry.policy.grace)
        _        <- stubborn.traverse_(_.destroyForcibly.attempt)
        stuck    <- survivors(stubborn, entry.policy.forceWait)
        _ <- stuck.traverse_ { control =>
          logger.warn(
            s"${entry.info.owner.name}: process ${control.pid} (of ${entry.info.command.mkString(" ")}) is still alive " +
              s"${(entry.policy.grace + entry.policy.forceWait).toSeconds}s after being told to stop"
          )
        }
      yield ()

    private def survivors(controls: List[ProcessControl], within: FiniteDuration): IO[List[ProcessControl]] =
      def alive(candidates: List[ProcessControl]): IO[List[ProcessControl]] =
        candidates.filterA(_.isAlive.handleError(_ => false))

      def poll(candidates: List[ProcessControl], remaining: FiniteDuration): IO[List[ProcessControl]] =
        alive(candidates).flatMap {
          case Nil                                => IO.pure(Nil)
          case left if remaining <= Duration.Zero => IO.pure(left)
          case left =>
            val step = remaining.min(PollInterval)
            IO.sleep(step) *> poll(left, remaining - step)
        }

      poll(controls, within)
