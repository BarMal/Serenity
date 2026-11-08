package com.serenity.project

import java.nio.file.Path

import cats.effect.std.Queue
import cats.effect.{IO, Resource}
import com.serenity.build.{
  BuildCandidate,
  BuildProvider,
  BuildProviderId,
  BuildSession,
  BuildTargetRef,
  DapEndpoint,
  Detection,
  OutputChannel,
  ProviderCapabilities,
  TaskEvent,
  TaskKind,
  TaskOutcome,
  TaskRequest
}
import com.serenity.trust.{CommandSource, TrustGrant}
import fs2.Stream

/** Today's conventional-command table ([[ProjectTaskDetector]]) and runner ([[ProjectTaskRunner]]) as a
  * [[BuildProvider]], unchanged: the floor every other provider falls back to.
  */
final class CommandTableProvider private (
    runTask: (ProjectTaskCommand, String => IO[Unit]) => IO[ProjectTaskResult]
) extends BuildProvider:

  override val id: BuildProviderId = CommandTableProvider.Id

  override def detect(root: Path): IO[List[Detection]] =
    IO.blocking(ProjectTaskDetector.detect(root, ProjectTaskKind.Build))
      .map(_.toList.map { command =>
        Detection.Found(
          BuildCandidate(
            provider = id,
            root = command.workingDirectory,
            label = command.ecosystemLabel,
            command = List(command.executable),
            source = CommandSource.BuiltIn
          )
        )
      })

  override def open(candidate: BuildCandidate, grant: TrustGrant): Resource[IO, BuildSession] =
    Resource.pure(CommandTableSession(candidate.root, runTask))

object CommandTableProvider:

  val Id: BuildProviderId = BuildProviderId("command-table")

  def apply(): CommandTableProvider =
    new CommandTableProvider((command, onOutput) => ProjectTaskRunner.runStreaming(command)(onOutput))

  private[project] def apply(
    runTask: (ProjectTaskCommand, String => IO[Unit]) => IO[ProjectTaskResult]
  ): CommandTableProvider =
    new CommandTableProvider(runTask)

  private[project] def projectKind(kind: TaskKind): ProjectTaskKind =
    kind match
      case TaskKind.Build        => ProjectTaskKind.Build
      case TaskKind.Test         => ProjectTaskKind.Test
      case TaskKind.Run          => ProjectTaskKind.Run
      case TaskKind.Debug        => ProjectTaskKind.Debug
      case TaskKind.Dependencies => ProjectTaskKind.Dependencies

final private class CommandTableSession(
    root: Path,
    runTask: (ProjectTaskCommand, String => IO[Unit]) => IO[ProjectTaskResult]
) extends BuildSession:

  override val capabilities: ProviderCapabilities =
    ProviderCapabilities(TaskKind.values.toSet, canCancel = true, listsTargets = false, debug = false)

  override val targets: IO[List[BuildTargetRef]] = IO.pure(Nil)

  override def run(request: TaskRequest): Stream[IO, TaskEvent] =
    Stream.eval(IO.blocking(ProjectTaskDetector.detect(root, CommandTableProvider.projectKind(request.kind)))).flatMap {
      case None =>
        Stream.emit(TaskEvent.Finished(TaskOutcome.Unavailable(s"No ${request.kind} task found in $root.")))
      case Some(command) =>
        Stream.emit(TaskEvent.Started(command.renderedCommand, command.renderedCommand)) ++ running(command)
    }

  /** Output arrives through a callback, so it goes through a queue; the stream's end cancels the run, which stops the
    * process as cancelling [[ProjectTaskRunner.runStreaming]] always has.
    */
  private def running(command: ProjectTaskCommand): Stream[IO, TaskEvent] =
    Stream.eval(Queue.unbounded[IO, Option[TaskEvent]]).flatMap { events =>
      val work =
        runTask(command, chunk => events.offer(Some(TaskEvent.Output(OutputChannel.Stdout, chunk)))).attempt
          .flatMap(result => events.offer(Some(TaskEvent.Finished(outcome(command, result)))) >> events.offer(None))
      Stream.fromQueueNoneTerminated(events).concurrently(Stream.eval(work))
    }

  private def outcome(command: ProjectTaskCommand, result: Either[Throwable, ProjectTaskResult]): TaskOutcome =
    result match
      case Right(done) if done.exitCode == 0 => TaskOutcome.Succeeded
      case Right(done) => TaskOutcome.Failed(s"${command.renderedCommand} exited with code ${done.exitCode}")
      case Left(error) => TaskOutcome.Failed(s"${command.renderedCommand} could not start: ${error.getMessage}")

  override def buildFilesChanged(paths: List[Path]): IO[Unit] = IO.unit

  override def debug(request: TaskRequest): Option[IO[DapEndpoint]] = None
