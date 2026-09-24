package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import com.serenity.command.ProjectIntent
import com.serenity.keystroke.events.InsertChar
import com.serenity.project.{ProjectTaskCommand, ProjectTaskKind, ProjectTaskResult, ProjectTaskTerminal}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.effects.Lane
import com.serenity.state.models.*
import com.serenity.state.reducers.{EditorEventReducer, PinnedPanelContentReducer}
import com.serenity.state.undo.UndoState
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Project tasks run on the switch-latest `LaneKey.Project` lane (#1697 Wave 3): their output comes back through the
  * dispatcher in batches, as results applied only while their task is still the one the terminal panel shows, and
  * quitting cancels a running task instead of waiting for it.
  */
class ProjectTaskLanesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  /** A real operation boundary (dispatcher + lanes) under the project-task effects, recording each output batch. */
  final private class Rig(
      modelRef: Ref[IO, Model],
      val operations: StateManagerOperationBoundary,
      val outputBatches: Ref[IO, List[String]],
      val launches: Ref[IO, Int],
      lspQueue: LspEffectQueue,
      scripts: Ref[IO, List[(String => IO[Unit]) => IO[Int]]]
  ):
    val stateRef: Ref[IO, AppState] = Model.appRef(modelRef)
    private val modelCommit         = new ModelCommit(modelRef, operations)

    private def commit(transition: AppState => AppState): IO[Unit] =
      modelCommit.updateValidated(model => Some(model.copy(app = transition(model.app))))

    private val lanes = new EffectLanePort:
      def submitEffect(lane: Lane.Keyed, job: IO[Unit]): IO[Unit] = operations.submitEffect(lane, job)
      def dispatchEffectResult(result: EffectResult, onApplied: AppState => IO[Unit]): IO[Unit] =
        (result match
          case EffectResult.ProjectTaskOutput(_, chunk) => outputBatches.update(_ :+ chunk)
          case _                                        => IO.unit
        ) >> operations.dispatch(operations.applyResult(result, onApplied))

    private val launch: ProjectTaskLauncher = (command, onOutput) =>
      launches.update(_ + 1) >> scripts
        .modify {
          case next :: rest => (rest, next)
          case Nil          => (Nil, (_: String => IO[Unit]) => IO.never[Int])
        }
        .flatMap(script => script(onOutput))
        .map(exitCode => ProjectTaskResult(command, exitCode, "final output"))

    val effects = new StateManagerProjectLspEffects(
      lspQueue,
      stateRef.get,
      commit,
      lanes,
      launch,
      (text, position, size) => commit(PinnedPanelContentReducer.pinOrUpdateTerminal(text, position, size, _).state),
      (_, _) => IO.unit,
      _ => IO.unit
    )

    def run: IO[Unit] =
      stateRef.get.flatMap(effects.interpretProject(ProjectIntent.RunProjectTask(ProjectTaskKind.Build), _))

    def cancel: IO[Unit] =
      stateRef.get.flatMap(effects.interpretProject(ProjectIntent.CancelProjectTask, _))

    def terminalText: IO[Option[String]] =
      stateRef.get.map(
        _.pinnedSurfaces.collectFirst(surface =>
          surface.content match
            case SurfaceContent.Terminal(text, _) => text
        )
      )

    def typeOnDispatcher(char: Char): IO[Unit] =
      operations.dispatch(
        stateRef.get.flatMap(state =>
          operations.validateAndUpdateState(EditorEventReducer.reduce(InsertChar(char), PaneId(0), state).state, state)
        )
      )

  /** A focused buffer inside a directory holding a Makefile, so a build task is detected for it. */
  private def rig(scripts: List[(String => IO[Unit]) => IO[Int]]): IO[(Rig, ProjectTaskCommand)] =
    for
      directory <- IO.blocking(Files.createTempDirectory("project-task-lanes"))
      _         <- IO.blocking(Files.writeString(directory.resolve("Makefile"), "all:\n\ttrue\n"))
      state = focusedOn(directory.resolve("main.c"))
      modelRef   <- Ref.of[IO, Model](Model(state, UndoState(), Map.empty))
      operations <- StateManagerOperationBoundary.create(Model.appRef(modelRef), NoOpLogger.impl[IO])
      batches    <- Ref.of[IO, List[String]](Nil)
      launches   <- Ref.of[IO, Int](0)
      lspQueue   <- LspEffectQueue.create
      scriptRef  <- IO.ref(scripts)
    yield (
      Rig(modelRef, operations, batches, launches, lspQueue, scriptRef),
      ProjectTaskCommand(ProjectTaskKind.Build, "make", directory.toAbsolutePath.normalize(), "make", Nil)
    )

  private def focusedOn(path: Path): AppState =
    val initial = AppState.initial
    val buffer  = initial.persisted.buffers(BufferId(0))
    initial.copy(persisted =
      initial.persisted.copy(buffers =
        initial.persisted.buffers.updated(
          BufferId(0),
          buffer.copy(document = buffer.document.copy(content = Rope.empty, filePath = Some(path)))
        )
      )
    )

  private def bufferText(state: AppState): String =
    state.persisted.buffers(BufferId(0)).document.content.toString

  "A project task" should "stream its output into the terminal panel in order, batched per refresh" in {
    val script: (String => IO[Unit]) => IO[Int] = emit =>
      emit("a") >> IO.sleep(50.millis) >> emit("b") >> IO.sleep(200.millis) >> emit("c") >> IO.sleep(150.millis).as(0)
    val program =
      for
        (rig, command) <- rig(List(script))
        _              <- rig.run
        started        <- rig.terminalText
        _              <- IO.sleep(150.millis)
        firstBatch     <- rig.terminalText
        _              <- IO.sleep(200.millis)
        secondBatch    <- rig.terminalText
        _              <- IO.sleep(100.millis)
        finished       <- rig.terminalText
        batches        <- rig.outputBatches.get
        running        <- rig.stateRef.get.map(_.runtime.projectTasks.running)
      yield (command, started, firstBatch, secondBatch, finished, batches, running)

    val (command, started, firstBatch, secondBatch, finished, batches, running) = runVirtual(program)
    started shouldBe Some(ProjectTaskTerminal.started(command))
    firstBatch shouldBe Some(ProjectTaskTerminal.running(command, "ab"))
    secondBatch shouldBe Some(ProjectTaskTerminal.running(command, "abc"))
    finished shouldBe Some(ProjectTaskTerminal.completed(ProjectTaskResult(command, 0, "final output")))
    batches shouldBe List("ab", "c")
    running shouldBe None
  }

  it should "hand a flood of output to the dispatcher at most once per refresh, without holding up keystrokes" in {
    val flood: (String => IO[Unit]) => IO[Int] = emit => (emit("x") >> IO.sleep(1.milli)).replicateA_(1_000) >> IO.never
    val program =
      for
        (rig, _) <- rig(List(flood))
        _        <- rig.run
        keystrokeTimes <- ('a' to 'j').toList.traverse(char =>
          IO.sleep(90.millis) >> rig.typeOnDispatcher(char).timed.map(_._1)
        )
        _       <- IO.sleep(300.millis)
        typed   <- rig.stateRef.get.map(bufferText)
        batches <- rig.outputBatches.get
        _       <- rig.cancel
      yield (keystrokeTimes, typed, batches)

    val (keystrokeTimes, typed, batches) = runVirtual(program)
    keystrokeTimes.forall(_ == Duration.Zero) shouldBe true
    typed shouldBe "abcdefghij"
    batches.size should be <= 11
    batches.map(_.length).sum shouldBe 1_000
  }

  it should "drop output from a task that has since been cancelled or replaced" in {
    val program =
      for
        oldCancelled <- Deferred[IO, Unit]
        old: ((String => IO[Unit]) => IO[Int]) = emit =>
          (emit("old") >> IO.never[Int]).onCancel(oldCancelled.complete(()).void)
        replacement: ((String => IO[Unit]) => IO[Int]) = emit => emit("new") >> IO.never[Int]
        (rig, command) <- rig(List(old, replacement))
        _              <- rig.run
        _              <- IO.sleep(150.millis)
        showingOld     <- rig.terminalText
        oldId          <- rig.stateRef.get.map(_.runtime.projectTasks.running.map(_.id))
        _              <- rig.cancel
        _              <- oldCancelled.get
        _ <- oldId.traverse_(id =>
          rig.operations.dispatch(rig.operations.applyResult(EffectResult.ProjectTaskOutput(id, "late"), _ => IO.unit))
        )
        afterCancel <- rig.terminalText
        _           <- rig.run
        _           <- IO.sleep(150.millis)
        _ <- oldId.traverse_(id =>
          rig.operations.dispatch(
            rig.operations
              .applyResult(EffectResult.ProjectTaskFinished(id, Right(ProjectTaskResult(command, 1, ""))), _ => IO.unit)
          )
        )
        showingNew <- rig.terminalText
        _          <- rig.cancel
      yield (command, showingOld, afterCancel, showingNew)

    val (command, showingOld, afterCancel, showingNew) = runVirtual(program)
    showingOld shouldBe Some(ProjectTaskTerminal.running(command, "old"))
    afterCancel shouldBe Some("Project task cancelled.")
    showingNew shouldBe Some(ProjectTaskTerminal.running(command, "new"))
  }

  it should "refuse to start a second task while one is running" in {
    val program =
      for
        (rig, _) <- rig(Nil)
        _        <- rig.run
        _        <- IO.sleep(10.millis)
        _        <- rig.run
        refused  <- rig.terminalText
        _        <- IO.sleep(10.millis)
        launches <- rig.launches.get
        _        <- rig.cancel
      yield (refused, launches)

    runVirtual(program) shouldBe (
      Some("A project task is already running. Use Cancel Project Task before starting another."),
      1
    )
  }

  it should "report that nothing is running when cancelled with no task" in {
    val program =
      for
        (rig, _) <- rig(Nil)
        _        <- rig.cancel
        text     <- rig.terminalText
      yield text

    runVirtual(program) shouldBe Some("No project task is running.")
  }

  it should "be cancelled by quitting, which does not wait for it to finish" in {
    val program =
      for
        cancelled <- Deferred[IO, Unit]
        endless: ((String => IO[Unit]) => IO[Int]) = _ => IO.never[Int].onCancel(cancelled.complete(()).void)
        (rig, _)  <- rig(List(endless))
        _         <- rig.run
        _         <- IO.sleep(10.millis)
        quitTime  <- rig.operations.shutdownEffects().timed.map(_._1)
        destroyed <- cancelled.tryGet
      yield (quitTime, destroyed)

    val (quitTime, destroyed) = runVirtual(program)
    destroyed shouldBe Some(())
    quitTime should be < 1.second
  }

  "A project task result" should "leave the state untouched when its task is not the running one" in {
    val state = AppState.initial
    EffectResult.applyIfCurrent(state, EffectResult.ProjectTaskOutput(3L, "stale")) should be theSameInstanceAs state
  }
