package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.*
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.command.{Command, CommandCategory, CommandIntent, ProjectIntent, ViewIntent}
import com.serenity.config.PreferredWindowSize
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.project.{ProjectTaskCommand, ProjectTaskKind, ProjectTaskResult}
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.effects.{Lane, LaneKey, LanePolicy}
import com.serenity.state.models.{AppState, BufferId, SurfaceContent}
import com.serenity.state.reducers.{AppEffect, LspQueueEffect}
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.PanelPosition
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateManagerRuntimeSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val analysisLane: Lane.Keyed = Lane.Keyed(LaneKey.Analysis, LanePolicy.SwitchLatest)

  private def runtimeOver(modelRef: Ref[IO, Model]): IO[StateManagerRuntime] =
    for
      themeNamesRef       <- Ref.of[IO, List[String]](List("dark"))
      quitSignal          <- Deferred[IO, Unit]
      lspQueue            <- LspEffectQueue.create
      mouseTargetCacheRef <- Ref.of[IO, Option[MouseTargetCache]](None)
      sessionRoot         <- IO.blocking(Files.createTempDirectory("serenity-runtime-spec"))
    yield StateManagerRuntime.create(
      modelRef = modelRef,
      themeNamesRef = themeNamesRef,
      quitSignal = quitSignal,
      logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerRuntimeSpec")),
      policy = SessionManager.SessionPolicy(),
      sessionRootOverride = Some(sessionRoot),
      themeManager = AppThemeManager.create,
      lspQueue = lspQueue,
      mouseTargetCacheRef = mouseTargetCacheRef,
      onFontConfigChanged = (_: FontConfig) => IO.unit,
      deviceTextScaleProvider = IO.pure(1.0),
      configPersistencePath = None,
      uiPresetStore = UiPresetStore.default,
      windowSizeProvider = IO.pure(Some(PreferredWindowSize(1000, 700))),
      onPreferredWindowSizeChanged = (_: PreferredWindowSize) => IO.unit,
      fileDialog = None
    )

  private def compose(runtime: StateManagerRuntime, operations: StateManagerOperationBoundary) =
    new StateManagerComposition(
      runtime.modelRef,
      runtime.themeNamesRef,
      runtime.quitSignal,
      runtime.logger,
      runtime.policy,
      runtime.themeManager,
      runtime.lspQueue,
      runtime.mouseTargetCacheRef,
      runtime.onFontConfigChanged,
      runtime.deviceTextScaleProvider,
      runtime.configPersistencePath,
      runtime.uiPresetStore,
      runtime.windowSizeProvider,
      runtime.fileDialog,
      runtime.markdownPreviewWindow,
      runtime.runProjectTask,
      runtime.fileManager,
      runtime.sessionManager,
      runtime.sessionPersistence,
      operations
    )

  /** A task whose process never ends by itself: it signals once it runs, and records when it is destroyed. */
  final private case class EndlessTask(started: Deferred[IO, Unit], destroyed: Deferred[IO, Unit])

  private val endlessTask: IO[EndlessTask] = (Deferred[IO, Unit], Deferred[IO, Unit]).mapN(EndlessTask.apply)

  /** A composition whose focused buffer sits in a Makefile project, launching `tasks` in turn. */
  private def projectComposition(
    tasks: List[EndlessTask]
  ): IO[(StateManagerComposition, StateManagerOperationBoundary)] =
    for
      directory <- IO.blocking(Files.createTempDirectory("serenity-runtime-spec-project"))
      _         <- IO.blocking(Files.writeString(directory.resolve("Makefile"), "all:\n\ttrue\n"))
      initial = AppState.initial
      buffer  = initial.persisted.buffers(BufferId(0))
      focused = initial.copy(persisted =
        initial.persisted.copy(buffers =
          initial.persisted.buffers.updated(
            BufferId(0),
            buffer.copy(document = buffer.document.copy(filePath = Some(directory.resolve("main.c"))))
          )
        )
      )
      modelRef  <- Ref.of[IO, Model](Model(focused, UndoState(), Map.empty))
      remaining <- Ref.of[IO, List[EndlessTask]](tasks)
      runtime   <- runtimeOver(modelRef)
      launcher: ProjectTaskLauncher = (_, _) =>
        remaining
          .modify(queue => (queue.drop(1), queue.headOption))
          .flatMap(
            _.fold(IO.never[ProjectTaskResult])(task =>
              (task.started.complete(()) >> IO.never[ProjectTaskResult]).onCancel(task.destroyed.complete(()).void)
            )
          )
      operations <- StateManagerOperationBoundary.create(Model.appRef(modelRef), runtime.logger)
    yield (compose(runtime.copy(runProjectTask = launcher), operations), operations)

  private def projectCommand(intent: ProjectIntent): Command =
    Command.typed("project", "Project task command.", CommandIntent.Project(intent), CommandCategory.Project)

  private val runBuild = projectCommand(ProjectIntent.RunProjectTask(ProjectTaskKind.Build))

  private def sessionOver(current: AppState): AppState =
    val fresh  = AppState.initial
    val buffer = fresh.persisted.buffers(BufferId(0))
    val path   = current.persisted.buffers.get(BufferId(0)).flatMap(_.document.filePath)
    fresh.copy(persisted =
      fresh.persisted.copy(buffers =
        fresh.persisted.buffers.updated(BufferId(0), buffer.copy(document = buffer.document.copy(filePath = path)))
      )
    )

  "StateManagerRuntime" should "collect manager dependencies behind one runtime boundary" in {
    val program = for
      modelRef            <- Ref.of[IO, Model](Model(AppState.initial, UndoState(), Map.empty))
      themeNamesRef       <- Ref.of[IO, List[String]](List("dark"))
      quitSignal          <- Deferred[IO, Unit]
      lspQueue            <- LspEffectQueue.create
      mouseTargetCacheRef <- Ref.of[IO, Option[MouseTargetCache]](None)
      logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerRuntimeSpec"))
      sessionRoot <- IO.blocking(Files.createTempDirectory("serenity-runtime-spec"))
      runtime = StateManagerRuntime.create(
        modelRef = modelRef,
        themeNamesRef = themeNamesRef,
        quitSignal = quitSignal,
        logger = logger,
        policy = SessionManager.SessionPolicy(),
        sessionRootOverride = Some(sessionRoot),
        themeManager = AppThemeManager.create,
        lspQueue = lspQueue,
        mouseTargetCacheRef = mouseTargetCacheRef,
        onFontConfigChanged = (_: FontConfig) => IO.unit,
        deviceTextScaleProvider = IO.pure(1.0),
        configPersistencePath = None,
        uiPresetStore = UiPresetStore.default,
        windowSizeProvider = IO.pure(Some(PreferredWindowSize(1000, 700))),
        onPreferredWindowSizeChanged = (_: PreferredWindowSize) => IO.unit,
        fileDialog = None
      )
    yield
      runtime.modelRef shouldBe modelRef
      runtime.themeNamesRef shouldBe themeNamesRef
      runtime.quitSignal shouldBe quitSignal
      runtime.lspQueue shouldBe lspQueue
      runtime.mouseTargetCacheRef shouldBe mouseTargetCacheRef
      runtime.sessionManager.sessionExists.unsafeRunSync() shouldBe false
      runtime.fileManager should not be null
      runtime.fileDialog shouldBe None
      runtime.sessionPersistence should not be null

    program.unsafeRunSync()
  }

  it should "cancel active project tasks from the cancel command and force quit" in {
    val program = for
      commandTask               <- endlessTask
      shutdownTask              <- endlessTask
      (composition, operations) <- projectComposition(List(commandTask, shutdownTask))
      analysisCancelled         <- Deferred[IO, Unit]
      analysisStarted           <- Deferred[IO, Unit]
      _                         <- composition.interpretCommand(runBuild, AppState.initial)
      _                         <- commandTask.started.get
      _ <- composition.interpretCommand(projectCommand(ProjectIntent.CancelProjectTask), AppState.initial)
      _ <- commandTask.destroyed.get
      projectTaskAfterCommand <- composition.stateRef.get.map(_.runtime.projectTasks.running)
      _                       <- composition.interpretCommand(runBuild, AppState.initial)
      _                       <- shutdownTask.started.get
      _ <- operations.effectLanes.submit(
        analysisLane,
        (analysisStarted.complete(()) >> IO.never[Unit]).onCancel(analysisCancelled.complete(()).void)
      )
      _                         <- analysisStarted.get
      _                         <- composition.runtimeLifecycle.forceQuit
      shutdownChildWasDestroyed <- shutdownTask.destroyed.tryGet
      analysisWasCancelled      <- analysisCancelled.tryGet
    yield
      projectTaskAfterCommand shouldBe None
      shutdownChildWasDestroyed shouldBe Some(())
      analysisWasCancelled shouldBe Some(())

    program.unsafeRunSync()
  }

  it should "cancel a running project task when its output panel is closed" in {
    val program = for
      task             <- endlessTask
      (composition, _) <- projectComposition(List(task))
      _                <- composition.interpretCommand(runBuild, AppState.initial)
      _                <- task.started.get
      stateWithPanel   <- composition.stateRef.get
      _ <- composition.interpretCommand(
        Command.typed(
          "unpin-bottom-panel",
          "Unpin the bottom panel.",
          CommandIntent.View(ViewIntent.UnpinPanel(PanelPosition.Bottom)),
          CommandCategory.View
        ),
        stateWithPanel
      )
      _               <- task.destroyed.get
      stateAfterUnpin <- composition.stateRef.get
    yield
      stateAfterUnpin.runtime.projectTasks.running shouldBe None
      stateAfterUnpin.pinnedSurfaces.exists { surface =>
        surface.content match
          case SurfaceContent.Terminal(_, _) =>
            stateAfterUnpin.persisted.layout.workspaceTree
              .flatMap(_.positionForSurface(surface.id))
              .contains(
                PanelPosition.Bottom
              )
          case _ => false
      } shouldBe false

    program.unsafeRunSync()
  }

  it should "retain a replacement task when an older task's finish lands after it starts" in {
    val command = ProjectTaskCommand(ProjectTaskKind.Build, "make", Path.of("/tmp"), "make", Nil)
    val older   = ProjectTaskTransitions.claimed(AppState.initial, 0L, command)
    val replacement =
      ProjectTaskTransitions.claimed(ProjectTaskTransitions.released(older), 1L, command)

    val afterOlderFinish =
      EffectResult.applyIfCurrent(
        replacement,
        EffectResult.ProjectTaskFinished(0L, Right(ProjectTaskResult(command, 0, "")))
      )

    afterOlderFinish.runtime.projectTasks.running.map(_.id) shouldBe Some(1L)
  }

  it should "never reuse a task id across a session restore, so an older task's late output cannot reach a newer one" in {
    val program = for
      taskA                     <- endlessTask
      taskB                     <- endlessTask
      (composition, operations) <- projectComposition(List(taskA, taskB))
      _                         <- composition.interpretCommand(runBuild, AppState.initial)
      _                         <- taskA.started.get
      idA <- composition.stateRef.get.flatMap(state =>
        IO.fromOption(state.runtime.projectTasks.running.map(_.id))(new IllegalStateException("task A did not start"))
      )
      beforeRestore <- composition.stateRef.get
      // A loaded session is a fresh state -- its runtime never persisted -- over the same project.
      loadedSession = sessionOver(beforeRestore)
      _ <- composition.validateAndUpdateState(
        composition.restoreSessionIntoCurrentViewport(loadedSession, beforeRestore),
        beforeRestore
      )
      restored <- composition.stateRef.get
      _ <- IO.raiseWhen(restored.runtime.projectTasks.running.isDefined)(
        new IllegalStateException("the session restore was not committed")
      )
      _ <- composition.interpretCommand(runBuild, AppState.initial)
      _ <- taskB.started.get.timeout(5.seconds)
      _ <- operations.dispatch(operations.applyResult(EffectResult.ProjectTaskOutput(idA, "late from A"), _ => IO.unit))
      afterLate <- composition.stateRef.get
      _         <- composition.interpretCommand(projectCommand(ProjectIntent.CancelProjectTask), AppState.initial)
    yield
      afterLate.runtime.projectTasks.running.map(_.id) should not be Some(idA)
      afterLate.runtime.projectTasks.running.map(_.output) shouldBe Some("")
      afterLate.pinnedSurfaces.map(_.content).collect { case SurfaceContent.Terminal(text, _) => text }.mkString should
        not include "late from A"

    program.unsafeRunSync()
  }

  "StateManagerFileFacade" should "be testable with injected file operations only" in {
    val bufferId = BufferId(7)
    val path     = Path.of("isolated.txt")

    val program = for
      stateRef <- Ref.of[IO, AppState](AppState.initial)
      calls    <- Ref.of[IO, List[String]](Nil)
      facade = new StateManagerFileFacade(
        stateRef,
        opened => calls.update(_ :+ s"open:$opened"),
        saved => calls.update(_ :+ s"save:$saved"),
        (saved, savedPath) => calls.update(_ :+ s"saveAs:$saved:$savedPath")
      )
      _        <- facade.openFile(path)
      _        <- facade.saveBuffer(bufferId)
      _        <- facade.saveBufferAs(bufferId, path)
      observed <- calls.get
    yield observed shouldBe List(
      s"open:$path",
      s"save:$bufferId",
      s"saveAs:$bufferId:$path"
    )

    program.unsafeRunSync()
  }

  "CommandEffectInterpreter" should "dispatch effects without runtime infrastructure" in {
    val effect = LspEffect.FileClosed("file:///isolated.txt", LanguageId.Scala)
    val program = for
      observed <- Ref.of[IO, List[LspEffect]](Nil)
      behavior = new CommandEffectInterpreter(
        CommandEffectInterpreter.Dependencies(
          lifecycle = IO.unit,
          command = _ => IO.unit,
          theme = _ => IO.unit,
          surface = _ => IO.unit,
          file = _ => IO.unit,
          explorer = _ => IO.unit,
          workflow = _ => IO.unit,
          lspQueue =
            case LspQueueEffect.Enqueue(value) => observed.update(_ :+ value)
            case LspQueueEffect.DocumentChanged(uri, languageId, text) =>
              observed.update(_ :+ LspEffect.FileChanged(uri, languageId, text, version = 0)),
          animation = _ => IO.unit,
          scheduleCommandRunnerBindingExpiry = _ => IO.unit
        )
      )
      _       <- behavior.interpret(AppEffect.LspQueue(LspQueueEffect.Enqueue(effect)))
      effects <- observed.get
    yield effects shouldBe List(effect)

    program.unsafeRunSync()
  }
