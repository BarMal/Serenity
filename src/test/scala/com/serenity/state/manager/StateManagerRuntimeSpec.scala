package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.*
import cats.effect.std.Semaphore
import cats.effect.unsafe.implicits.global
import com.serenity.animation.AnimationState
import com.serenity.command.{Command, CommandCategory, CommandIntent, ProjectIntent}
import com.serenity.config.PreferredWindowSize
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.{AppState, BufferId}
import com.serenity.state.reducers.{AppEffect, LspQueueEffect}
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateManagerRuntimeSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  "StateManagerRuntime" should "collect manager dependencies behind one runtime boundary" in {
    val program = for
      stateRef                 <- Ref.of[IO, AppState](AppState.initial)
      undoRef                  <- Ref.of[IO, UndoState](UndoState())
      themeNamesRef            <- Ref.of[IO, List[String]](List("dark"))
      quitSignal               <- Deferred[IO, Unit]
      lspQueue                 <- LspEffectQueue.create
      projectTaskFiberRef      <- Ref.of[IO, Option[ManagedProjectTask]](None)
      projectTaskSemaphore     <- Semaphore[IO](1)
      mouseTargetCacheRef      <- Ref.of[IO, Option[MouseTargetCache]](None)
      documentAnalysisFiberRef <- Ref.of[IO, Option[Fiber[IO, Throwable, Unit]]](None)
      bufferAnimationsRef      <- Ref.of[IO, Map[BufferId, AnimationState]](Map.empty)
      logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerRuntimeSpec"))
      sessionRoot <- IO.blocking(Files.createTempDirectory("serenity-runtime-spec"))
      runtime = StateManagerRuntime.create(
        stateRef = stateRef,
        undoRef = undoRef,
        themeNamesRef = themeNamesRef,
        quitSignal = quitSignal,
        logger = logger,
        policy = SessionManager.SessionPolicy(),
        sessionRootOverride = Some(sessionRoot),
        themeManager = AppThemeManager.create,
        lspQueue = lspQueue,
        projectTaskFiberRef = projectTaskFiberRef,
        projectTaskSemaphore = projectTaskSemaphore,
        mouseTargetCacheRef = mouseTargetCacheRef,
        documentAnalysisFiberRef = documentAnalysisFiberRef,
        bufferAnimationsRef = bufferAnimationsRef,
        onFontConfigChanged = (_: FontConfig) => IO.unit,
        deviceTextScaleProvider = IO.pure(1.0),
        configPersistencePath = None,
        uiPresetStore = UiPresetStore.default,
        windowSizeProvider = IO.pure(Some(PreferredWindowSize(1000, 700))),
        onPreferredWindowSizeChanged = (_: PreferredWindowSize) => IO.unit,
        fileDialog = None
      )
    yield
      runtime.stateRef shouldBe stateRef
      runtime.undoRef shouldBe undoRef
      runtime.themeNamesRef shouldBe themeNamesRef
      runtime.quitSignal shouldBe quitSignal
      runtime.lspQueue shouldBe lspQueue
      runtime.projectTaskFiberRef shouldBe projectTaskFiberRef
      runtime.projectTaskSemaphore shouldBe projectTaskSemaphore
      runtime.mouseTargetCacheRef shouldBe mouseTargetCacheRef
      runtime.documentAnalysisFiberRef shouldBe documentAnalysisFiberRef
      runtime.bufferAnimationsRef shouldBe bufferAnimationsRef
      runtime.sessionManager.sessionExists.unsafeRunSync() shouldBe false
      runtime.fileManager should not be null
      runtime.fileDialog shouldBe None
      runtime.sessionPersistence should not be null

    program.unsafeRunSync()
  }

  it should "cancel active project tasks from the cancel command and force quit" in {
    val program = for
      stateRef                 <- Ref.of[IO, AppState](AppState.initial)
      undoRef                  <- Ref.of[IO, UndoState](UndoState())
      themeNamesRef            <- Ref.of[IO, List[String]](List("dark"))
      quitSignal               <- Deferred[IO, Unit]
      lspQueue                 <- LspEffectQueue.create
      projectTaskFiberRef      <- Ref.of[IO, Option[ManagedProjectTask]](None)
      projectTaskSemaphore     <- Semaphore[IO](1)
      mouseTargetCacheRef      <- Ref.of[IO, Option[MouseTargetCache]](None)
      documentAnalysisFiberRef <- Ref.of[IO, Option[Fiber[IO, Throwable, Unit]]](None)
      bufferAnimationsRef      <- Ref.of[IO, Map[BufferId, AnimationState]](Map.empty)
      analysisCancelled        <- Deferred[IO, Unit]
      analysisStarted          <- Deferred[IO, Unit]
      pendingAnalysis <- IO
        .defer(analysisStarted.complete(()).void >> IO.never[Unit])
        .onCancel(analysisCancelled.complete(()).void)
        .start
      logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerRuntimeSpec"))
      sessionRoot <- IO.blocking(Files.createTempDirectory("serenity-runtime-spec"))
      runtime = StateManagerRuntime.create(
        stateRef = stateRef,
        undoRef = undoRef,
        themeNamesRef = themeNamesRef,
        quitSignal = quitSignal,
        logger = logger,
        policy = SessionManager.SessionPolicy(),
        sessionRootOverride = Some(sessionRoot),
        themeManager = AppThemeManager.create,
        lspQueue = lspQueue,
        projectTaskFiberRef = projectTaskFiberRef,
        projectTaskSemaphore = projectTaskSemaphore,
        mouseTargetCacheRef = mouseTargetCacheRef,
        documentAnalysisFiberRef = documentAnalysisFiberRef,
        bufferAnimationsRef = bufferAnimationsRef,
        onFontConfigChanged = (_: FontConfig) => IO.unit,
        deviceTextScaleProvider = IO.pure(1.0),
        configPersistencePath = None,
        uiPresetStore = UiPresetStore.default,
        windowSizeProvider = IO.pure(Some(PreferredWindowSize(1000, 700))),
        onPreferredWindowSizeChanged = (_: PreferredWindowSize) => IO.unit,
        fileDialog = None
      )
      operations <- StateManagerOperationBoundary.create(
        stateRef,
        documentAnalysisFiberRef,
        logger
      )
      composition = new StateManagerComposition(
        runtime.stateRef,
        runtime.undoRef,
        runtime.themeNamesRef,
        runtime.quitSignal,
        runtime.logger,
        runtime.policy,
        runtime.themeManager,
        runtime.lspQueue,
        runtime.projectTaskFiberRef,
        runtime.projectTaskSemaphore,
        runtime.mouseTargetCacheRef,
        runtime.documentAnalysisFiberRef,
        runtime.bufferAnimationsRef,
        runtime.onFontConfigChanged,
        runtime.deviceTextScaleProvider,
        runtime.configPersistencePath,
        runtime.uiPresetStore,
        runtime.windowSizeProvider,
        runtime.fileDialog,
        runtime.markdownPreviewWindow,
        runtime.fileManager,
        runtime.sessionManager,
        runtime.sessionPersistence,
        operations
      )
      commandChildDestroyed <- Deferred[IO, Unit]
      commandTaskStarted    <- Deferred[IO, Unit]
      commandTaskFinished   <- Deferred[IO, Unit]
      commandTask <- IO
        .defer(commandTaskStarted.complete(()).void >> IO.never[Unit])
        .onCancel(commandChildDestroyed.complete(()).void)
        .start
      _ <- commandTaskStarted.get
      _ <- projectTaskFiberRef.set(Some(ManagedProjectTask(commandTaskFinished, commandTask)))
      _ <- composition.interpretCommand(
        Command.typed(
          "project-cancel",
          "Cancel the running project task.",
          CommandIntent.Project(ProjectIntent.CancelProjectTask),
          CommandCategory.Project
        ),
        AppState.initial
      )
      commandChildWasDestroyed <- commandChildDestroyed.tryGet
      projectTaskAfterCommand  <- projectTaskFiberRef.get
      shutdownChildDestroyed   <- Deferred[IO, Unit]
      shutdownTaskStarted      <- Deferred[IO, Unit]
      shutdownTaskFinished     <- Deferred[IO, Unit]
      shutdownTask <- IO
        .defer(shutdownTaskStarted.complete(()).void >> IO.never[Unit])
        .onCancel(shutdownChildDestroyed.complete(()).void)
        .start
      _                         <- shutdownTaskStarted.get
      _                         <- projectTaskFiberRef.set(Some(ManagedProjectTask(shutdownTaskFinished, shutdownTask)))
      _                         <- analysisStarted.get
      _                         <- documentAnalysisFiberRef.set(Some(pendingAnalysis))
      _                         <- composition.forceQuit()
      shutdownChildWasDestroyed <- shutdownChildDestroyed.tryGet
      projectTaskAfterShutdown  <- projectTaskFiberRef.get
      analysisWasCancelled      <- analysisCancelled.tryGet
      pendingAnalysisFiber      <- documentAnalysisFiberRef.get
      _                         <- projectTaskAfterCommand.fold(IO.unit)(_.fiber.cancel)
      _                         <- projectTaskAfterShutdown.fold(IO.unit)(_.fiber.cancel)
      _                         <- pendingAnalysisFiber.fold(IO.unit)(_.cancel)
    yield
      commandChildWasDestroyed shouldBe Some(())
      projectTaskAfterCommand shouldBe None
      shutdownChildWasDestroyed shouldBe Some(())
      projectTaskAfterShutdown shouldBe None
      analysisWasCancelled shouldBe Some(())
      pendingAnalysisFiber shouldBe None

    program.unsafeRunSync()
  }

  it should "retain a replacement task when an older finalizer clears after it starts" in {
    val program = for
      projectTaskFiberRef <- Ref.of[IO, Option[ManagedProjectTask]](None)
      olderFinished       <- Deferred[IO, Unit]
      replacementFinished <- Deferred[IO, Unit]
      releaseOlder        <- Deferred[IO, Unit]
      olderTask           <- IO.never[Unit].start
      replacementTask     <- IO.never[Unit].start
      replacement = ManagedProjectTask(replacementFinished, replacementTask)
      _              <- projectTaskFiberRef.set(Some(ManagedProjectTask(olderFinished, olderTask)))
      olderFinalizer <- (releaseOlder.get >> ProjectTaskOwnership.clear(projectTaskFiberRef, olderFinished)).start
      _              <- projectTaskFiberRef.set(Some(replacement))
      _              <- releaseOlder.complete(())
      _              <- olderFinalizer.joinWithNever
      active         <- projectTaskFiberRef.get
      _              <- olderTask.cancel
      _              <- replacementTask.cancel
    yield active shouldBe Some(replacement)

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
        (saved, savedPath) => calls.update(_ :+ s"saveAs:$saved:$savedPath"),
        closed => calls.update(_ :+ s"close:$closed")
      )
      _        <- facade.openFile(path)
      _        <- facade.saveBuffer(bufferId)
      _        <- facade.saveBufferAs(bufferId, path)
      _        <- facade.forceCloseBuffer(bufferId)
      observed <- calls.get
    yield observed shouldBe List(
      s"open:$path",
      s"save:$bufferId",
      s"saveAs:$bufferId:$path",
      s"close:$bufferId"
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
          animation = _ => IO.unit
        )
      )
      _       <- behavior.interpret(AppEffect.LspQueue(LspQueueEffect.Enqueue(effect)))
      effects <- observed.get
    yield effects shouldBe List(effect)

    program.unsafeRunSync()
  }
