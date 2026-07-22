package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.*
import cats.effect.unsafe.implicits.global
import com.serenity.config.PreferredWindowSize
import com.serenity.io.FileDialog
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
      mouseTargetCacheRef      <- Ref.of[IO, Option[MouseTargetCache]](None)
      documentAnalysisFiberRef <- Ref.of[IO, Option[Fiber[IO, Throwable, Unit]]](None)
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
        mouseTargetCacheRef = mouseTargetCacheRef,
        documentAnalysisFiberRef = documentAnalysisFiberRef,
        onFontConfigChanged = (_: FontConfig) => IO.unit,
        deviceTextScaleProvider = IO.pure(1.0),
        configPersistencePath = None,
        uiPresetStore = UiPresetStore.default,
        windowSizeProvider = IO.pure(Some(PreferredWindowSize(1000, 700))),
        onPreferredWindowSizeChanged = (_: PreferredWindowSize) => IO.unit,
        fileDialog = FileDialog.unavailable
      )
    yield
      runtime.stateRef shouldBe stateRef
      runtime.undoRef shouldBe undoRef
      runtime.themeNamesRef shouldBe themeNamesRef
      runtime.quitSignal shouldBe quitSignal
      runtime.lspQueue shouldBe lspQueue
      runtime.mouseTargetCacheRef shouldBe mouseTargetCacheRef
      runtime.documentAnalysisFiberRef shouldBe documentAnalysisFiberRef
      runtime.sessionManager.sessionExists.unsafeRunSync() shouldBe false
      runtime.fileManager should not be null
      runtime.fileDialog shouldBe FileDialog.unavailable
      runtime.sessionPersistence should not be null

    program.unsafeRunSync()
  }

  it should "cancel pending document analysis when force quitting" in {
    val program = for
      stateRef                 <- Ref.of[IO, AppState](AppState.initial)
      undoRef                  <- Ref.of[IO, UndoState](UndoState())
      themeNamesRef            <- Ref.of[IO, List[String]](List("dark"))
      quitSignal               <- Deferred[IO, Unit]
      lspQueue                 <- LspEffectQueue.create
      mouseTargetCacheRef      <- Ref.of[IO, Option[MouseTargetCache]](None)
      documentAnalysisFiberRef <- Ref.of[IO, Option[Fiber[IO, Throwable, Unit]]](None)
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
        mouseTargetCacheRef = mouseTargetCacheRef,
        documentAnalysisFiberRef = documentAnalysisFiberRef,
        onFontConfigChanged = (_: FontConfig) => IO.unit,
        deviceTextScaleProvider = IO.pure(1.0),
        configPersistencePath = None,
        uiPresetStore = UiPresetStore.default,
        windowSizeProvider = IO.pure(Some(PreferredWindowSize(1000, 700))),
        onPreferredWindowSizeChanged = (_: PreferredWindowSize) => IO.unit,
        fileDialog = FileDialog.unavailable
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
        runtime.mouseTargetCacheRef,
        runtime.documentAnalysisFiberRef,
        runtime.onFontConfigChanged,
        runtime.deviceTextScaleProvider,
        runtime.configPersistencePath,
        runtime.uiPresetStore,
        runtime.windowSizeProvider,
        runtime.fileDialog,
        runtime.fileManager,
        runtime.sessionManager,
        runtime.sessionPersistence,
        operations
      )
      _         <- analysisStarted.get
      _         <- documentAnalysisFiberRef.set(Some(pendingAnalysis))
      _         <- composition.forceQuit()
      cancelled <- analysisCancelled.tryGet
      pending   <- documentAnalysisFiberRef.get
      _         <- pending.fold(IO.unit)(_.cancel)
    yield
      cancelled shouldBe Some(())
      pending shouldBe None

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
          lifecycle = _ => IO.unit,
          command = _ => IO.unit,
          theme = _ => IO.unit,
          surface = _ => IO.unit,
          file = _ => IO.unit,
          explorer = _ => IO.unit,
          workflow = _ => IO.unit,
          lspQueue =
            case LspQueueEffect.Enqueue(value) => observed.update(_ :+ value)
            case LspQueueEffect.DocumentChanged(uri, languageId, text) =>
              observed.update(_ :+ LspEffect.FileChanged(uri, languageId, text, version = 0))
        )
      )
      _       <- behavior.interpret(AppEffect.LspQueue(LspQueueEffect.Enqueue(effect)))
      effects <- observed.get
    yield effects shouldBe List(effect)

    program.unsafeRunSync()
  }
