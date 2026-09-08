package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.std.Semaphore
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.command.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.AppMode
import com.serenity.io.{FileDialog, FileManager}
import com.serenity.keystroke.events.Event
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.LossyRichTextOverwriteException
import com.serenity.rope.Balance
import com.serenity.session.{SessionManager, SessionPersistence, SessionSaveTrigger}
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.layout.{PanelContent, PanelPosition, PanelTarget, PeekContent}
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import com.serenity.ui.tui.MarkdownPreviewWindowAvailability
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Exercises [[StateManagerEffectHandlers]] on its own: every capability port it depends on (runtime, editor, surfaces,
  * files, sessions, modal-workflow) is a recording double, so each `CommandIntent`/`AppEffect` case can be checked for
  * "which collaborator fired, with what" or "what landed in state" rather than through a fully composed `StateManager`.
  */
class StateManagerEffectHandlersSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  final private class RecordingSessionPersistence(triggers: Ref[IO, List[SessionSaveTrigger]], root: Path)
      extends SessionPersistence(
        SessionManager.create(root, AppThemeManager.create, NoOpLogger.impl[IO], SessionManager.SessionPolicy()),
        SessionManager.SessionPolicy()
      ):
    override def maybeSaveSession(appState: AppState, trigger: SessionSaveTrigger): IO[Unit] =
      triggers.update(_ :+ trigger)

  final private class Harness(
      val stateRef: Ref[IO, AppState],
      val committedStates: Ref[IO, List[AppState]],
      val events: Ref[IO, List[Event]],
      val calls: Ref[IO, List[String]],
      val fontConfigs: Ref[IO, List[com.serenity.ui.fonts.FontLoader.FontConfig]],
      val sessionTriggers: Ref[IO, List[SessionSaveTrigger]],
      val quitSignal: Deferred[IO, Unit],
      val lspQueue: LspEffectQueue,
      val handlers: StateManagerEffectHandlers
  ):
    def currentState: AppState = stateRef.get.unsafeRunSync()

  private def harness(
    initialState: AppState = AppState.initial,
    fileDialogOpt: Option[FileDialog] = None,
    saveExistingBufferHook: BufferId => IO[Unit] = _ => IO.unit,
    loadSessionResult: IO[Option[AppState]] = IO.pure(None)
  ): Harness =
    val stateRefVar        = Ref.of[IO, AppState](initialState).unsafeRunSync()
    val committedVar       = Ref.of[IO, List[AppState]](Nil).unsafeRunSync()
    val eventsVar          = Ref.of[IO, List[Event]](Nil).unsafeRunSync()
    val callsVar           = Ref.of[IO, List[String]](Nil).unsafeRunSync()
    val fontConfigsVar     = Ref.of[IO, List[com.serenity.ui.fonts.FontLoader.FontConfig]](Nil).unsafeRunSync()
    val sessionRoot        = Files.createTempDirectory("effect-handlers-spec")
    val sessionTriggersVar = Ref.of[IO, List[SessionSaveTrigger]](Nil).unsafeRunSync()
    val themeNamesRefVar   = Ref.of[IO, List[String]](Nil).unsafeRunSync()
    val bufferAnimationsRefVar =
      Ref.of[IO, Map[BufferId, com.serenity.animation.AnimationState]](Map.empty).unsafeRunSync()
    val quitSignalVar           = Deferred[IO, Unit].unsafeRunSync()
    val lspQueueVar             = LspEffectQueue.create.unsafeRunSync()
    val projectTaskFiberRefVar  = Ref.of[IO, Option[ManagedProjectTask]](None).unsafeRunSync()
    val projectTaskSemaphoreVar = Semaphore[IO](1).unsafeRunSync()

    val runtime = new EffectRuntimePort:
      val stateRef             = stateRefVar
      val themeNamesRef        = themeNamesRefVar
      val quitSignal           = quitSignalVar
      val logger               = NoOpLogger.impl[IO]
      val themeManager         = AppThemeManager.create
      val lspQueue             = lspQueueVar
      val projectTaskFiberRef  = projectTaskFiberRefVar
      val projectTaskSemaphore = projectTaskSemaphoreVar
      val onFontConfigChanged =
        (config: com.serenity.ui.fonts.FontLoader.FontConfig) => fontConfigsVar.update(_ :+ config)
      val deviceTextScaleProvider = IO.pure(1.0)
      val configPersistencePath   = None
      val uiPresetStore           = UiPresetStore(sessionRoot.resolve("ui-presets.json"))
      val windowSizeProvider      = IO.pure(None)
      val bufferAnimationsRef     = bufferAnimationsRefVar
      val markdownPreviewWindow   = MarkdownPreviewWindowAvailability.Unavailable

    val editor = new EffectEditorPort:
      def updateState(update: AppState => AppState): IO[Unit] = stateRefVar.update(update)
      def enqueueEvent(event: Event): IO[Unit]                = eventsVar.update(_ :+ event)
      def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
        committedVar.update(_ :+ newState) >> stateRefVar.set(newState)
      def scheduleDocumentAnalysis(): IO[Unit] = IO.unit
      def scheduleFindSearch(request: FindSearchRequest): IO[Unit] =
        callsVar.update(_ :+ s"scheduleFindSearch:$request")

    val surfaces = new EffectSurfacePort:
      def showPeek(content: PeekContent, at: CursorPosition): IO[Unit] = callsVar.update(_ :+ s"showPeek:$content")
      def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit] =
        callsVar.update(_ :+ s"pinPanel:$content:$position:$size")
      def pinOrUpdateTerminalPanel(text: String, position: PanelPosition, size: Int): IO[Unit] =
        callsVar.update(_ :+ s"pinOrUpdateTerminalPanel:$text")
      def unpinPanel(target: PanelTarget): IO[Unit]        = callsVar.update(_ :+ s"unpinPanel:$target")
      def expandPinnedPanel(target: PanelTarget): IO[Unit] = callsVar.update(_ :+ s"expandPinnedPanel:$target")
      def collapseExpandedPanel(): IO[Unit]                = callsVar.update(_ :+ "collapseExpandedPanel")
      def switchToPinnedPanel(target: PanelTarget): IO[Unit] =
        callsVar.update(_ :+ s"switchToPinnedPanel:$target")
      def resizePinnedPanel(target: PanelTarget, newSize: Int): IO[Unit] =
        callsVar.update(_ :+ s"resizePinnedPanel:$target:$newSize")

    val files = new EffectFilePort:
      val fileDialog  = fileDialogOpt
      val fileManager = new FileManager()
      def saveExistingBuffer(id: BufferId): IO[Unit] =
        callsVar.update(_ :+ s"saveExistingBuffer:$id") >> saveExistingBufferHook(id)
      def saveBufferAs(id: BufferId, path: Path): IO[Unit] = callsVar.update(_ :+ s"saveBufferAs:$id:$path")

    val sessions = new EffectSessionPort:
      val sessionPersistence                  = new RecordingSessionPersistence(sessionTriggersVar, sessionRoot)
      def saveSession(): IO[Unit]             = callsVar.update(_ :+ "saveSession")
      def loadSession(): IO[Option[AppState]] = callsVar.update(_ :+ "loadSession") >> loadSessionResult
      def clearSession(): IO[Unit]            = callsVar.update(_ :+ "clearSession")

    val workflow = new EffectModalWorkflowPort:
      def clearCloseActions(state: AppState): AppState = state
      def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit] =
        callsVar.update(_ :+ s"beginCloseAction:$scope")
      def showSaveAsWorkflow(state: AppState, bufferId: BufferId, statusMessage: String): IO[Unit] =
        callsVar.update(_ :+ s"showSaveAsWorkflow:$bufferId:$statusMessage")
      def openFileWorkflowModal(mode: FileWorkflowMode, state: AppState): IO[Unit] =
        callsVar.update(_ :+ s"openFileWorkflowModal:$mode")
      def requestSaveAsFileDialog(state: AppState, bufferIdOverride: Option[BufferId]): IO[Unit] =
        callsVar.update(_ :+ s"requestSaveAsFileDialog:$bufferIdOverride")
      def refreshFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
        callsVar.update(_ :+ s"refreshFileWorkflowEffect:$surfaceId")
      def submitFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
        callsVar.update(_ :+ s"submitFileWorkflowEffect:$surfaceId")
      def submitReplaceWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
        callsVar.update(_ :+ s"submitReplaceWorkflowEffect:$surfaceId")
      def submitCloseWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
        callsVar.update(_ :+ s"submitCloseWorkflowEffect:$surfaceId")
      def createFileWorkflowDirectoriesEffect(surfaceId: SurfaceId): IO[Unit] =
        callsVar.update(_ :+ s"createFileWorkflowDirectoriesEffect:$surfaceId")
      def restoreSessionIntoCurrentViewport(restoredState: AppState, currentState: AppState): AppState =
        restoredState
      def createStartupSession(): IO[Unit]                        = callsVar.update(_ :+ "createStartupSession")
      def restoreStartupSession(): IO[Unit]                       = callsVar.update(_ :+ "restoreStartupSession")
      def activeEditorBufferId(state: AppState): Option[BufferId] = state.focusedBufferId

    new Harness(
      stateRefVar,
      committedVar,
      eventsVar,
      callsVar,
      fontConfigsVar,
      sessionTriggersVar,
      quitSignalVar,
      lspQueueVar,
      new StateManagerEffectHandlers(runtime, editor, surfaces, files, sessions, workflow)
    )

  private def command(intent: CommandIntent, category: CommandCategory = CommandCategory.Edit): Command =
    Command.typed("test-command", "A test command.", intent, category)

  // ---------------------------------------------------------------------------------------------------------------
  // Lifecycle / file intents
  // ---------------------------------------------------------------------------------------------------------------

  "StateManagerEffectHandlers" should "begin the quit close action for QuitApp" in {
    val fixture = harness()

    fixture.handlers
      .interpretCommand(command(CommandIntent.Lifecycle(LifecycleIntent.QuitApp)), AppState.initial)
      .unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"beginCloseAction:${CloseScope.Quit}")
  }

  it should "save the focused buffer directly when it already has a file path" in {
    val path = Path.of("notes.txt")
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = Map(bufferId -> Buffer.fromFile(bufferId, path, "hello")))
    )
    val fixture = harness(state)

    fixture.handlers.interpretCommand(command(CommandIntent.File(FileIntent.SaveCurrentFile)), state).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"saveExistingBuffer:$bufferId")
  }

  it should "do nothing saving the current file when no buffer is focused" in {
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted
        .copy(layout = com.serenity.ui.layout.Layout(editorPanes = Map.empty, activeEditorPaneId = None))
    )
    val fixture = harness(state)

    fixture.handlers.interpretCommand(command(CommandIntent.File(FileIntent.SaveCurrentFile)), state).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe Nil
  }

  it should "request the native save-as dialog for SaveCurrentFileAs" in {
    val fixture = harness()

    fixture.handlers
      .interpretCommand(command(CommandIntent.File(FileIntent.SaveCurrentFileAs)), AppState.initial)
      .unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"requestSaveAsFileDialog:${AppState.initial.focusedBufferId}")
  }

  it should "load through the native open dialog and dismiss surfaces first" in {
    val directory = Files.createTempDirectory("effect-handlers-open")
    val target    = Files.writeString(directory.resolve("picked.txt"), "picked content")
    try
      val dialog = FileDialog(
        chooseOpenFile = _ => IO.pure(Some(target)),
        chooseSaveFile = (_, _) => IO.pure(None)
      )
      val fixture = harness(fileDialogOpt = Some(dialog))

      fixture.handlers
        .interpretCommand(command(CommandIntent.File(FileIntent.OpenFile)), AppState.initial)
        .unsafeRunSync()

      val after = fixture.currentState
      after.persisted.buffers.values.flatMap(_.document.filePath).toList should contain(target)
      after.runtime.uiSurfaces shouldBe Nil
    finally
      Files.deleteIfExists(target)
      Files.deleteIfExists(directory)
  }

  it should "fall back to the in-app workflow modal when no native dialog is available" in {
    val fixture = harness(fileDialogOpt = None)

    fixture.handlers
      .interpretCommand(command(CommandIntent.File(FileIntent.OpenFile)), AppState.initial)
      .unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"openFileWorkflowModal:${FileWorkflowMode.Open}")
  }

  it should "load a readable recent file and dismiss surfaces first" in {
    val directory = Files.createTempDirectory("effect-handlers-recent")
    val target    = Files.writeString(directory.resolve("recent.txt"), "recent content")
    try
      val fixture = harness()

      fixture.handlers
        .interpretCommand(command(CommandIntent.File(FileIntent.OpenRecentFile(target))), AppState.initial)
        .unsafeRunSync()

      val after = fixture.currentState
      after.persisted.buffers.values.flatMap(_.document.filePath).toList should contain(target)
      after.persisted.recentFiles should contain(target)
      after.runtime.uiSurfaces shouldBe Nil
    finally
      Files.deleteIfExists(target)
      Files.deleteIfExists(directory)
  }

  it should "do nothing for an unreadable recent file" in {
    val missing = Path.of("/nonexistent/effect-handlers-recent-missing.txt")
    val fixture = harness()

    fixture.handlers
      .interpretCommand(command(CommandIntent.File(FileIntent.OpenRecentFile(missing))), AppState.initial)
      .unsafeRunSync()

    fixture.currentState shouldBe AppState.initial
  }

  it should "route CloseAll, CloseOthers, and CloseCurrentFile to their close scopes" in {
    val fixture = harness()

    fixture.handlers
      .interpretCommand(command(CommandIntent.File(FileIntent.CloseAll)), AppState.initial)
      .unsafeRunSync()
    fixture.handlers
      .interpretCommand(command(CommandIntent.File(FileIntent.CloseOthers)), AppState.initial)
      .unsafeRunSync()
    fixture.handlers
      .interpretCommand(command(CommandIntent.File(FileIntent.CloseCurrentFile)), AppState.initial)
      .unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(
      s"beginCloseAction:${CloseScope.All}",
      s"beginCloseAction:${CloseScope.Others}",
      s"beginCloseAction:${CloseScope.Current}"
    )
  }

  it should "create a new buffer for NewFile" in {
    val fixture = harness()

    fixture.handlers.interpretCommand(command(CommandIntent.File(FileIntent.NewFile)), AppState.initial).unsafeRunSync()

    fixture.currentState.persisted.bufferOrder.size shouldBe AppState.initial.persisted.bufferOrder.size + 1
  }

  it should "update the buffer language and open the LSP document when app mode is Code" in {
    val path = Path.of("main.py")
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(bufferId -> Buffer.fromFile(bufferId, path, "print(1)")),
        config = AppState.initial.persisted.config.withAppMode(AppMode.Code)
      )
    )
    val fixture = harness(state)

    fixture.handlers
      .interpretCommand(
        command(CommandIntent.File(FileIntent.SetBufferLanguage(Some(LanguageId.Python)))),
        state
      )
      .unsafeRunSync()

    fixture.currentState.persisted.buffers(bufferId).document.language shouldBe Some(LanguageId.Python)
    val opened = fixture.lspQueue.stream
      .take(1)
      .compile
      .toList
      .timeoutTo(
        scala.concurrent.duration.DurationInt(1).second,
        IO.pure(Nil)
      )
      .unsafeRunSync()
    opened shouldBe List(LspEffect.FileOpened(path.toUri.toString, LanguageId.Python, "print(1)"))
  }

  it should "skip the LSP refresh when the buffer's language does not change" in {
    val path = Path.of("main.py")
    val buffer = Buffer
      .fromFile(bufferId, path, "print(1)")
      .copy(document = Buffer.fromFile(bufferId, path, "print(1)").document.copy(language = Some(LanguageId.Python)))
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        config = AppState.initial.persisted.config.withAppMode(AppMode.Code)
      )
    )
    val fixture = harness(state)

    fixture.handlers
      .interpretCommand(
        command(CommandIntent.File(FileIntent.SetBufferLanguage(Some(LanguageId.Python)))),
        state
      )
      .unsafeRunSync()

    val opened = fixture.lspQueue.stream
      .take(1)
      .compile
      .toList
      .timeoutTo(
        scala.concurrent.duration.DurationInt(1).second,
        IO.pure(Nil)
      )
      .unsafeRunSync()
    opened shouldBe Nil
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Edit intents
  // ---------------------------------------------------------------------------------------------------------------

  it should "show a find modal seeded from the buffer's existing find state" in {
    val buffer = Buffer
      .fromString(bufferId, "cat dog cat")
      .copy(findState = Some(FindState(query = "cat", results = Nil, currentIndex = 0)))
    val state   = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
    val fixture = harness(state)

    fixture.handlers
      .interpretCommand(command(CommandIntent.Edit(EditIntent.FindInCurrentFile)), state)
      .unsafeRunSync()

    fixture.currentState.runtime.uiSurfaces.map(_.content) match
      case List(SurfaceContent.ModalWorkflow(Modal.Find(query, results, _))) =>
        query shouldBe "cat"
        results.size shouldBe 2
      case other => fail(s"Expected a single Find modal surface, got $other")
  }

  it should "forward clipboard and undo commands as editor events in order" in {
    val fixture = harness()

    fixture.handlers.interpretCommand(command(CommandIntent.Edit(EditIntent.Copy)), AppState.initial).unsafeRunSync()
    fixture.handlers.interpretCommand(command(CommandIntent.Edit(EditIntent.Paste)), AppState.initial).unsafeRunSync()
    fixture.handlers.interpretCommand(command(CommandIntent.Edit(EditIntent.Undo)), AppState.initial).unsafeRunSync()

    fixture.events.get.unsafeRunSync() shouldBe List(
      com.serenity.keystroke.events.Copy,
      com.serenity.keystroke.events.Paste,
      com.serenity.keystroke.events.Undo
    )
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Session intents
  // ---------------------------------------------------------------------------------------------------------------

  it should "delegate session save, clear, and startup intents to their ports" in {
    val fixture = harness()

    fixture.handlers
      .interpretCommand(command(CommandIntent.Session(SessionIntent.SaveSession)), AppState.initial)
      .unsafeRunSync()
    fixture.handlers
      .interpretCommand(command(CommandIntent.Session(SessionIntent.ClearSession)), AppState.initial)
      .unsafeRunSync()
    fixture.handlers
      .interpretCommand(command(CommandIntent.Session(SessionIntent.StartupNewSession)), AppState.initial)
      .unsafeRunSync()
    fixture.handlers
      .interpretCommand(command(CommandIntent.Session(SessionIntent.StartupRestoreSession)), AppState.initial)
      .unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(
      "saveSession",
      "clearSession",
      "createStartupSession",
      "restoreStartupSession"
    )
  }

  it should "restore a saved session into the current viewport" in {
    val restored = AppState.initial.copy(runtime = AppState.initial.runtime.copy(nextBufferId = BufferId(99)))
    val fixture  = harness(loadSessionResult = IO.pure(Some(restored)))

    fixture.handlers
      .interpretCommand(command(CommandIntent.Session(SessionIntent.RestoreSession)), AppState.initial)
      .unsafeRunSync()

    fixture.currentState.runtime.nextBufferId shouldBe BufferId(99)
    fixture.committedStates.get.unsafeRunSync() shouldBe List(restored)
  }

  it should "do nothing restoring a session when none was saved" in {
    val fixture = harness(loadSessionResult = IO.pure(None))

    fixture.handlers
      .interpretCommand(command(CommandIntent.Session(SessionIntent.RestoreSession)), AppState.initial)
      .unsafeRunSync()

    fixture.committedStates.get.unsafeRunSync() shouldBe Nil
    fixture.currentState shouldBe AppState.initial
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Direct save/load entry points
  // ---------------------------------------------------------------------------------------------------------------

  it should "do nothing for directLoadFileEffect on an unreadable path" in {
    val fixture = harness()

    fixture.handlers.directLoadFileEffect(Path.of("/nonexistent/direct-load.txt")).unsafeRunSync()

    fixture.currentState shouldBe AppState.initial
  }

  it should "open the Save As workflow when saving hits a lossy rich-text overwrite" in {
    val path = Path.of("rich.txt")
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = Map(bufferId -> Buffer.fromFile(bufferId, path, "styled")))
    )
    val failing: BufferId => IO[Unit] = _ => IO.raiseError(new LossyRichTextOverwriteException("would lose styling"))
    val fixture                       = harness(state, saveExistingBufferHook = failing)

    fixture.handlers.saveBufferEffect(bufferId).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() should contain(s"showSaveAsWorkflow:$bufferId:would lose styling")
  }

  it should "open the native Save As dialog when the buffer has no file path" in {
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = Map(bufferId -> Buffer.fromString(bufferId, "unsaved")))
    )
    val fixture = harness(state)

    fixture.handlers.saveBufferEffect(bufferId).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"requestSaveAsFileDialog:${Some(bufferId)}")
  }

  it should "do nothing saving an unknown buffer" in {
    val fixture = harness()

    fixture.handlers.saveBufferEffect(BufferId(999)).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe Nil
  }

  it should "save an existing buffer as, but not a missing one" in {
    val path = Path.of("target.txt")
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = Map(bufferId -> Buffer.fromString(bufferId, "content")))
    )
    val fixture = harness(state)

    fixture.handlers.saveBufferAsEffect(bufferId, path).unsafeRunSync()
    fixture.handlers.saveBufferAsEffect(BufferId(999), path).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"saveBufferAs:$bufferId:$path")
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Effect dispatch and config delegation
  // ---------------------------------------------------------------------------------------------------------------

  it should "complete the quit signal for a CompleteQuit effect" in {
    val fixture = harness()

    fixture.handlers.interpretEffect(AppEffect.CompleteQuit).unsafeRunSync()

    fixture.quitSignal.tryGet.unsafeRunSync() shouldBe Some(())
  }

  it should "enqueue an LSP effect from an LspQueue effect" in {
    val effect  = LspEffect.FileClosed("file:///dispatch.txt", LanguageId.Scala)
    val fixture = harness()

    fixture.handlers.interpretEffect(AppEffect.LspQueue(LspQueueEffect.Enqueue(effect))).unsafeRunSync()

    val received = fixture.lspQueue.stream
      .take(1)
      .compile
      .toList
      .timeoutTo(scala.concurrent.duration.DurationInt(1).second, IO.pure(Nil))
      .unsafeRunSync()
    received shouldBe List(effect)
  }

  it should "open the file-search surface from a Surface effect" in {
    val fixture = harness()

    fixture.handlers.interpretEffect(AppEffect.Surface(SurfaceEffect.OpenFileSearch)).unsafeRunSync()

    fixture.currentState.runtime.uiSurfaces.map(_.content) match
      case List(SurfaceContent.FileSearch(_)) => succeed
      case other                              => fail(s"Expected a single FileSearch surface, got $other")
  }

  it should "persist a config update through to state and report the resulting config" in {
    val fixture = harness()

    val reported = fixture.handlers.updateConfig(_.withWheelScrollLines(11)).unsafeRunSync()

    reported.inputConfig.wheelScrollLines shouldBe 11
    fixture.currentState.persisted.config.inputConfig.wheelScrollLines shouldBe 11
    fixture.sessionTriggers.get.unsafeRunSync() shouldBe List(SessionSaveTrigger.Manual)
  }

  it should "report the resulting font config from updateFontConfig" in {
    val fixture = harness()

    fixture.handlers
      .updateFontConfig(_.copy(textScaleMode = com.serenity.ui.fonts.FontLoader.TextScaleMode.Auto))
      .unsafeRunSync()

    val reported = fixture.fontConfigs.get.unsafeRunSync()
    reported.size shouldBe 1
    reported.head.textScaleMode shouldBe com.serenity.ui.fonts.FontLoader.TextScaleMode.Auto
  }
