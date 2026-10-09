package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.data.State
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.StateManagerTestFixtures
import com.serenity.command.{Command, CommandCategory, CommandIntent, FileIntent, SessionIntent, ViewIntent}
import com.serenity.config.PreferredWindowSize
import com.serenity.keystroke.events.{Enter, InsertChar, NextTab, TabKey, ToggleCommandRunner, Undo}
import com.serenity.project.{ProjectTaskCommand, ProjectTaskKind, ProjectTaskResult}
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer
import com.serenity.state.undo.{HistoryEntry, UndoState}
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{PanelContent, PanelPosition}
import com.serenity.ui.presets.{UiPreset, UiPresetStore}
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** The dispatcher-owned model (#1697 F3): app state and undo history live in one `Ref`, so an operation that changes
  * several of them commits them together.
  *
  * The model ref here records every value written to it. That record is exactly the set of snapshots a reader (the
  * renderer, `getModel`) could ever observe, so "no recorded write is torn" means no reader can see a torn model.
  */
class ModelAtomicitySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val quietLogger: Logger[IO] = NoOpLogger.impl[IO]

  final private class RecordingRef[A](underlying: Ref[IO, A], writes: Ref[IO, Vector[A]]) extends Ref[IO, A]:

    private def recorded[B](f: A => (A, B)): IO[B] =
      underlying
        .modify { a =>
          val (next, result) = f(a)
          (next, (next, result))
        }
        .flatMap((next, result) => writes.update(_ :+ next).as(result))

    def get: IO[A]                                           = underlying.get
    def set(a: A): IO[Unit]                                  = recorded(_ => (a, ()))
    def update(f: A => A): IO[Unit]                          = recorded(a => (f(a), ()))
    def modify[B](f: A => (A, B)): IO[B]                     = recorded(f)
    def tryUpdate(f: A => A): IO[Boolean]                    = update(f).as(true)
    def tryModify[B](f: A => (A, B)): IO[Option[B]]          = modify(f).map(Some(_))
    def modifyState[B](state: State[A, B]): IO[B]            = modify(a => state.run(a).value)
    def tryModifyState[B](state: State[A, B]): IO[Option[B]] = modifyState(state).map(Some(_))
    def access: IO[(A, A => IO[Boolean])] =
      underlying.get.map(snapshot => (snapshot, next => set(next).as(true)))

  final private case class Recorded(modelRef: Ref[IO, Model], writes: Ref[IO, Vector[Model]]):
    def clear: IO[Unit]                   = writes.set(Vector.empty)
    def recordedWrites: IO[Vector[Model]] = writes.get

  private def recording(initial: Model): IO[Recorded] =
    for
      underlying <- Ref.of[IO, Model](initial)
      writes     <- Ref.of[IO, Vector[Model]](Vector.empty)
    yield Recorded(new RecordingRef(underlying, writes), writes)

  private def stateManagerOver(
    modelRef: Ref[IO, Model],
    uiPresetStore: Option[UiPresetStore] = None
  ): IO[StateManager] =
    for
      directory           <- IO.blocking(Files.createTempDirectory("model-atomicity-spec"))
      themeNamesRef       <- Ref.of[IO, List[String]](Nil)
      quitSignal          <- Deferred[IO, Unit]
      lspQueue            <- LspEffectQueue.create
      mouseTargetCacheRef <- Ref.of[IO, Option[MouseTargetCache]](None)
      runtime = StateManagerRuntime.create(
        modelRef = modelRef,
        themeNamesRef = themeNamesRef,
        quitSignal = quitSignal,
        logger = quietLogger,
        policy = SessionManager.SessionPolicy(),
        sessionRootOverride = Some(directory.resolve("session")),
        themeManager = AppThemeManager.create,
        lspQueue = lspQueue,
        mouseTargetCacheRef = mouseTargetCacheRef,
        onFontConfigChanged = (_: FontConfig) => IO.unit,
        deviceTextScaleProvider = IO.pure(1.0),
        configPersistencePath = None,
        uiPresetStore = uiPresetStore.getOrElse(UiPresetStore(directory.resolve("presets.json"))),
        windowSizeProvider = IO.pure(None),
        onPreferredWindowSizeChanged = (_: PreferredWindowSize) => IO.unit,
        fileDialog = None,
        dictionaryCache = SharedDictionary.default
      )
      stateManager <- StateManager.fromRuntime(runtime)
    yield stateManager

  private val initialModel: Model = Model(AppState.initial, UndoState())

  /** A fresh buffer holding "Hello", focused in the first pane with the cursor at its end. */
  private def focusedHelloBuffer(stateManager: StateManager): IO[BufferId] =
    for
      bufferId <- stateManager.createBuffer("Hello", None)
      state    <- stateManager.getCurrentState
      paneId <- IO.fromOption(state.persisted.layout.editorPanes.keys.headOption)(
        new IllegalStateException("no editor pane")
      )
      _ <- stateManager.updateState(StateManagerTestFixtures.setBufferForPane(paneId, bufferId))
      _ <- stateManager.updateState(StateManagerTestFixtures.setCursorPosition(paneId, 0, 5))
    yield bufferId

  private def content(model: Model, bufferId: BufferId): Option[String] =
    model.app.persisted.buffers.get(bufferId).map(_.document.content.toString)

  private def hasUndoHistory(model: Model): Boolean =
    model.undo.undoStack.nonEmpty

  "An edit" should "commit its text and its undo entry in one write" in {
    val program =
      for
        recorded     <- recording(initialModel)
        stateManager <- stateManagerOver(recorded.modelRef)
        bufferId     <- focusedHelloBuffer(stateManager)
        _            <- recorded.clear
        _            <- stateManager.applyEvent(InsertChar('a'))
        writes       <- recorded.recordedWrites
        after        <- stateManager.getModel
      yield (bufferId, writes, after)

    val (bufferId, writes, after) = program.unsafeRunSync()

    content(after, bufferId) shouldBe Some("Helloa")
    hasUndoHistory(after) shouldBe true
    writes should not be empty
    all(writes.map(model => content(model, bufferId).contains("Helloa") == hasUndoHistory(model))) shouldBe true
  }

  "Undo" should "restore the text and move its entry to the redo stack in one write" in {
    val program =
      for
        recorded     <- recording(initialModel)
        stateManager <- stateManagerOver(recorded.modelRef)
        bufferId     <- focusedHelloBuffer(stateManager)
        _            <- stateManager.applyEvent(InsertChar('a'))
        _            <- recorded.clear
        _            <- stateManager.applyEvent(Undo)
        writes       <- recorded.recordedWrites
        after        <- stateManager.getModel
      yield (bufferId, writes, after)

    val (bufferId, writes, after) = program.unsafeRunSync()

    content(after, bufferId) shouldBe Some("Hello")
    after.undo.redoStack should not be empty
    writes should not be empty
    all(writes.map(model => content(model, bufferId).contains("Hello") == model.undo.redoStack.nonEmpty)) shouldBe true
  }

  "A validated model write" should "leave every part of the model unchanged when the app state it carries is invalid" in {
    val before = Model(AppState.initial, UndoState())
    val invalid =
      AppState.initial.copy(persisted = AppState.initial.persisted.copy(focus = Focus.EditorPane(PaneId(999))))
    val program =
      for
        recorded <- recording(before)
        operations <- StateManagerOperationBoundary.create(
          recorded.modelRef,
          quietLogger,
          dictionaryCache = SharedDictionary.default
        )
        commit = operations.modelCommit
        _     <- commit.updateValidated(_ => Some(Model(invalid, UndoState(maxUndoDepth = 3))))
        after <- recorded.modelRef.get
      yield after

    program.unsafeRunSync() shouldBe before
  }

  "Cycling to the next tab" should "commit the buffer switch in one write" in {
    val program =
      for
        recorded     <- recording(initialModel)
        stateManager <- stateManagerOver(recorded.modelRef)
        _            <- focusedHelloBuffer(stateManager)
        nextBufferId <- stateManager.createBuffer("World", None)
        _            <- recorded.clear
        _            <- stateManager.applyEvent(NextTab)
        writes       <- recorded.recordedWrites
        after        <- stateManager.getModel
      yield (nextBufferId, writes, after)

    val (nextBufferId, writes, after) = program.unsafeRunSync()

    def showsNext(model: Model): Boolean = model.app.focusedBufferId.contains(nextBufferId)

    showsNext(after) shouldBe true
    writes should not be empty
  }

  private def viewCommand(intent: ViewIntent): Command =
    Command.typed("panel-pin", "Pin panel", CommandIntent.View(intent), CommandCategory.View)

  private def diagnosticsPinned(model: Model): Boolean =
    model.app.runtime.uiSurfaces.exists(_.content == SurfaceContent.Diagnostics(Nil))

  "Pinning a panel" should "commit the panel and its undo boundary in one write" in {
    // Spell check is off so no analysis result lands in the diagnostics panel this test judges the pin by.
    val quiet = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(config =
        AppState.initial.persisted.config
          .withSpellCheck(AppState.initial.persisted.config.languageToolsConfig.spellCheck.copy(enabled = false))
      )
    )
    val program =
      for
        recorded     <- recording(Model(quiet, UndoState()))
        stateManager <- stateManagerOver(recorded.modelRef)
        _            <- stateManager.executeCommand(viewCommand(ViewIntent.TogglePanelShown(PanelId.Diagnostics)))
        writes       <- recorded.recordedWrites
        after        <- stateManager.getModel
      yield (writes, after)

    val (writes, after) = program.unsafeRunSync()

    diagnosticsPinned(after) shouldBe true
    after.undo.undoStack shouldBe Vector(HistoryEntry.PanelChange.capture(quiet))
    all(writes.map(model => diagnosticsPinned(model) == model.undo.undoStack.nonEmpty)) shouldBe true
  }

  "Pinning a panel through the panel manager" should "commit the panel and its undo boundary in one write" in {
    val program =
      for
        recorded     <- recording(Model(AppState.initial, UndoState()))
        stateManager <- stateManagerOver(recorded.modelRef)
        _            <- stateManager.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Right, 30)
        writes       <- recorded.recordedWrites
        after        <- stateManager.getModel
      yield (writes, after)

    val (writes, after) = program.unsafeRunSync()

    diagnosticsPinned(after) shouldBe true
    after.undo.undoStack should have size 1
    writes should not be empty
    all(writes.map(model => diagnosticsPinned(model) == model.undo.undoStack.nonEmpty)) shouldBe true
  }

  "Unpinning a panel" should "commit the removal and its undo boundary in one write" in {
    val unpin = ViewIntent.SetPanelPin(PanelId.Diagnostics, None)
    val program =
      for
        recorded     <- recording(Model(AppState.initial, UndoState()))
        stateManager <- stateManagerOver(recorded.modelRef)
        _            <- stateManager.executeCommand(viewCommand(ViewIntent.TogglePanelShown(PanelId.Diagnostics)))
        _            <- recorded.clear
        _            <- stateManager.executeCommand(viewCommand(unpin))
        writes       <- recorded.recordedWrites
        after        <- stateManager.getModel
      yield (writes, after)

    val (writes, after) = program.unsafeRunSync()

    diagnosticsPinned(after) shouldBe false
    after.undo.undoStack should have size 2
    writes should not be empty
    all(writes.map(model => diagnosticsPinned(model) == (model.undo.undoStack.size == 1))) shouldBe true
  }

  private def closePromptFor(model: Model): Option[BufferId] =
    com.serenity.ClosePromptFixtures.closePromptShown(model.app).map(_.currentBufferId)

  "Discarding one buffer of a close-all" should "close it and prompt for the next in one write" in {
    val closeAll =
      Command.typed("close-all", "Close all", CommandIntent.File(FileIntent.CloseAll), CommandCategory.File)
    val program =
      for
        recorded     <- recording(Model(AppState.initial, UndoState()))
        stateManager <- stateManagerOver(recorded.modelRef)
        first        <- focusedHelloBuffer(stateManager)
        second       <- stateManager.createBuffer("World", None)
        _            <- stateManager.executeCommand(closeAll)
        prompted     <- stateManager.getModel
        _            <- stateManager.applyEvent(TabKey)
        _            <- recorded.clear
        _            <- stateManager.applyEvent(Enter)
        writes       <- recorded.recordedWrites
        after        <- stateManager.getModel
      yield (first, second, prompted, writes, after)

    val (first, second, prompted, writes, after) = program.unsafeRunSync()

    closePromptFor(prompted) shouldBe Some(first)
    closePromptFor(after) shouldBe Some(second)
    after.app.persisted.buffers should not contain key(first)
    writes should not be empty
    all(
      writes.map(model => model.app.persisted.buffers.contains(first) || closePromptFor(model).contains(second))
    ) shouldBe true
  }

  "Restoring a startup session when none was saved" should "commit its fresh buffer and pane in one write" in {
    val restore = Command.typed(
      "startup-restore",
      "Restore session",
      CommandIntent.Session(SessionIntent.StartupRestoreSession),
      CommandCategory.File
    )
    val program =
      for
        recorded     <- recording(Model(AppState.initial, UndoState()))
        stateManager <- stateManagerOver(recorded.modelRef)
        _            <- stateManager.executeCommand(restore)
        writes       <- recorded.recordedWrites
        after        <- stateManager.getModel
      yield (writes, after)

    val (writes, after) = program.unsafeRunSync()

    val created = after.app.persisted.buffers.keySet - BufferId(0)
    created should have size 1
    after.app.focusedBufferId shouldBe created.headOption
    val withCreated = writes.filter(_.app.persisted.buffers.size == 2)
    withCreated should not be empty
    all(withCreated.map(_.app.focusedBufferId)) shouldBe created.headOption
  }

  private def runnerPresetPreviews(model: Model): Option[List[UiPreset.Preview]] =
    model.app.commandRunnerSurface.map(_.content).collect {
      case SurfaceContent.CommandPalette(runner) =>
        runner.uiPresetPreviews
    }

  "Opening the command runner" should "commit the runner at once and its UI preset previews in a write of their own" in {
    val program =
      for
        directory <- IO.blocking(Files.createTempDirectory("model-atomicity-presets"))
        store = UiPresetStore(directory.resolve("presets.json"))
        _            <- store.create(UiPreset.capture("Existing", AppState.initial, None))
        recorded     <- recording(Model(AppState.initial, UndoState()))
        stateManager <- stateManagerOver(recorded.modelRef, Some(store))
        _            <- stateManager.applyEvent(ToggleCommandRunner)
        _            <- stateManager.runtimeLifecycle.awaitEffects
        writes       <- recorded.recordedWrites
        after        <- stateManager.getModel
      yield (writes, after)

    val (writes, after) = program.unsafeRunSync()

    runnerPresetPreviews(after).map(_.map(_.name)) shouldBe Some(List("Existing"))
    val withRunner = writes.flatMap(runnerPresetPreviews)
    withRunner.headOption shouldBe Some(Nil)
    withRunner.lastOption.map(_.map(_.name)) shouldBe Some(List("Existing"))
  }

  private def replace(stateManager: StateManager, action: ReplaceWorkflowAction): IO[Unit] =
    stateManager.updateStateValidated(state =>
      ModalStateReducer
        .show(
          Modal.ReplaceWorkflow(ReplaceWorkflowState(findText = "l", replacementText = "L", selectedAction = action)),
          state
        )
        .state
    ) >> stateManager.applyEvent(Enter)

  private def replacePrompt(model: Model): Option[ReplaceWorkflowState] =
    model.app.runtime.uiSurfaces.map(_.content).collectFirst {
      case SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)) => workflow
    }

  "Replace all" should "commit the replaced text and its undo entry in one write" in {
    val program =
      for
        recorded     <- recording(Model(AppState.initial, UndoState()))
        stateManager <- stateManagerOver(recorded.modelRef)
        bufferId     <- focusedHelloBuffer(stateManager)
        _            <- recorded.clear
        _            <- replace(stateManager, ReplaceWorkflowAction.ReplaceAll)
        writes       <- recorded.recordedWrites
        after        <- stateManager.getModel
      yield (bufferId, writes, after)

    val (bufferId, writes, after) = program.unsafeRunSync()

    content(after, bufferId) shouldBe Some("HeLLo")
    after.undo.undoStack should have size 1
    writes should not be empty
    all(writes.map(model => content(model, bufferId).contains("HeLLo") == model.undo.undoStack.nonEmpty)) shouldBe true
  }

  "Replace next" should "commit the replaced text, its undo entry and the prompt's status in one write" in {
    val program =
      for
        recorded     <- recording(Model(AppState.initial, UndoState()))
        stateManager <- stateManagerOver(recorded.modelRef)
        bufferId     <- focusedHelloBuffer(stateManager)
        _            <- recorded.clear
        _            <- replace(stateManager, ReplaceWorkflowAction.ReplaceNext)
        writes       <- recorded.recordedWrites
        after        <- stateManager.getModel
      yield (bufferId, writes, after)

    val (bufferId, writes, after) = program.unsafeRunSync()

    content(after, bufferId) shouldBe Some("HeLlo")
    replacePrompt(after).flatMap(_.statusMessage) shouldBe Some("Replaced next match")
    writes should not be empty
    all(writes.map(model => content(model, bufferId).contains("HeLlo") == model.undo.undoStack.nonEmpty)) shouldBe true
    all(
      writes.map(model =>
        content(model, bufferId).contains("HeLlo") ==
          replacePrompt(model).flatMap(_.statusMessage).contains("Replaced next match")
      )
    ) shouldBe true
  }

  private val terminalTaskCommand: ProjectTaskCommand =
    ProjectTaskCommand(ProjectTaskKind.Build, "sbt", Path.of("."), "sbt", List("compile"))

  private def terminalPinned(model: Model): Boolean =
    model.app.runtime.uiSurfaces.exists(_.content.isInstanceOf[SurfaceContent.Terminal])

  /** A project-task result used to re-pin the Terminal panel, committing that pin and its undo boundary together (#1697
    * Wave 4). It now only records the latest output: a hidden output panel stays hidden, so nothing is pinned and no
    * undo boundary is recorded, in any write.
    */
  "A project-task result" should "record the latest output without re-showing the output panel or touching undo" in {
    val running = RunningProjectTask(id = 0L, command = terminalTaskCommand, output = "")
    val before = Model(
      AppState.initial.copy(runtime =
        AppState.initial.runtime.copy(projectTasks = ProjectTasks(nextId = 1L, running = Some(running)))
      ),
      UndoState()
    )
    val program =
      for
        recorded <- recording(before)
        operations <- StateManagerOperationBoundary.create(
          recorded.modelRef,
          quietLogger,
          dictionaryCache = SharedDictionary.default
        )
        _ <- operations.modelCommit.applyResult(
          EffectResult.ProjectTaskFinished(0L, Right(ProjectTaskResult(terminalTaskCommand, 0, "done"))),
          _ => IO.unit
        )
        writes <- recorded.recordedWrites
        after  <- recorded.modelRef.get
      yield (writes, after)

    val (writes, after) = program.unsafeRunSync()

    after.app.runtime.projectTasks.terminalText shouldBe
      com.serenity.project.ProjectTaskTerminal.completed(ProjectTaskResult(terminalTaskCommand, 0, "done"))
    writes should not be empty
    all(writes.map(terminalPinned)) shouldBe false
    all(writes.map(_.undo.undoStack.isEmpty)) shouldBe true
  }
