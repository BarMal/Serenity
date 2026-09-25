package com.serenity.state.manager

import java.awt.Color
import java.nio.file.Files

import cats.data.State
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.StateManagerTestFixtures
import com.serenity.animation.AnimationState
import com.serenity.animation.sprite.CompanionSpriteState
import com.serenity.command.{
  Command,
  CommandCategory,
  CommandIntent,
  FileIntent,
  MotionIntent,
  PanelKind,
  SessionIntent,
  SettingsIntent,
  ViewIntent
}
import com.serenity.config.{AppConfig, MotionAccessibility, PreferredWindowSize}
import com.serenity.keystroke.events.{Enter, InsertChar, NextTab, TabKey, ToggleCommandRunner, Undo}
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.*
import com.serenity.state.undo.{HistoryEntry, UndoState}
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{PanelContent, PanelPosition}
import com.serenity.ui.presets.{UiPreset, UiPresetStore}
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** The dispatcher-owned model (#1697 F3): app state, undo history and buffer animations live in one `Ref`, so an
  * operation that changes several of them commits them together.
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
        fileDialog = None
      )
      stateManager <- StateManager.fromRuntime(runtime)
    yield stateManager

  private val animatedInitial: Model =
    val app = AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.withTestAnimations))
    Model(app, UndoState(), Map.empty)

  /** A fresh buffer holding "Hello", focused in the first pane with the cursor at its end. */
  private def focusedHelloBuffer(stateManager: StateManager): IO[BufferId] =
    for
      bufferId <- stateManager.bufferManager.createBuffer("Hello", None)
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
    model.undo.pendingGroup.nonEmpty || model.undo.undoStack.nonEmpty

  private def hasAnimations(model: Model, bufferId: BufferId): Boolean =
    model.bufferAnimations.get(bufferId).exists(_.hasActiveAnimations)

  "An edit" should "commit its text, its undo entry and its character animation in one write" in {
    val program =
      for
        recorded     <- recording(animatedInitial)
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
    hasAnimations(after, bufferId) shouldBe true
    writes should not be empty
    all(writes.map(model => content(model, bufferId).contains("Helloa") == hasUndoHistory(model))) shouldBe true
    all(
      writes.map(model => content(model, bufferId).contains("Helloa") == hasAnimations(model, bufferId))
    ) shouldBe true
  }

  "Undo" should "restore the text and move its entry to the redo stack in one write" in {
    val program =
      for
        recorded     <- recording(animatedInitial)
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

  "Disabling motion" should "cancel buffer animations and in-flight app motion in one write" in {
    val bufferId = BufferId(0)
    val typing = AppState.initial.copy(runtime =
      AppState.initial.runtime.copy(companionSprite = CompanionSpriteState.default.observeTyping(1_000_000_000L))
    )
    val animations =
      Map(bufferId -> AnimationState.empty.addCharacterAnimation('a', 0, 0, Color.BLACK, Color.WHITE, 5))
    val disableMotion = Command.typed(
      "disable-motion",
      "Disable motion",
      CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionAccessibility(MotionAccessibility.Off))),
      CommandCategory.Settings
    )
    val program =
      for
        recorded     <- recording(Model(typing, UndoState(), animations))
        stateManager <- stateManagerOver(recorded.modelRef)
        _            <- stateManager.commandExecutor.executeCommand(disableMotion)
        writes       <- recorded.recordedWrites
        after        <- stateManager.getModel
      yield (writes, after)

    val (writes, after) = program.unsafeRunSync()

    after.app.runtime.companionSprite.isTypingActive shouldBe false
    hasAnimations(after, bufferId) shouldBe false
    writes should not be empty
    all(
      writes.map(model => model.app.runtime.companionSprite.isTypingActive == hasAnimations(model, bufferId))
    ) shouldBe true
  }

  "A validated model write" should "leave every part of the model unchanged when the app state it carries is invalid" in {
    val animations =
      Map(BufferId(0) -> AnimationState.empty.addCharacterAnimation('a', 0, 0, Color.BLACK, Color.WHITE, 5))
    val before = Model(AppState.initial, UndoState(), animations)
    val invalid =
      AppState.initial.copy(persisted = AppState.initial.persisted.copy(focus = Focus.EditorPane(PaneId(999))))
    val program =
      for
        recorded   <- recording(before)
        operations <- StateManagerOperationBoundary.create(recorded.modelRef, quietLogger)
        commit = operations.modelCommit
        _     <- commit.updateValidated(_ => Some(Model(invalid, UndoState(maxUndoDepth = 3), Map.empty)))
        after <- recorded.modelRef.get
      yield after

    program.unsafeRunSync() shouldBe before
  }

  "Cycling to the next tab" should "commit the buffer switch and its pane-flow sweep in one write" in {
    val program =
      for
        recorded     <- recording(animatedInitial)
        stateManager <- stateManagerOver(recorded.modelRef)
        _            <- focusedHelloBuffer(stateManager)
        nextBufferId <- stateManager.bufferManager.createBuffer("World", None)
        _            <- recorded.clear
        _            <- stateManager.applyEvent(NextTab)
        writes       <- recorded.recordedWrites
        after        <- stateManager.getModel
      yield (nextBufferId, writes, after)

    val (nextBufferId, writes, after) = program.unsafeRunSync()

    def showsNext(model: Model): Boolean = model.app.focusedBufferId.contains(nextBufferId)

    showsNext(after) shouldBe true
    hasAnimations(after, nextBufferId) shouldBe true
    writes should not be empty
    all(writes.map(model => showsNext(model) == hasAnimations(model, nextBufferId))) shouldBe true
  }

  private def viewCommand(intent: ViewIntent): Command =
    Command.typed("panel-pin", "Pin panel", CommandIntent.View(intent), CommandCategory.View)

  private def diagnosticsPinned(model: Model): Boolean =
    model.app.runtime.uiSurfaces.exists(_.content == SurfaceContent.Diagnostics(Nil))

  "Pinning a panel" should "commit the panel and its undo boundary in one write" in {
    val program =
      for
        recorded     <- recording(Model(AppState.initial, UndoState(), Map.empty))
        stateManager <- stateManagerOver(recorded.modelRef)
        _            <- stateManager.commandExecutor.executeCommand(viewCommand(ViewIntent.PinDiagnosticsPanel))
        writes       <- recorded.recordedWrites
        after        <- stateManager.getModel
      yield (writes, after)

    val (writes, after) = program.unsafeRunSync()

    diagnosticsPinned(after) shouldBe true
    after.undo.undoStack shouldBe Vector(HistoryEntry.PanelChange.capture(AppState.initial))
    all(writes.map(model => diagnosticsPinned(model) == model.undo.undoStack.nonEmpty)) shouldBe true
  }

  "Pinning a panel through the panel manager" should "commit the panel and its undo boundary in one write" in {
    val program =
      for
        recorded     <- recording(Model(AppState.initial, UndoState(), Map.empty))
        stateManager <- stateManagerOver(recorded.modelRef)
        _            <- stateManager.panelManager.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Right, 30)
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
    val unpin = ViewIntent.SetPanelPin(PanelKind.Diagnostics, None)
    val program =
      for
        recorded     <- recording(Model(AppState.initial, UndoState(), Map.empty))
        stateManager <- stateManagerOver(recorded.modelRef)
        _            <- stateManager.commandExecutor.executeCommand(viewCommand(ViewIntent.PinDiagnosticsPanel))
        _            <- recorded.clear
        _            <- stateManager.commandExecutor.executeCommand(viewCommand(unpin))
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
    model.app.runtime.modalStack.map(_.modal).collectFirst {
      case Modal.CloseWorkflow(workflow) =>
        workflow.currentBufferId
    }

  "Discarding one buffer of a close-all" should "close it and prompt for the next in one write" in {
    val closeAll =
      Command.typed("close-all", "Close all", CommandIntent.File(FileIntent.CloseAll), CommandCategory.File)
    val program =
      for
        recorded     <- recording(Model(AppState.initial, UndoState(), Map.empty))
        stateManager <- stateManagerOver(recorded.modelRef)
        first        <- focusedHelloBuffer(stateManager)
        second       <- stateManager.bufferManager.createBuffer("World", None)
        _            <- stateManager.commandExecutor.executeCommand(closeAll)
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
        recorded     <- recording(Model(AppState.initial, UndoState(), Map.empty))
        stateManager <- stateManagerOver(recorded.modelRef)
        _            <- stateManager.commandExecutor.executeCommand(restore)
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

  "Opening the command runner" should "commit the runner and its UI preset previews in one write" in {
    val program =
      for
        directory <- IO.blocking(Files.createTempDirectory("model-atomicity-presets"))
        store = UiPresetStore(directory.resolve("presets.json"))
        _            <- store.create(UiPreset.capture("Existing", AppState.initial, None))
        recorded     <- recording(Model(AppState.initial, UndoState(), Map.empty))
        stateManager <- stateManagerOver(recorded.modelRef, Some(store))
        _            <- stateManager.applyEvent(ToggleCommandRunner)
        writes       <- recorded.recordedWrites
        after        <- stateManager.getModel
      yield (writes, after)

    val (writes, after) = program.unsafeRunSync()

    runnerPresetPreviews(after).map(_.map(_.name)) shouldBe Some(List("Existing"))
    writes should not be empty
    all(writes.flatMap(runnerPresetPreviews).map(_.nonEmpty)) shouldBe true
  }

  private def replace(stateManager: StateManager, action: ReplaceWorkflowAction): IO[Unit] =
    stateManager.modalService.showModal(
      Modal.ReplaceWorkflow(ReplaceWorkflowState(findText = "l", replacementText = "L", selectedAction = action))
    ) >> stateManager.applyEvent(Enter)

  private def replacePrompt(model: Model): Option[ReplaceWorkflowState] =
    model.app.runtime.uiSurfaces.map(_.content).collectFirst {
      case SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)) => workflow
    }

  "Replace all" should "commit the replaced text and its undo entry in one write" in {
    val program =
      for
        recorded     <- recording(Model(AppState.initial, UndoState(), Map.empty))
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
        recorded     <- recording(Model(AppState.initial, UndoState(), Map.empty))
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
