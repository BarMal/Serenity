package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.{Deferred, IO, Ref}
import com.serenity.command.{CommandRunner, KeybindingsIntent, UiPresetsIntent}
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.{AppConfig, ConfigError, HotkeyAction, HotkeyTrigger}
import com.serenity.keystroke.events.{Event, InsertChar}
import com.serenity.rope.Balance
import com.serenity.session.{SessionManager, SessionPersistence, SessionSaveTrigger}
import com.serenity.state.effects.{Lane, LaneKey, LanePolicy}
import com.serenity.state.models.*
import com.serenity.state.reducers.EditorEventReducer
import com.serenity.state.undo.UndoState
import com.serenity.testkit.VirtualTime.runVirtual
import com.serenity.ui.layout.PanelPosition
import com.serenity.ui.presets.{UiPreset, UiPresetStore}
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** Config, UI-preset and keybinding persistence run on `EffectLanes` (#1697 Wave 3): the dispatcher commits the state
  * decision and returns, the disk work runs FIFO on its lane, and a result that has to touch state again comes back
  * through the dispatcher only while it is still current.
  */
class PersistenceEffectLanesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val quietLogger: Logger[IO] = NoOpLogger.impl[IO]
  private val paletteId               = SurfaceId("palette")

  final private class RecordingSessionPersistence(triggers: Ref[IO, List[SessionSaveTrigger]], root: Path)
      extends SessionPersistence(
        SessionManager.create(root, AppThemeManager.create, NoOpLogger.impl[IO], SessionManager.SessionPolicy()),
        SessionManager.SessionPolicy()
      ):
    override def maybeSaveSession(appState: AppState, trigger: SessionSaveTrigger): IO[Unit] =
      triggers.update(_ :+ trigger)

  /** A real operation boundary (dispatcher + lanes) over a model, with the editor port every effect family uses. */
  final private class Rig(val modelRef: Ref[IO, Model], val operations: StateManagerOperationBoundary):
    val stateRef: Ref[IO, AppState] = ModelViews.appRef(modelRef)
    private val modelCommit         = operations.modelCommit

    val editor: EffectEditorPort = new EffectEditorPort:
      def enqueueEvent(event: Event): IO[Unit] = operations.enqueueEvent(event)
      def commitState(newState: AppState, fallbackState: AppState): IO[Unit] =
        modelCommit.commitState(newState, fallbackState)
      def updateModelValidated(transition: Model => Option[Model]): IO[Unit] =
        modelCommit.updateValidated(transition)
      def updateBufferAnimations(
        update: Map[BufferId, com.serenity.animation.AnimationState] => Map[
          BufferId,
          com.serenity.animation.AnimationState
        ]
      ): IO[Unit] = modelCommit.updateBufferAnimations(update)
      def scheduleDocumentAnalysis(): IO[Unit]                     = IO.unit
      def scheduleFindSearch(request: FindSearchRequest): IO[Unit] = IO.unit
      def submitEffect(lane: Lane.Keyed, job: IO[Unit]): IO[Unit] =
        operations.submitEffect(lane, job)
      def dispatchEffectResult(result: EffectResult, onApplied: AppState => IO[Unit]): IO[Unit] =
        operations.dispatch(operations.modelCommit.applyResult(result, onApplied))

    def commit(transition: AppState => AppState): IO[Unit] =
      editor.updateModelValidated(model => Some(model.copy(app = transition(model.app))))

    /** What an editor keystroke does on the dispatcher: reduce it and commit the result through the checked path. */
    def typeOnDispatcher(char: Char): IO[Unit] =
      operations.dispatch(
        stateRef.get.flatMap(state =>
          operations.modelCommit.commitState(EditorEventReducer.reduce(InsertChar(char), PaneId(0), state).state, state)
        )
      )

  private def rig(initial: AppState = AppState.initial): IO[Rig] =
    for
      modelRef   <- Ref.of[IO, Model](Model(initial, UndoState(), Map.empty))
      operations <- StateManagerOperationBoundary.create(modelRef, quietLogger)
    yield Rig(modelRef, operations)

  private def configEffects(
    rig: Rig,
    saveConfig: (AppConfig, Path) => IO[Either[ConfigError, Unit]],
    sessionTriggers: Ref[IO, List[SessionSaveTrigger]],
    root: Path
  ): StateManagerConfigEffects =
    new StateManagerConfigEffects(
      rig.stateRef.get,
      quietLogger,
      Some(root.resolve("config.conf")),
      new RecordingSessionPersistence(sessionTriggers, root),
      _ => IO.unit,
      IO.pure(1.0),
      rig.editor,
      saveConfig
    )

  /** A writer whose first call waits for `gate`, recording each config in the order the writes land. */
  private def gatedWriter(
    gate: Deferred[IO, Unit],
    written: Ref[IO, List[AppConfig]]
  ): IO[(AppConfig, Path) => IO[Either[ConfigError, Unit]]] =
    Ref.of[IO, Int](0).map { calls => (config, _) =>
      calls.getAndUpdate(_ + 1).flatMap(call => IO.whenA(call == 0)(gate.get)) >>
        written.update(_ :+ config).as(Right(()))
    }

  private def commandPaletteState: AppState =
    AppState.initial.copy(runtime =
      AppState.initial.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            paletteId,
            SurfaceContent.CommandPalette(CommandRunner.empty),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  private def runnerStatus(state: AppState): Option[String] =
    state
      .surfaceById(paletteId)
      .map(_.content)
      .collect {
        case SurfaceContent.CommandPalette(runner) =>
          runner.statusMessage
      }
      .flatten

  private def bufferText(state: AppState): String =
    state.persisted.buffers(BufferId(0)).document.content.toString

  final private class PresetRecorders(
      val persistedConfigs: Ref[IO, List[AppConfig]],
      val pinnedDirectoryLoads: Ref[IO, List[(PanelPosition, Path)]]
  )

  private def presetEffects(
    rig: Rig,
    store: UiPresetStore,
    root: Path
  ): IO[(StateManagerUiPresetEffects, PresetRecorders)] =
    for
      persisted <- Ref.of[IO, List[AppConfig]](Nil)
      loads     <- Ref.of[IO, List[(PanelPosition, Path)]](Nil)
      triggers  <- Ref.of[IO, List[SessionSaveTrigger]](Nil)
    yield (
      new StateManagerUiPresetEffects(
        rig.stateRef.get,
        quietLogger,
        store,
        IO.pure(None),
        AppThemeManager.create,
        _ => IO.unit,
        new RecordingSessionPersistence(triggers, root),
        config => persisted.update(_ :+ config),
        StateManagerConfigEffects.withUpdatedRunnerConfig,
        IO.unit,
        (position, path) => loads.update(_ :+ (position -> path)),
        rig.commit,
        rig.editor
      ),
      PresetRecorders(persisted, loads)
    )

  /** A preset store whose `create` waits for `gate` before writing. */
  private def storeGatedOnCreate(path: Path, gate: Deferred[IO, Unit]): UiPresetStore =
    new UiPresetStore(path):
      override def create(preset: UiPreset): IO[Unit] = gate.get >> super.create(preset)

  "Config writes" should "land on disk in the order they were requested, without holding the dispatcher" in {
    val root = Files.createTempDirectory("persistence-lanes-config-fifo")
    val program =
      for
        rig      <- rig()
        gate     <- Deferred[IO, Unit]
        written  <- Ref.of[IO, List[AppConfig]](Nil)
        triggers <- Ref.of[IO, List[SessionSaveTrigger]](Nil)
        writer   <- gatedWriter(gate, written)
        config = configEffects(rig, writer, triggers, root)
        _             <- rig.operations.dispatch(config.updateConfig(_.withWheelScrollLines(7)).void)
        _             <- rig.operations.dispatch(config.updateConfig(_.withWheelScrollLines(9)).void)
        committed     <- rig.stateRef.get.map(_.persisted.config.inputConfig.wheelScrollLines)
        beforeRelease <- written.get
        _             <- gate.complete(())
        _             <- rig.operations.awaitEffects
        afterRelease  <- written.get
        saves         <- triggers.get
      yield (committed, beforeRelease, afterRelease.map(_.inputConfig.wheelScrollLines), saves)

    runVirtual(program) shouldBe (9, Nil, List(7, 9), List(SessionSaveTrigger.Manual, SessionSaveTrigger.Manual))
  }

  it should "persist a keybinding change on the config lane, in order with other config writes" in {
    val root = Files.createTempDirectory("persistence-lanes-keybinding")
    val program =
      for
        rig      <- rig()
        gate     <- Deferred[IO, Unit]
        written  <- Ref.of[IO, List[AppConfig]](Nil)
        triggers <- Ref.of[IO, List[SessionSaveTrigger]](Nil)
        writer   <- gatedWriter(gate, written)
        config      = configEffects(rig, writer, triggers, root)
        keybindings = new StateManagerKeybindingEffects(rig.stateRef.get, rig.commit, config.updateConfig)
        _ <- rig.operations.dispatch(config.updateConfig(_.withWheelScrollLines(7)).void)
        _ <- rig.operations.dispatch(
          keybindings.interpret(KeybindingsIntent.SetGlobalHotkey(HotkeyAction.Quit, "ctrl+shift+q"))
        )
        _     <- gate.complete(())
        _     <- rig.operations.awaitEffects
        after <- written.get
      yield after.map(config =>
        (config.inputConfig.wheelScrollLines, config.inputConfig.hotkeyConfig.bindingsFor(HotkeyAction.Quit))
      )

    val landed = runVirtual(program)
    landed.map(_._1) shouldBe List(7, 7)
    landed.lastOption.map(_._2) shouldBe Some(HotkeyTrigger.parse("ctrl+shift+q").toList)
  }

  "A slow preset write" should "leave the dispatcher free for an editor keystroke dispatched meanwhile" in {
    val root = Files.createTempDirectory("persistence-lanes-preset-slow")
    val program =
      for
        rig  <- rig(commandPaletteState)
        gate <- Deferred[IO, Unit]
        store = storeGatedOnCreate(root.resolve("ui-presets.json"), gate)
        (presets, _) <- presetEffects(rig, store, root)
        _            <- rig.operations.dispatch(presets.interpret(UiPresetsIntent.SaveUiPresetAsNew("Slow")))
        _            <- rig.typeOnDispatcher('x')
        typed        <- rig.stateRef.get.map(bufferText)
        savedBefore  <- store.list().map(_.map(_.name))
        statusBefore <- rig.stateRef.get.map(runnerStatus)
        _            <- gate.complete(())
        _            <- rig.operations.awaitEffects
        savedAfter   <- store.list().map(_.map(_.name))
        statusAfter  <- rig.stateRef.get.map(runnerStatus)
      yield (typed, savedBefore, statusBefore, savedAfter, statusAfter)

    runVirtual(program) shouldBe ("x", Nil, None, List("Slow"), Some("Preset saved. Configure Slow."))
  }

  "A failing preset write" should "surface the same error on the command runner as before" in {
    val root = Files.createTempDirectory("persistence-lanes-preset-failure")
    val failingStore = new UiPresetStore(root.resolve("ui-presets.json")):
      override def create(preset: UiPreset): IO[Unit] = IO.raiseError(new java.io.IOException("disk full"))
    val program =
      for
        rig          <- rig(commandPaletteState)
        (presets, _) <- presetEffects(rig, failingStore, root)
        _            <- rig.operations.dispatch(presets.interpret(UiPresetsIntent.SaveUiPresetAsNew("Doomed")))
        _            <- rig.operations.awaitEffects
        status       <- rig.stateRef.get.map(runnerStatus)
      yield status

    runVirtual(program) shouldBe Some("Could not save Doomed: disk full")
  }

  "A preset apply result" should "be dropped once a later apply has been requested" in {
    val root = Files.createTempDirectory("persistence-lanes-preset-stale")
    val program =
      for
        rig   <- rig(commandPaletteState)
        gate  <- Deferred[IO, Unit]
        finds <- Ref.of[IO, Int](0)
        store = new UiPresetStore(root.resolve("ui-presets.json")):
          override def find(name: String): IO[Option[UiPreset]] =
            finds.getAndUpdate(_ + 1).flatMap(call => IO.whenA(call == 0)(gate.get)) >> super.find(name)
        (presets, recorders) <- presetEffects(rig, store, root)
        _                    <- rig.operations.dispatch(presets.interpret(UiPresetsIntent.ApplyUiPreset("Writing")))
        _                    <- rig.operations.dispatch(presets.interpret(UiPresetsIntent.ApplyUiPreset("Code")))
        _                    <- gate.complete(())
        _                    <- rig.operations.awaitEffects
        persisted            <- recorders.persistedConfigs.get
        loads                <- recorders.pinnedDirectoryLoads.get
        after                <- rig.stateRef.get
      yield (persisted.size, loads, after.runtime.pendingUiPresetApply)

    runVirtual(program) shouldBe (1, List(PanelPosition.Left -> Path.of(".")), None)
  }

  "Shutting down effects" should "finish at once when no persistence work is pending" in {
    val program =
      for
        rig     <- rig()
        started <- IO.monotonic
        _       <- rig.operations.shutdownEffects()
        _       <- rig.operations.shutdownEffects()
        ended   <- IO.monotonic
      yield ended - started

    runVirtual(program) shouldBe Duration.Zero
  }

  it should "cancel switch-latest work rather than wait for it" in {
    val program =
      for
        rig     <- rig()
        _       <- rig.operations.effectLanes.submit(Lane.Keyed(LaneKey.Search, LanePolicy.SwitchLatest), IO.never)
        started <- IO.monotonic
        _       <- rig.operations.shutdownEffects()
        ended   <- IO.monotonic
      yield ended - started

    runVirtual(program) shouldBe Duration.Zero
  }

  it should "let a pending config write finish before releasing the lanes" in {
    val root = Files.createTempDirectory("persistence-lanes-shutdown-drain")
    val program =
      for
        rig      <- rig()
        gate     <- Deferred[IO, Unit]
        written  <- Ref.of[IO, List[AppConfig]](Nil)
        triggers <- Ref.of[IO, List[SessionSaveTrigger]](Nil)
        writer   <- gatedWriter(gate, written)
        config = configEffects(rig, writer, triggers, root)
        _        <- rig.operations.dispatch(config.updateConfig(_.withWheelScrollLines(7)).void)
        shutdown <- rig.operations.shutdownEffects().start
        _        <- IO.sleep(1.second)
        finished <- shutdown.join.map(_ => true).timeoutTo(1.milli, IO.pure(false))
        _        <- gate.complete(())
        _        <- shutdown.joinWithNever
        landed   <- written.get
      yield (finished, landed.map(_.inputConfig.wheelScrollLines))

    runVirtual(program) shouldBe (false, List(7))
  }

  it should "give up on a write that never finishes after the grace period" in {
    val root = Files.createTempDirectory("persistence-lanes-shutdown-grace")
    val program =
      for
        rig      <- rig()
        gate     <- Deferred[IO, Unit]
        written  <- Ref.of[IO, List[AppConfig]](Nil)
        triggers <- Ref.of[IO, List[SessionSaveTrigger]](Nil)
        writer   <- gatedWriter(gate, written)
        config = configEffects(rig, writer, triggers, root)
        _      <- rig.operations.dispatch(config.updateConfig(_.withWheelScrollLines(7)).void)
        _      <- rig.operations.shutdownEffects()
        landed <- written.get
        late   <- rig.operations.submitEffect(PersistenceLanes.Config, written.update(_ :+ AppConfig.default)).attempt
      yield (landed, late)

    runVirtual(program) shouldBe (Nil, Right(()))
  }
