package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*
import scala.util.Random

import cats.effect.std.Semaphore
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, Fiber, IO, Ref}
import cats.syntax.all.*
import com.serenity.animation.AnimationState
import com.serenity.config.PreferredWindowSize
import com.serenity.io.{DocumentRevision, FileManager}
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.*
import com.serenity.state.reducers.AppEffect
import com.serenity.state.undo.UndoState
import com.serenity.testkit.VirtualTime.runVirtual
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** The single-writer inbox (#1697 slice F1): background results, the external-change check and the render tick must
  * never write state in the middle of an event dispatch.
  *
  * The pipeline specs block a dispatch inside an effect that, like several real handlers, commits a state snapshot it
  * read before its I/O. Anything written to the state while that effect is blocked is either lost to the commit or
  * interleaved with a dispatch that has not finished yet.
  */
class StateManagerDispatchInboxSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val quietLogger: Logger[IO] = NoOpLogger.impl[IO]
  private val bufferId                = BufferId(0)

  final private case class PipelineHarness(
      stateRef: Ref[IO, AppState],
      operations: StateManagerOperationBoundary,
      pipeline: StateManagerEventPipeline,
      ticker: AnimationTicker
  )

  final private case class DispatchGate(entered: Deferred[IO, Unit], release: Deferred[IO, Unit])

  private def newGate: IO[DispatchGate] =
    (Deferred[IO, Unit], Deferred[IO, Unit]).tupled.map(DispatchGate.apply)

  /** Every interpreted effect reads the state, waits on `gate`, then commits what it read. */
  private def snapshotCommittingPipeline(initialState: AppState, gate: DispatchGate): IO[PipelineHarness] =
    for
      sharedStateRef   <- Ref.of[IO, AppState](initialState)
      fiberRef         <- Ref.of[IO, Option[Fiber[IO, Throwable, Unit]]](None)
      cacheRef         <- Ref.of[IO, Option[MouseTargetCache]](None)
      bufferAnimations <- Ref.of[IO, Map[BufferId, AnimationState]](Map.empty)
      sharedUndoRef    <- Ref.of[IO, UndoState](UndoState())
      lspQueue         <- LspEffectQueue.create
      operations       <- StateManagerOperationBoundary.create(sharedStateRef, fiberRef, quietLogger)
      statePort = new EventStatePort:
        val stateRef                 = sharedStateRef
        val logger                   = quietLogger
        val documentAnalysisFiberRef = fiberRef
        val mouseTargetCacheRef      = cacheRef
        val bufferAnimationsRef      = bufferAnimations
      snapshotCommittingEffect = (_: AppEffect) =>
        sharedStateRef.get.flatMap { snapshot =>
          gate.entered.complete(()) >> gate.release.get >> operations.validateAndUpdateState(snapshot, snapshot)
        }
      effectPort = EventEffectPort(
        interpretEffect = snapshotCommittingEffect,
        interpretCommand = (_, _) => IO.unit
      )
      workflowPort = new EventWorkflowPort:
        def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit]      = IO.unit
        def createBuffer(content: String, filePath: Option[Path]): IO[BufferId] = IO.pure(BufferId(0))
        def createPane(bufferId: Option[BufferId]): IO[PaneId]                  = IO.pure(PaneId(0))
      undoRecording = new UndoRecording(new UndoRecordingPort:
        val stateRef = sharedStateRef; val undoRef = sharedUndoRef
        export operations.validateAndUpdateState)
      pipeline = new StateManagerEventPipeline(
        statePort,
        effectPort,
        workflowPort,
        UiPresetStore(Path.of("target", "state-manager-dispatch-inbox-spec.json")),
        update =>
          sharedStateRef.modify(state =>
            val config = update(state.persisted.config)
            (state.copy(persisted = state.persisted.copy(config = config)), config)
          ),
        (_, _) => IO.unit,
        operations,
        undoRecording
      )
      animations = new AnimationChoreography(new AnimationChoreographyPort:
        val stateRef            = sharedStateRef
        val bufferAnimationsRef = bufferAnimations
        export operations.validateAndUpdateState)
      editor = new StateManagerEditorCapability(
        sharedStateRef,
        lspQueue,
        bufferAnimations,
        animations,
        operations,
        new Random(0L)
      )
    yield PipelineHarness(sharedStateRef, operations, pipeline, editor.animationTicker)

  "The state dispatcher" should "apply a background result offered mid-dispatch after that dispatch instead of losing it" in {
    val program =
      for
        gate     <- newGate
        harness  <- snapshotCommittingPipeline(AppState.initial, gate)
        dispatch <- harness.pipeline.applyEvent(FileSearch).start
        _        <- gate.entered.get
        _        <- harness.operations.scheduleMarkdownPreviewCommit(bufferId, 7L)
        _        <- IO.sleep(1.second)
        _        <- gate.release.complete(())
        _        <- dispatch.joinWithNever
        _        <- IO.sleep(1.second)
        after    <- harness.stateRef.get
      yield after.persisted.buffers.get(bufferId).map(_.markdownPreviewCommittedGeneration)

    runVirtual(program) shouldBe Some(7L)
  }

  it should "skip a render tick that arrives mid-dispatch, report it still active, and advance on the next one" in {
    val typing = AppState.initial.copy(runtime =
      AppState.initial.runtime.copy(typingActivity = AppState.initial.runtime.typingActivity.observed)
    )
    val program =
      for
        gate        <- newGate
        harness     <- snapshotCommittingPipeline(typing, gate)
        dispatch    <- harness.pipeline.applyEvent(FileSearch).start
        _           <- gate.entered.get
        beforeTick  <- harness.stateRef.get
        stillActive <- harness.ticker.advanceAnimationsOnTick.timeout(10.seconds)
        midDispatch <- harness.stateRef.get
        _           <- gate.release.complete(())
        _           <- dispatch.joinWithNever
        committed   <- harness.stateRef.get
        _           <- harness.ticker.advanceAnimationsOnTick
        nextFrame   <- harness.stateRef.get
      yield
        stillActive shouldBe true
        midDispatch.runtime.typingActivity shouldBe beforeTick.runtime.typingActivity
        nextFrame.runtime.typingActivity shouldBe committed.runtime.typingActivity.advance

    runVirtual(program)
  }

  /** Saves the buffer to disk for real, then holds the save open -- after the disk write, before the state records the
    * new revision -- until `release` completes: exactly the window the #1623 watcher used to observe.
    */
  final private class GatedSaveFileManager(
      written: Deferred[IO, Unit],
      release: Deferred[IO, Unit],
      revisionRead: Deferred[IO, Unit]
  ) extends FileManager:
    override def saveBuffer(buffer: Buffer): IO[Buffer] =
      super.saveBuffer(buffer) <* (written.complete(()) >> release.get)

    override def currentRevision(path: Path): IO[Option[DocumentRevision]] =
      super.currentRevision(path) <* revisionRead.complete(())

  final private case class SaveRace(
      stateManager: StateManager,
      bufferId: BufferId,
      written: Deferred[IO, Unit],
      release: Deferred[IO, Unit],
      revisionRead: Deferred[IO, Unit]
  )

  private def dirtyBufferWithGatedSave: IO[SaveRace] =
    for
      written      <- Deferred[IO, Unit]
      release      <- Deferred[IO, Unit]
      revisionRead <- Deferred[IO, Unit]
      directory    <- IO.blocking(Files.createTempDirectory("state-manager-dispatch-inbox-spec"))
      file = directory.resolve("notes.txt")
      _                        <- IO.blocking(Files.writeString(file, "draft"))
      stateRef                 <- Ref.of[IO, AppState](AppState.initial)
      undoRef                  <- Ref.of[IO, UndoState](UndoState())
      themeNamesRef            <- Ref.of[IO, List[String]](Nil)
      quitSignal               <- Deferred[IO, Unit]
      lspQueue                 <- LspEffectQueue.create
      projectTaskFiberRef      <- Ref.of[IO, Option[ManagedProjectTask]](None)
      projectTaskSemaphore     <- Semaphore[IO](1)
      mouseTargetCacheRef      <- Ref.of[IO, Option[MouseTargetCache]](None)
      documentAnalysisFiberRef <- Ref.of[IO, Option[Fiber[IO, Throwable, Unit]]](None)
      bufferAnimationsRef      <- Ref.of[IO, Map[BufferId, AnimationState]](Map.empty)
      runtime = StateManagerRuntime
        .create(
          stateRef = stateRef,
          undoRef = undoRef,
          themeNamesRef = themeNamesRef,
          quitSignal = quitSignal,
          logger = quietLogger,
          policy = SessionManager.SessionPolicy(),
          sessionRootOverride = Some(directory.resolve("session")),
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
          uiPresetStore = UiPresetStore(directory.resolve("presets.json")),
          windowSizeProvider = IO.pure(None),
          onPreferredWindowSizeChanged = (_: PreferredWindowSize) => IO.unit,
          fileDialog = None
        )
        .copy(fileManager = new GatedSaveFileManager(written, release, revisionRead))
      stateManager <- StateManager.fromRuntime(runtime)
      _            <- stateManager.fileOpener.openFile(file)
      _            <- stateManager.applyEvent(InsertChar('!'))
      opened       <- stateManager.getCurrentState
      openedId <- IO.fromOption(opened.focusedBufferId)(new IllegalStateException("opening the file focused no buffer"))
      _ <- IO.raiseUnless(opened.persisted.buffers.get(openedId).exists(_.hasUnsavedChanges))(
        new IllegalStateException("typing into the opened file did not dirty it")
      )
    yield SaveRace(stateManager, openedId, written, release, revisionRead)

  private def raceExternalChangeCheckAgainstSave(check: SaveRace => IO[Unit]): AppState =
    val program =
      for
        race  <- dirtyBufferWithGatedSave
        save  <- race.stateManager.applyEvent(SaveFile).start
        _     <- race.written.get
        probe <- check(race).start
        _     <- race.revisionRead.get
        _     <- race.release.complete(())
        _     <- save.joinWithNever
        _     <- probe.joinWithNever
        after <- race.stateManager.getCurrentState
      yield after
    program.timeout(30.seconds).unsafeRunSync()

  private def assertCleanWithoutConflict(after: AppState): Unit =
    after.runtime.modalStack shouldBe empty
    after.focusedBufferId.flatMap(after.persisted.buffers.get).map(_.hasUnsavedChanges) shouldBe Some(false)

  "The #1623 watcher check" should "not open a reload conflict for a save that has written the disk but not committed" in
    assertCleanWithoutConflict(
      raceExternalChangeCheckAgainstSave(race =>
        race.stateManager.fileService.checkBufferForExternalChanges(race.bufferId)
      )
    )

  "The #1623 focus-in check" should "not open a reload conflict for a save that has written the disk but not committed" in
    assertCleanWithoutConflict(
      raceExternalChangeCheckAgainstSave(race => race.stateManager.fileService.checkExternalChangesOnFocus)
    )
