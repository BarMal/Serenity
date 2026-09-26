package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import com.serenity.command.{Command, CommandCategory, CommandIntent, EditIntent}
import com.serenity.config.{AppConfig, MarkdownViewMode, PreferredWindowSize}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.session.SessionManager
import com.serenity.state.components.ComponentResult
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.state.reducers.ReducerResult
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{PanelContent, PanelPosition}
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** Every write to the model goes through `AppStateValidation` (#1697 Wave 4): a write whose resulting state is invalid
  * is rejected and the state before it is kept.
  *
  * Each scenario starts from a state that is already invalid -- its buffer order names a buffer that does not exist --
  * seeded into the model the state manager is built over, so any write that lands on it produces an invalid state too.
  * A validated write is therefore rejected, and only a write that bypasses validation changes anything.
  */
class ValidatedWritesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val quietLogger: Logger[IO] = NoOpLogger.impl[IO]
  private val bufferId                = BufferId(0)

  private def withStaleBufferOrder(state: AppState): AppState =
    state.copy(persisted = state.persisted.copy(bufferOrder = state.persisted.bufferOrder :+ BufferId(999)))

  private def isInvalid(state: AppState): Boolean = AppStateValidation.validationErrors(state).nonEmpty

  private def stateManagerOver(modelRef: Ref[IO, Model]): IO[StateManager] =
    for
      directory           <- IO.blocking(Files.createTempDirectory("validated-writes-spec"))
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
        uiPresetStore = UiPresetStore(directory.resolve("presets.json")),
        windowSizeProvider = IO.pure(None),
        onPreferredWindowSizeChanged = (_: PreferredWindowSize) => IO.unit,
        fileDialog = None
      )
      stateManager <- StateManager.fromRuntime(runtime)
    yield stateManager

  /** A state manager over `initial` made invalid, seeded at construction since every write is validated. */
  private def invalidStateManager(initial: AppState = AppState.initial): IO[(StateManager, Ref[IO, Model], AppState)] =
    for
      modelRef     <- Ref.of[IO, Model](Model(withStaleBufferOrder(initial), UndoState(), Map.empty))
      stateManager <- stateManagerOver(modelRef)
      seeded       <- stateManager.getCurrentState
    yield (stateManager, modelRef, seeded)

  "Pinning a panel through the panel manager" should "record no undo boundary when its state is rejected" in {
    val program =
      for
        (stateManager, _, seeded) <- invalidStateManager()
        _                         <- stateManager.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Right, 30)
        after                     <- stateManager.getModel
      yield (seeded, after)

    val (seeded, after) = program.unsafeRunSync()

    after.app shouldBe seeded
    after.undo.undoStack shouldBe empty
  }

  private def edit(intent: EditIntent): Command =
    Command.typed("edit", "Edit", CommandIntent.Edit(intent), CommandCategory.Edit)

  "An edit command that opens the find or replace prompt" should "keep the previous state when the result is invalid" in {
    val intents = List(
      EditIntent.FindInCurrentFile,
      EditIntent.FindAllInCurrentFile,
      EditIntent.ReplaceInCurrentFile,
      EditIntent.ReplaceAllInCurrentFile
    )
    val program = intents.traverse { intent =>
      for
        (stateManager, _, seeded) <- invalidStateManager()
        _                         <- stateManager.executeCommand(edit(intent))
        after                     <- stateManager.getCurrentState
      yield (seeded, after)
    }

    program.unsafeRunSync().foreach((seeded, after) => after shouldBe seeded)
  }

  private val dirty: AppState => AppState = state =>
    val buffer = state.persisted.buffers(bufferId)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(bufferId, buffer.copy(document = buffer.document.copy(isDirty = true)))
      )
    )

  "Marking a buffer saved" should "keep the previous state when the result is invalid" in {
    val program =
      for
        (stateManager, _, _) <- invalidStateManager(dirty(AppState.initial))
        seeded               <- stateManager.getCurrentState
        _                    <- stateManager.markBufferSaved(bufferId)
        after                <- stateManager.getCurrentState
      yield (seeded, after)

    val (seeded, after) = program.unsafeRunSync()

    after shouldBe seeded
    after.persisted.buffers(bufferId).document.isDirty shouldBe true
  }

  "Setting a buffer's file path" should "keep the previous state when the result is invalid" in {
    val program =
      for
        (stateManager, _, seeded) <- invalidStateManager()
        _                         <- stateManager.setBufferFilePath(bufferId, Path.of("notes.md"))
        after                     <- stateManager.getCurrentState
      yield (seeded, after)

    val (seeded, after) = program.unsafeRunSync()

    after shouldBe seeded
    after.persisted.buffers(bufferId).document.filePath shouldBe None
  }

  private def withGenerations(edit: Long, committed: Long): AppState => AppState = state =>
    val buffer = state.persisted.buffers(bufferId)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(
          bufferId,
          buffer.copy(markdownPreviewEditGeneration = edit, markdownPreviewCommittedGeneration = committed)
        )
      )
    )

  "A settled markdown-preview commit" should "keep the previous state when the result is invalid" in {
    val program =
      for
        modelRef <- ModelViews.modelOf(
          withStaleBufferOrder(withGenerations(edit = 1, committed = 0)(AppState.initial))
        )
        stateRef = ModelViews.appRef(modelRef)
        seeded     <- stateRef.get
        operations <- StateManagerOperationBoundary.create(modelRef, quietLogger)
        _          <- operations.scheduleMarkdownPreviewCommit(bufferId, 1)
        _          <- operations.awaitEffects
        after      <- stateRef.get
      yield (seeded, after)

    val (seeded, after) = program.unsafeRunSync()

    after shouldBe seeded
    after.persisted.buffers(bufferId).markdownPreviewCommittedGeneration shouldBe 0
  }

  // ---- Event pipeline writes, driven on a pipeline built over the model directly. ----

  final private case class PipelineRig(
      modelRef: Ref[IO, Model],
      pipeline: StateManagerEventPipeline,
      statesSeenByCommands: Ref[IO, List[AppState]]
  ):
    val stateRef: Ref[IO, AppState] = ModelViews.appRef(modelRef)

  private def pipelineOver(initial: AppState): IO[PipelineRig] =
    for
      model      <- Ref.of[IO, Model](Model(initial, UndoState(), Map.empty))
      seen       <- Ref.of[IO, List[AppState]](Nil)
      cacheRef   <- Ref.of[IO, Option[MouseTargetCache]](None)
      operations <- StateManagerOperationBoundary.create(model, quietLogger)
      directory  <- IO.blocking(Files.createTempDirectory("validated-writes-pipeline"))
    yield
      val stateRef = ModelViews.appRef(model)
      val statePort = new EventStatePort:
        val logger              = quietLogger
        val mouseTargetCacheRef = cacheRef
      val effectPort = EventEffectPort(
        interpretEffect = _ => IO.unit,
        interpretCommand = (_, _) => stateRef.get.flatMap(current => seen.update(_ :+ current))
      )
      val workflowPort = new EventWorkflowPort:
        def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit] = IO.unit
      val undoRecording = new UndoRecording(new UndoRecordingPort:
        def updateUndo(update: UndoState => UndoState): IO[Unit] = ModelViews.undoRef(model).update(update)
        def updateModelValidated(transition: Model => Option[Model]): IO[Unit] =
          operations.modelCommit.updateValidated(transition))
      val pipeline = new StateManagerEventPipeline(
        statePort,
        effectPort,
        workflowPort,
        UiPresetStore(directory.resolve("presets.json")),
        update => stateRef.get.map(state => update(state.persisted.config)),
        (_, _) => IO.unit,
        operations,
        undoRecording
      )
      PipelineRig(model, pipeline, seen)

  private val invalidate: AppState => AppState = withStaleBufferOrder

  "A component result that runs a command" should "never commit an invalid state for the command to see" in {
    val command = edit(EditIntent.FindInCurrentFile)
    val program =
      for
        rig    <- pipelineOver(AppState.initial)
        before <- rig.stateRef.get
        _ <- rig.pipeline.applyComponentResult(
          ComponentResult.composite(ComponentResult.updateState(invalidate), ComponentResult.executeCommand(command)),
          before
        )
        seen  <- rig.statesSeenByCommands.get
        after <- rig.stateRef.get
      yield (before, seen, after)

    val (before, seen, after) = program.unsafeRunSync()

    seen shouldBe List(before)
    after shouldBe before
  }

  "A rejected reducer update inside a component result" should "keep the committed state, not the uncommitted one" in {
    val program =
      for
        rig    <- pipelineOver(AppState.initial)
        before <- rig.stateRef.get
        _ <- rig.pipeline.applyComponentResult(
          ComponentResult.composite(
            ComponentResult.updateState(invalidate),
            ComponentResult.reducerResult(ReducerResult.noEffects(invalidate(before)))
          ),
          before
        )
        after <- rig.stateRef.get
      yield (before, after)

    val (before, after) = program.unsafeRunSync()

    isInvalid(after) shouldBe false
    after shouldBe before
  }

  private val noEditorPanes: AppState =
    val surfaceId = SurfaceId("surface-7")
    val surface = UiSurface(
      surfaceId,
      SurfaceContent.FileSearch(FileSearchState("", Nil, 0)),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    AppState.empty.copy(
      persisted = AppState.empty.persisted.copy(focus = Focus.Surface(surfaceId)),
      runtime = AppState.empty.runtime.copy(uiSurfaces = List(surface))
    )

  "Dismissing the last surface with no editor pane" should "build its fresh buffer and pane without writing state" in {
    val program =
      for
        rig       <- pipelineOver(noEditorPanes)
        before    <- rig.stateRef.get
        dismissed <- rig.pipeline.applyComponentResult(ComponentResult.dismiss, before)
        after     <- rig.stateRef.get
      yield (before, dismissed, after)

    val (before, dismissed, after) = program.unsafeRunSync()

    after shouldBe before
    dismissed.runtime.uiSurfaces shouldBe empty
    val panes = dismissed.persisted.layout.editorPanes
    panes should have size 1
    dismissed.persisted.focus shouldBe Focus.EditorPane(panes.keys.head)
    panes.values.head.bufferId.flatMap(dismissed.persisted.buffers.get) should not be empty
    isInvalid(dismissed) shouldBe false
  }

  private val markdownWithInlinePreview: AppState =
    val initial = AppState.initial(AppConfig.default.withMarkdownViewMode(MarkdownViewMode.InlineLens))
    val buffer  = initial.persisted.buffers(bufferId)
    initial.copy(persisted =
      initial.persisted.copy(buffers =
        initial.persisted.buffers.updated(
          bufferId,
          buffer.copy(document = buffer.document.copy(language = Some(LanguageId.Markdown), content = Rope("edited")))
        )
      )
    )

  "The markdown-preview edit-generation bump" should "keep the previous state when the result is invalid" in {
    val previous = markdownWithInlinePreview.copy(persisted =
      markdownWithInlinePreview.persisted.copy(buffers =
        markdownWithInlinePreview.persisted.buffers.updatedWith(bufferId)(
          _.map(buffer => buffer.copy(document = buffer.document.copy(content = Rope("original"))))
        )
      )
    )
    val program =
      for
        rig    <- pipelineOver(withStaleBufferOrder(markdownWithInlinePreview))
        seeded <- rig.stateRef.get
        _      <- rig.pipeline.scheduleMarkdownPreviewCommits(previous)
        after  <- rig.stateRef.get
      yield (seeded, after)

    val (seeded, after) = program.unsafeRunSync()

    after shouldBe seeded
    after.persisted.buffers(bufferId).markdownPreviewEditGeneration shouldBe 0
  }

  it should "bump the generation of an edited buffer with a live preview" in {
    val previous = markdownWithInlinePreview.copy(persisted =
      markdownWithInlinePreview.persisted.copy(buffers =
        markdownWithInlinePreview.persisted.buffers.updatedWith(bufferId)(
          _.map(buffer => buffer.copy(document = buffer.document.copy(content = Rope("original"))))
        )
      )
    )
    val program =
      for
        rig   <- pipelineOver(markdownWithInlinePreview)
        _     <- rig.pipeline.scheduleMarkdownPreviewCommits(previous)
        after <- rig.stateRef.get
      yield after

    program.unsafeRunSync().persisted.buffers(bufferId).markdownPreviewEditGeneration shouldBe 1
  }
