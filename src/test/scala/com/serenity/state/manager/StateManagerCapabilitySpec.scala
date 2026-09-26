package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.AppRuntimeRenderLoops
import com.serenity.config.AppConfig
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.input.{InputRouter, SystemClipboard}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.state.undo.UndoState
import com.serenity.testkit.VirtualTime.runVirtual
import com.serenity.ui.layout.{ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.renderer.RenderController
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateManagerCapabilitySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def composedPipeline(
    currentModelRef: Ref[IO, Model],
    operations: StateManagerOperationBoundary,
    runEffect: AppEffect => IO[Unit]
  ): StateManagerEventPipeline =
    val currentLogger   = org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
    val currentStateRef = ModelViews.appRef(currentModelRef)
    val currentCacheRef = Ref.of[IO, Option[MouseTargetCache]](None).unsafeRunSync()
    val statePort = new EventStatePort:
      val logger              = currentLogger
      val mouseTargetCacheRef = currentCacheRef
    val effectPort = EventEffectPort(
      interpretEffect = runEffect,
      interpretCommand = (_, _) => IO.unit
    )
    val workflowPort = new EventWorkflowPort:
      def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit] = IO.unit
    val undoRecording = new UndoRecording(new UndoRecordingPort:
      def updateUndo(update: UndoState => UndoState): IO[Unit] = ModelViews.undoRef(currentModelRef).update(update)
      def updateModelValidated(transition: Model => Option[Model]): IO[Unit] =
        operations.modelCommit.updateValidated(transition))
    new StateManagerEventPipeline(
      statePort,
      effectPort,
      workflowPort,
      UiPresetStore(Path.of("target", "state-manager-capability-spec.json")),
      update =>
        currentStateRef.modify(state =>
          val config = update(state.persisted.config)
          (state.copy(persisted = state.persisted.copy(config = config)), config)
        ),
      (_, _) => IO.unit,
      operations,
      undoRecording
    )

  "StateManager" should "compose focused façade capabilities" in {
    // All of these are #1017's capability-record slices: fields, not mixed-in traits. `scrollManager`, `focusManager`,
    // `peekManager`, `modalService`, `paneManager`/`panelManager`, `commandExecutor`, and `bufferManager` were all
    // retired in #1724 -- production drives their behavior through real events, or (for `commandExecutor`) plain
    // methods on `StateManager` via `StateManagerTestFacade.executeCommand`.
    val _: StateManager => (FileOpener, FileService) =
      sm => (sm.fileOpener, sm.fileService)
    val _: StateManager => AnimationTicker =
      sm => sm.animationTicker
    val _: StateManager => (RuntimeLifecycle, SessionService) =
      sm => (sm.runtimeLifecycle, sm.sessionService)
    summon[StateManager <:< StateEngine] // hot state engine: a mixed-in trait, not a record field (#1017)
    succeed
  }

  "StateManagerFileFacade" should "delegate file operations without a StateManager" in {
    val stateRef = Ref.of[IO, AppState](AppState.initial).unsafeRunSync()
    val calls    = Ref.of[IO, List[String]](Nil).unsafeRunSync()
    val facade = new StateManagerFileFacade(
      stateRef.get,
      update => stateRef.update(update),
      path => calls.update(_ :+ s"open:$path"),
      bufferId => calls.update(_ :+ s"save:$bufferId"),
      (bufferId, path) => calls.update(_ :+ s"save-as:$bufferId:$path")
    )
    val path = Path.of("notes.md")

    facade.openFile(path).unsafeRunSync()
    facade.saveBuffer(BufferId(0)).unsafeRunSync()
    facade.saveBufferAs(BufferId(0), path).unsafeRunSync()

    calls.get.unsafeRunSync() shouldBe List(
      s"open:$path",
      "save:0",
      s"save-as:0:$path"
    )
  }

  "RenderController" should "depend only on the event application capability" in {
    val applied = Ref.of[IO, List[ResizeEvent]](Nil).unsafeRunSync()
    val events = new EventApplier:
      def applyEvent(event: com.serenity.keystroke.events.Event): IO[Unit] =
        event match
          case resize: ResizeEvent => applied.update(_ :+ resize)
          case _                   => IO.unit

    RenderController.handleResize(Some(ViewportSize(120, 40)), events, IO.unit).unsafeRunSync()

    applied.get.unsafeRunSync() shouldBe List(ResizeEvent(ViewportSize(120, 40)))
  }

  "StateManager composition" should "exclude retired behavior implementations and dependency hubs" in {
    val sources = List(
      "StateManagerEditorCapability.scala",
      "StateManagerEffectHandlers.scala",
      "StateManagerFileCapability.scala",
      "StateManagerSurfaceCapability.scala",
      "StateManagerViewportCapability.scala",
      "StateManagerWorkflowCapability.scala"
    ).map(name => Files.readString(Path.of("src/main/scala/com/serenity/state/manager", name)))

    sources.mkString("\n") should not include "StateManagerRuntimeSupport"
    sources.mkString("\n") should not include "StateManagerBehaviorDependencies"
    sources.mkString("\n") should not include "EffectCapabilityPort"
    sources.mkString("\n") should not include "StateManagerRuntime,"
  }

  it should "name production components by their explicit ownership rather than behavior" in {
    val managerDirectory = Path.of("src/main/scala/com/serenity/state/manager")
    val sourcePaths = Files
      .list(managerDirectory)
      .toArray
      .collect { case path: Path if path.getFileName.toString.endsWith(".scala") => path }

    sourcePaths.map(_.getFileName.toString) should not contain "StateManagerRuntimeSupport.scala"
    sourcePaths.map(_.getFileName.toString).exists(_.contains("Behavior")) shouldBe false
    sourcePaths.map(Files.readString).mkString("\n") should not include "StateManagerBehavior"
  }

  it should "keep Balance capability-local rather than protected" in {
    val sources = Files
      .list(Path.of("src/main/scala/com/serenity/state/manager"))
      .toArray
      .collect { case path: Path if path.getFileName.toString.endsWith(".scala") => Files.readString(path) }
      .mkString("\n")

    sources should not include "protected val balance"
  }

  it should "wire capability ports directly to their owning components" in {
    val compositionRoot = Files.readString(
      Path.of("src/main/scala/com/serenity/state/manager/StateManagerComposition.scala")
    )

    compositionRoot should not include "StateManagerBehavior.this"
  }

  it should "route effect-triggered events through the shared operation boundary" in {
    val compositionRoot = Files.readString(
      Path.of("src/main/scala/com/serenity/state/manager/StateManagerComposition.scala")
    )
    val effectHandlers = Files.readString(
      Path.of("src/main/scala/com/serenity/state/manager/StateManagerEffectHandlers.scala")
    )
    val effectEditorPort = compositionRoot.slice(
      compositionRoot.indexOf("private val effectEditorPort"),
      compositionRoot.indexOf("private val effectSurfacePort")
    )
    val effectSurfacePort = compositionRoot.slice(
      compositionRoot.indexOf("private val effectSurfacePort"),
      compositionRoot.indexOf("private val effectFilePort")
    )
    val effectModalWorkflowPort = compositionRoot.slice(
      compositionRoot.indexOf("private val effectModalWorkflowPort"),
      compositionRoot.indexOf("private val effects")
    )
    val eventWorkflowPort = compositionRoot.slice(
      compositionRoot.indexOf("private val eventWorkflowPort"),
      compositionRoot.indexOf("private val events")
    )

    compositionRoot should include("StateManagerOperationBoundary")
    effectEditorPort should not include "events."
    effectSurfacePort should not include "events."
    effectModalWorkflowPort should not include "effects."
    effectModalWorkflowPort should not include "events."
    eventWorkflowPort should not include "effects."
    effectHandlers should include("enqueueEvent")
    effectHandlers should not include "applyEvent("
  }

  it should "preserve effect-triggered event order at the operation boundary" in {
    val modelRef = ModelViews.modelOf(AppState.initial).unsafeRunSync()
    val operations = StateManagerOperationBoundary
      .create(
        modelRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
      )
      .unsafeRunSync()

    (operations.enqueueEvent(Copy) >>
      operations.enqueueEvent(Paste) >>
      operations.takeOperations).unsafeRunSync() shouldBe List(
      StateManagerOperation.Event(Copy),
      StateManagerOperation.Event(Paste)
    )
    operations.takeOperations.unsafeRunSync() shouldBe Nil
  }

  it should "skip surface animation hooks when motion is disabled" in {
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withMotionAccessibility(com.serenity.config.MotionAccessibility.Off)
      )
    )
    val modelRef = Ref.of[IO, Model](Model(state, UndoState(), Map.empty)).unsafeRunSync()
    val operations = StateManagerOperationBoundary
      .create(modelRef, org.typelevel.log4cats.noop.NoOpLogger.impl[IO])
      .unsafeRunSync()
    val pipeline = composedPipeline(modelRef, operations, _ => IO.unit)

    pipeline.shouldApplySurfaceAnimationHooks(state) shouldBe false
  }

  it should "coordinate document analysis scheduling with shutdown" in {
    val spellCheckEnabledState =
      val enabled = AppState.initial.copy(
        persisted = AppState.initial.persisted.copy(
          config = AppState.initial.persisted.config
            .withSpellCheck(AppConfig.default.languageToolsConfig.spellCheck.copy(enabled = true))
        )
      )
      val buffer = enabled.persisted.buffers(BufferId(0))
      enabled.copy(persisted =
        enabled.persisted.copy(buffers =
          enabled.persisted.buffers
            .updated(BufferId(0), buffer.copy(document = buffer.document.copy(content = Rope("qzxvbnw"))))
        )
      )
    val program = for
      modelRef           <- ModelViews.modelOf(spellCheckEnabledState)
      scheduleStarted    <- cats.effect.Deferred[IO, Unit]
      continueScheduling <- cats.effect.Deferred[IO, Unit]
      shutdownRequested  <- cats.effect.Deferred[IO, Unit]
      operations <- StateManagerOperationBoundary.create(
        modelRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO],
        beforeDocumentAnalysisStart = scheduleStarted.complete(()).void >> continueScheduling.get,
        beforeEffectsShutdown = shutdownRequested.complete(()).void
      )
      scheduling <- operations.scheduleDocumentAnalysis().start
      _          <- scheduleStarted.get
      shutdown   <- operations.shutdownEffects().start
      _          <- shutdownRequested.get
      _          <- continueScheduling.complete(())
      _          <- scheduling.joinWithNever
      _          <- shutdown.joinWithNever
      _          <- operations.scheduleDocumentAnalysis()
      _          <- IO.sleep(1.second)
      after      <- ModelViews.appRef(modelRef).get
    yield after.runtime.diagnosticsState.diagnostics shouldBe empty

    runVirtual(program)
  }

  it should "skip document analysis scheduling when spell checking is disabled" in {
    val program = for
      modelRef <- ModelViews.modelOf(AppState.initial)
      starts   <- Ref.of[IO, Int](0)
      operations <- StateManagerOperationBoundary.create(
        modelRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO],
        beforeDocumentAnalysisStart = starts.update(_ + 1)
      )
      _       <- operations.scheduleDocumentAnalysis()
      started <- starts.get
    yield started shouldBe 0

    program.unsafeRunSync()
  }

  it should "schedule document analysis only when spell-check inputs change" in {
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config =
          AppConfig.default.withSpellCheck(AppConfig.default.languageToolsConfig.spellCheck.copy(enabled = true)),
        buffers =
          val buffer = AppState.initial.persisted.buffers(bufferId)
          AppState.initial.persisted.buffers
            .updated(bufferId, buffer.copy(document = buffer.document.copy(content = Rope("hello"))))
      )
    )
    val movedCursorState = initialState.copy(
      persisted = initialState.persisted.copy(
        buffers =
          val buffer = initialState.persisted.buffers(bufferId)
          initialState.persisted.buffers
            .updated(bufferId, buffer.copy(editing = EditingState(List(CursorPosition(0, 1)))))
      )
    )
    val editedState = movedCursorState.copy(
      persisted = movedCursorState.persisted.copy(
        buffers =
          val buffer = movedCursorState.persisted.buffers(bufferId)
          movedCursorState.persisted.buffers
            .updated(bufferId, buffer.copy(document = buffer.document.copy(content = Rope("wurld"))))
      )
    )
    val configuredState = editedState.copy(
      persisted = editedState.persisted.copy(
        config = editedState.persisted.config.withSpellCheck(
          editedState.persisted.config.languageToolsConfig.spellCheck.copy(additionalWords = List("wurld"))
        )
      )
    )

    val program = for
      modelRef <- ModelViews.modelOf(initialState)
      starts   <- Ref.of[IO, Int](0)
      operations <- StateManagerOperationBoundary.create(
        modelRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO],
        beforeDocumentAnalysisStart = starts.update(_ + 1)
      )
      _                        <- operations.modelCommit.commitState(initialState, initialState)
      _                        <- operations.modelCommit.commitState(movedCursorState, initialState)
      afterCursorMove          <- starts.get
      _                        <- operations.modelCommit.commitState(editedState, movedCursorState)
      afterEdit                <- starts.get
      _                        <- operations.modelCommit.commitState(configuredState, editedState)
      afterConfigurationChange <- starts.get
      _                        <- operations.shutdownEffects()
    yield
      afterCursorMove shouldBe 1
      afterEdit shouldBe 2
      afterConfigurationChange shouldBe 3

    program.unsafeRunSync()
  }

  it should "commit a reducer state before interpreting its effect" in {
    val initialState   = AppState.initial
    val committedState = initialState.copy(runtime = initialState.runtime.copy(nextBufferId = BufferId(42)))
    val program = for
      modelRef <- Ref.of[IO, Model](Model(initialState, UndoState(), Map.empty))
      operations <- StateManagerOperationBoundary.create(
        modelRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
      )
      observed <- Ref.of[IO, List[Int]](Nil)
      pipeline = composedPipeline(
        modelRef,
        operations,
        _ => ModelViews.appRef(modelRef).get.flatMap(state => observed.update(_ :+ state.runtime.nextBufferId.value))
      )
      _ <- pipeline.applyReducerResult(
        ReducerResult.withEffect(committedState, AppEffect.CompleteQuit),
        initialState
      )
      observedStates <- observed.get
      state          <- ModelViews.appRef(modelRef).get
    yield
      observedStates shouldBe List(42)
      state.runtime.nextBufferId shouldBe BufferId(42)

    program.unsafeRunSync()
  }

  it should "drain effect-triggered nested events through the pipeline in FIFO order" in {
    val initialState = AppState.initial
    val program = for
      modelRef <- Ref.of[IO, Model](Model(initialState, UndoState(), Map.empty))
      operations <- StateManagerOperationBoundary.create(
        modelRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
      )
      pipeline = composedPipeline(
        modelRef,
        operations,
        _ => operations.enqueueEvent(ToggleCommandRunner) >> operations.enqueueEvent(RunnerInsertChar('x'))
      )
      _ <- pipeline.applyReducerResult(
        ReducerResult.withEffect(initialState, AppEffect.CompleteQuit),
        initialState
      )
      state <- ModelViews.appRef(modelRef).get
    yield state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner.searchTerm)
        case _                                     => None
    } shouldBe Some("x")

    program.unsafeRunSync()
  }

  it should "construct every capability port without lazy callback wiring" in {
    val compositionRoot = Files.readString(
      Path.of("src/main/scala/com/serenity/state/manager/StateManagerComposition.scala")
    )

    compositionRoot should not include "private lazy val"
  }

  "CommandEffectInterpreter" should "preserve declared effect order and propagate required failures" in {
    val program = for
      observed <- Ref.of[IO, List[String]](Nil)
      interpreter = new CommandEffectInterpreter(
        CommandEffectInterpreter.Dependencies(
          lifecycle = observed.update(_ :+ "lifecycle"),
          command = _ => observed.update(_ :+ "command"),
          theme = _ => observed.update(_ :+ "theme"),
          surface = _ => observed.update(_ :+ "surface"),
          file = _ => IO.raiseError(new IllegalStateException("file failed")),
          explorer = _ => observed.update(_ :+ "explorer"),
          workflow = _ => observed.update(_ :+ "workflow"),
          lspQueue = _ => observed.update(_ :+ "lsp"),
          animation = _ => observed.update(_ :+ "animation"),
          scheduleCommandRunnerBindingExpiry = _ => observed.update(_ :+ "scheduleCommandRunnerBindingExpiry")
        )
      )
      _       <- interpreter.interpret(AppEffect.CompleteQuit)
      failure <- interpreter.interpret(AppEffect.File(FileEffect.DirectLoadFile(Path.of("missing")))).attempt
      entries <- observed.get
    yield
      entries shouldBe List("lifecycle")
      failure.isLeft shouldBe true

    program.unsafeRunSync()
  }

  "AppRuntime input phase" should "depend only on state read, update, and event application capabilities" in {
    val modelRef = Ref.of[IO, Model](Model(AppState.initial, UndoState(), Map.empty)).unsafeRunSync()
    val stateRef = ModelViews.appRef(modelRef)
    val applied  = Ref.of[IO, List[Event]](Nil).unsafeRunSync()
    val capabilities = new StateEngine:
      def getModel: IO[Model]                                          = modelRef.get
      def getCurrentState: IO[AppState]                                = stateRef.get
      def updateStateValidated(update: AppState => AppState): IO[Unit] = stateRef.update(update)
      def applyEvent(event: Event): IO[Unit]                           = applied.update(_ :+ event)
    val router        = InputRouter.create[IO, Event](new TextEntryTranslator(AppConfig.default)).unsafeRunSync()
    val clipboard     = SystemClipboard[IO](readText = IO.pure(Some("pasted")), writeText = _ => IO.unit)
    val cursorVisible = Ref.of[IO, Boolean](true).unsafeRunSync()
    val breathIndex   = Ref.of[IO, Int](0).unsafeRunSync()

    AppRuntimeRenderLoops
      .inputEventPhase(capabilities, router, clipboard, IO.unit, cursorVisible, breathIndex, (_: Damage) => IO.unit)(
        Stream.emit(Paste)
      )
      .compile
      .drain
      .unsafeRunSync()

    stateRef.get.unsafeRunSync().runtime.clipboard.shouldBe(Some("pasted"))
    applied.get.unsafeRunSync().shouldBe(List(Paste))
  }

  private def withPreviewBuffer(bufferId: BufferId, editGeneration: Long): AppState =
    val preview = Buffer.fromString(bufferId, "# hello").copy(markdownPreviewEditGeneration = editGeneration)
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(buffers = AppState.initial.persisted.buffers + (bufferId -> preview)),
      runtime = AppState.initial.runtime.copy(nextBufferId = BufferId(bufferId.value + 1))
    )

  "StateManagerOperationBoundary" should "commit a markdown preview render generation after the debounce settles" in {
    val bufferId     = BufferId(1)
    val initialState = withPreviewBuffer(bufferId, editGeneration = 1L)
    val program = for
      modelRef <- ModelViews.modelOf(initialState)
      stateRef = ModelViews.appRef(modelRef)
      operations <- StateManagerOperationBoundary.create(
        modelRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
      )
      _          <- operations.scheduleMarkdownPreviewCommit(bufferId, 1L)
      _          <- IO.sleep(250.millis)
      afterState <- stateRef.get
    yield afterState.persisted.buffers(bufferId).markdownPreviewCommittedGeneration

    runVirtual(program) shouldBe 1L
  }

  it should "cancel a pending markdown preview commit when superseded by a newer edit" in {
    val bufferId     = BufferId(1)
    val initialState = withPreviewBuffer(bufferId, editGeneration = 2L)
    val program = for
      modelRef <- ModelViews.modelOf(initialState)
      stateRef = ModelViews.appRef(modelRef)
      operations <- StateManagerOperationBoundary.create(
        modelRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
      )
      _          <- operations.scheduleMarkdownPreviewCommit(bufferId, 1L)
      _          <- operations.scheduleMarkdownPreviewCommit(bufferId, 2L)
      _          <- IO.sleep(250.millis)
      afterState <- stateRef.get
    yield afterState.persisted.buffers(bufferId).markdownPreviewCommittedGeneration

    runVirtual(program) shouldBe 2L
  }

  "StateManagerEventPipeline" should "recognize a live markdown preview via a pinned panel surface" in {
    val bufferId = BufferId(1)
    val state = AppState.initial.copy(
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("markdown-preview"),
            SurfaceContent.MarkdownPreview(bufferId, "notes.md"),
            SurfacePresentation.Docked
          )
        )
      )
    )
    val modelRef = Ref.of[IO, Model](Model(state, UndoState(), Map.empty)).unsafeRunSync()
    val operations = StateManagerOperationBoundary
      .create(modelRef, org.typelevel.log4cats.noop.NoOpLogger.impl[IO])
      .unsafeRunSync()
    val pipeline = composedPipeline(modelRef, operations, _ => IO.unit)

    pipeline.hasLiveMarkdownPreview(state, bufferId) shouldBe true
    pipeline.hasLiveMarkdownPreview(state, BufferId(2)) shouldBe false
  }

  private def focusedOnBuffer(state: AppState, paneId: PaneId, bufferId: BufferId): AppState =
    state.copy(
      persisted = state.persisted.copy(
        layout = com.serenity.ui.layout.Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId)
      )
    )

  it should "bump markdownPreviewEditGeneration when an edit changes a buffer with a live inline markdown preview" in {
    val bufferId       = BufferId(0)
    val paneId         = PaneId(0)
    val beforeUnstyled = Buffer.fromString(bufferId, "# Before")
    val before =
      beforeUnstyled.copy(document =
        beforeUnstyled.document.copy(language = Some(com.serenity.lsp.config.LanguageId.Markdown))
      )
    val after = before.copy(document = before.document.copy(content = Rope("# After")))
    val prevState = focusedOnBuffer(
      AppState.initial.copy(
        persisted = AppState.initial.persisted.copy(
          buffers = Map(bufferId -> before),
          config = AppConfig.default.withMarkdownViewMode(com.serenity.config.MarkdownViewMode.InlineLens)
        )
      ),
      paneId,
      bufferId
    )
    val currentState = prevState.copy(persisted = prevState.persisted.copy(buffers = Map(bufferId -> after)))
    val program = for
      modelRef <- Ref.of[IO, Model](Model(currentState, UndoState(), Map.empty))
      operations <- StateManagerOperationBoundary.create(
        modelRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
      )
      pipeline = composedPipeline(modelRef, operations, _ => IO.unit)
      _          <- pipeline.scheduleMarkdownPreviewCommits(prevState)
      afterState <- ModelViews.appRef(modelRef).get
    yield afterState.persisted.buffers(bufferId).markdownPreviewEditGeneration

    program.unsafeRunSync() shouldBe 1L
  }

  it should "leave markdownPreviewEditGeneration untouched when the buffer has no live markdown preview" in {
    val bufferId       = BufferId(1)
    val paneId         = PaneId(1)
    val beforeUnstyled = Buffer.fromString(bufferId, "# Before")
    val before =
      beforeUnstyled.copy(document =
        beforeUnstyled.document.copy(language = Some(com.serenity.lsp.config.LanguageId.Markdown))
      )
    val after = before.copy(document = before.document.copy(content = Rope("# After")))
    // No withMarkdownViewMode(InlineLens) and no MarkdownPreview surface -- markdownViewMode defaults to Source.
    val prevState = focusedOnBuffer(
      AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> before))),
      paneId,
      bufferId
    )
    val currentState = prevState.copy(persisted = prevState.persisted.copy(buffers = Map(bufferId -> after)))
    val program = for
      modelRef <- Ref.of[IO, Model](Model(currentState, UndoState(), Map.empty))
      operations <- StateManagerOperationBoundary.create(
        modelRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
      )
      pipeline = composedPipeline(modelRef, operations, _ => IO.unit)
      _          <- pipeline.scheduleMarkdownPreviewCommits(prevState)
      afterState <- ModelViews.appRef(modelRef).get
    yield afterState.persisted.buffers(bufferId).markdownPreviewEditGeneration

    program.unsafeRunSync() shouldBe 0L
  }

  it should "leave markdownPreviewEditGeneration untouched when the buffer's content did not change" in {
    val bufferId       = BufferId(1)
    val paneId         = PaneId(1)
    val bufferUnstyled = Buffer.fromString(bufferId, "# Same")
    val buffer =
      bufferUnstyled.copy(document =
        bufferUnstyled.document.copy(language = Some(com.serenity.lsp.config.LanguageId.Markdown))
      )
    val prevState = focusedOnBuffer(
      AppState.initial.copy(
        persisted = AppState.initial.persisted.copy(
          buffers = Map(bufferId -> buffer),
          config = AppConfig.default.withMarkdownViewMode(com.serenity.config.MarkdownViewMode.InlineLens)
        )
      ),
      paneId,
      bufferId
    )
    val program = for
      modelRef <- Ref.of[IO, Model](Model(prevState, UndoState(), Map.empty))
      operations <- StateManagerOperationBoundary.create(
        modelRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
      )
      pipeline = composedPipeline(modelRef, operations, _ => IO.unit)
      _          <- pipeline.scheduleMarkdownPreviewCommits(prevState)
      afterState <- ModelViews.appRef(modelRef).get
    yield afterState.persisted.buffers(bufferId).markdownPreviewEditGeneration

    program.unsafeRunSync() shouldBe 0L
  }
