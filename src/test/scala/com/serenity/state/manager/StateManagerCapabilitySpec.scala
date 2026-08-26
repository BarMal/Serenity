package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.AppRuntime
import com.serenity.config.AppConfig
import com.serenity.input.{InputRouter, SystemClipboard}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.state.undo.UndoState
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.renderer.RenderController
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateManagerCapabilitySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def composedPipeline(
    currentStateRef: Ref[IO, AppState],
    operations: StateManagerOperationBoundary,
    runEffect: AppEffect => IO[Unit]
  ): StateManagerEventPipeline =
    val currentLogger   = org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
    val currentUndoRef  = Ref.of[IO, UndoState](UndoState()).unsafeRunSync()
    val currentFiberRef = Ref.of[IO, Option[cats.effect.Fiber[IO, Throwable, Unit]]](None).unsafeRunSync()
    val currentCacheRef = Ref.of[IO, Option[MouseTargetCache]](None).unsafeRunSync()
    val currentBufferAnimationsRef =
      Ref.of[IO, Map[BufferId, com.serenity.animation.AnimationState]](Map.empty).unsafeRunSync()
    val statePort = new EventStatePort:
      val stateRef                 = currentStateRef
      val undoRef                  = currentUndoRef
      val logger                   = currentLogger
      val documentAnalysisFiberRef = currentFiberRef
      val mouseTargetCacheRef      = currentCacheRef
      val bufferAnimationsRef      = currentBufferAnimationsRef
    val effectPort = new EventEffectPort:
      def interpretEffect(effect: AppEffect): IO[Unit]                                       = runEffect(effect)
      def interpretCommand(command: com.serenity.command.Command, state: AppState): IO[Unit] = IO.unit
      def executeCommand(command: com.serenity.command.Command): IO[Unit]                    = IO.unit
    val workflowPort = new EventWorkflowPort:
      def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit]      = IO.unit
      def createBuffer(content: String, filePath: Option[Path]): IO[BufferId] = IO.pure(BufferId(0))
      def createPane(bufferId: Option[BufferId]): IO[PaneId]                  = IO.pure(PaneId(0))
    val uiPort = new EventUiPort:
      val uiPresetStore = UiPresetStore(Path.of("target", "state-manager-capability-spec.json"))
      def updateConfig(update: AppConfig => AppConfig): IO[AppConfig] =
        currentStateRef.modify(state =>
          val config = update(state.config)
          (state.copy(config = config), config)
        )
      def resizePinnedPanel(position: com.serenity.ui.layout.PanelPosition, newSize: Int): IO[Unit] = IO.unit
    new StateManagerEventPipeline(statePort, effectPort, workflowPort, uiPort, operations)

  "StateManager" should "compose focused façade capabilities" in {
    summon[StateManager <:< FocusManager]
    summon[StateManager <:< BufferManager]
    summon[StateManager <:< PaneManager]
    summon[StateManager <:< PeekManager]
    summon[StateManager <:< SessionService]
    summon[StateManager <:< PanelManager]
    summon[StateManager <:< ModalService]
    summon[StateManager <:< FileService]
    summon[StateManager <:< ScrollManager]
    succeed
  }

  "StateManagerFileFacade" should "delegate file operations without a StateManager" in {
    val stateRef = Ref.of[IO, AppState](AppState.initial).unsafeRunSync()
    val calls    = Ref.of[IO, List[String]](Nil).unsafeRunSync()
    val facade = new StateManagerFileFacade(
      stateRef,
      path => calls.update(_ :+ s"open:$path"),
      bufferId => calls.update(_ :+ s"save:$bufferId"),
      (bufferId, path) => calls.update(_ :+ s"save-as:$bufferId:$path"),
      bufferId => calls.update(_ :+ s"close:$bufferId")
    )
    val path = Path.of("notes.md")

    facade.openFile(path).unsafeRunSync()
    facade.saveBuffer(BufferId(0)).unsafeRunSync()
    facade.saveBufferAs(BufferId(0), path).unsafeRunSync()
    facade.forceCloseBuffer(BufferId(0)).unsafeRunSync()

    calls.get.unsafeRunSync() shouldBe List(
      s"open:$path",
      "save:BufferId(0)",
      s"save-as:BufferId(0):$path",
      "close:BufferId(0)"
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
    val workflowPort = compositionRoot.slice(
      compositionRoot.indexOf("private val workflowPort"),
      compositionRoot.indexOf("private val surfacePort")
    )
    val surfacePort = compositionRoot.slice(
      compositionRoot.indexOf("private val surfacePort"),
      compositionRoot.indexOf("private val viewportPort")
    )

    compositionRoot should include("StateManagerOperationBoundary")
    effectEditorPort should not include "events."
    effectSurfacePort should not include "events."
    workflowPort should not include "effects."
    workflowPort should not include "events."
    surfacePort should not include "events."
    effectHandlers should include("enqueueEvent")
    effectHandlers should not include "applyEvent("
  }

  it should "preserve effect-triggered event order at the operation boundary" in {
    val stateRef = Ref.of[IO, AppState](AppState.initial).unsafeRunSync()
    val fiberRef = Ref.of[IO, Option[cats.effect.Fiber[IO, Throwable, Unit]]](None).unsafeRunSync()
    val operations = StateManagerOperationBoundary
      .create(
        stateRef,
        fiberRef,
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
      config = AppConfig.default.withMotionAccessibility(com.serenity.config.MotionAccessibility.Off)
    )
    val stateRef = Ref.of[IO, AppState](state).unsafeRunSync()
    val fiberRef = Ref.of[IO, Option[cats.effect.Fiber[IO, Throwable, Unit]]](None).unsafeRunSync()
    val operations = StateManagerOperationBoundary
      .create(stateRef, fiberRef, org.typelevel.log4cats.noop.NoOpLogger.impl[IO])
      .unsafeRunSync()
    val pipeline = composedPipeline(stateRef, operations, _ => IO.unit)

    pipeline.shouldApplySurfaceAnimationHooks(state) shouldBe false
  }

  it should "coordinate document analysis scheduling with shutdown" in {
    val spellCheckEnabledState = AppState.initial.copy(
      config = AppState.initial.config.withSpellCheck(AppConfig.default.spellCheck.copy(enabled = true))
    )
    val program = for
      stateRef           <- Ref.of[IO, AppState](spellCheckEnabledState)
      fiberRef           <- Ref.of[IO, Option[cats.effect.Fiber[IO, Throwable, Unit]]](None)
      scheduleStarted    <- cats.effect.Deferred[IO, Unit]
      continueScheduling <- cats.effect.Deferred[IO, Unit]
      shutdownRequested  <- cats.effect.Deferred[IO, Unit]
      operations <- StateManagerOperationBoundary.create(
        stateRef,
        fiberRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO],
        beforeDocumentAnalysisStart = scheduleStarted.complete(()).void >> continueScheduling.get,
        beforeDocumentAnalysisShutdown = shutdownRequested.complete(()).void
      )
      scheduling    <- operations.scheduleDocumentAnalysis().start
      _             <- scheduleStarted.get
      shutdown      <- operations.cancelDocumentAnalysis().start
      _             <- shutdownRequested.get
      _             <- continueScheduling.complete(())
      _             <- scheduling.joinWithNever
      _             <- shutdown.joinWithNever
      pending       <- fiberRef.get
      _             <- pending.fold(IO.unit)(_.cancel)
      _             <- operations.scheduleDocumentAnalysis()
      afterShutdown <- fiberRef.get
    yield
      pending shouldBe None
      afterShutdown shouldBe None

    program.unsafeRunSync()
  }

  it should "skip document analysis scheduling when spell checking is disabled" in {
    val program = for
      stateRef <- Ref.of[IO, AppState](AppState.initial)
      fiberRef <- Ref.of[IO, Option[cats.effect.Fiber[IO, Throwable, Unit]]](None)
      starts   <- Ref.of[IO, Int](0)
      operations <- StateManagerOperationBoundary.create(
        stateRef,
        fiberRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO],
        beforeDocumentAnalysisStart = starts.update(_ + 1)
      )
      _       <- operations.scheduleDocumentAnalysis()
      started <- starts.get
      pending <- fiberRef.get
    yield
      started shouldBe 0
      pending shouldBe None

    program.unsafeRunSync()
  }

  it should "schedule document analysis only when spell-check inputs change" in {
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      config = AppConfig.default.withSpellCheck(AppConfig.default.spellCheck.copy(enabled = true)),
      buffers =
        val buffer = AppState.initial.buffers(bufferId)
        AppState.initial.buffers
          .updated(bufferId, buffer.copy(document = buffer.document.copy(content = Rope("hello"))))
    )
    val movedCursorState = initialState.copy(
      buffers =
        val buffer = initialState.buffers(bufferId)
        initialState.buffers
          .updated(bufferId, buffer.copy(editing = buffer.editing.copy(cursors = List(CursorPosition(0, 1)))))
    )
    val editedState = movedCursorState.copy(
      buffers =
        val buffer = movedCursorState.buffers(bufferId)
        movedCursorState.buffers
          .updated(bufferId, buffer.copy(document = buffer.document.copy(content = Rope("wurld"))))
    )
    val configuredState = editedState.copy(
      config = editedState.config.withSpellCheck(editedState.config.spellCheck.copy(additionalWords = List("wurld")))
    )

    val program = for
      stateRef <- Ref.of[IO, AppState](initialState)
      fiberRef <- Ref.of[IO, Option[cats.effect.Fiber[IO, Throwable, Unit]]](None)
      starts   <- Ref.of[IO, Int](0)
      operations <- StateManagerOperationBoundary.create(
        stateRef,
        fiberRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO],
        beforeDocumentAnalysisStart = starts.update(_ + 1)
      )
      _                        <- operations.validateAndUpdateState(initialState, initialState)
      _                        <- operations.validateAndUpdateState(movedCursorState, initialState)
      afterCursorMove          <- starts.get
      _                        <- operations.validateAndUpdateState(editedState, movedCursorState)
      afterEdit                <- starts.get
      _                        <- operations.validateAndUpdateState(configuredState, editedState)
      afterConfigurationChange <- starts.get
      _                        <- operations.cancelDocumentAnalysis()
    yield
      afterCursorMove shouldBe 1
      afterEdit shouldBe 2
      afterConfigurationChange shouldBe 3

    program.unsafeRunSync()
  }

  it should "commit a reducer state before interpreting its effect" in {
    val initialState   = AppState.initial
    val committedState = initialState.copy(nextBufferId = BufferId(42))
    val program = for
      stateRef <- Ref.of[IO, AppState](initialState)
      fiberRef <- Ref.of[IO, Option[cats.effect.Fiber[IO, Throwable, Unit]]](None)
      operations <- StateManagerOperationBoundary.create(
        stateRef,
        fiberRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
      )
      observed <- Ref.of[IO, List[Int]](Nil)
      pipeline = composedPipeline(
        stateRef,
        operations,
        _ => stateRef.get.flatMap(state => observed.update(_ :+ state.nextBufferId.value))
      )
      _ <- pipeline.applyReducerResult(
        ReducerResult.withEffect(committedState, AppEffect.CompleteQuit()),
        initialState
      )
      observedStates <- observed.get
      state          <- stateRef.get
    yield
      observedStates shouldBe List(42)
      state.nextBufferId shouldBe BufferId(42)

    program.unsafeRunSync()
  }

  it should "drain effect-triggered nested events through the pipeline in FIFO order" in {
    val initialState = AppState.initial
    val program = for
      stateRef <- Ref.of[IO, AppState](initialState)
      fiberRef <- Ref.of[IO, Option[cats.effect.Fiber[IO, Throwable, Unit]]](None)
      operations <- StateManagerOperationBoundary.create(
        stateRef,
        fiberRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
      )
      pipeline = composedPipeline(
        stateRef,
        operations,
        _ => operations.enqueueEvent(ToggleCommandRunner) >> operations.enqueueEvent(RunnerInsertChar('x'))
      )
      _ <- pipeline.applyReducerResult(
        ReducerResult.withEffect(initialState, AppEffect.CompleteQuit()),
        initialState
      )
      state <- stateRef.get
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
          lifecycle = _ => observed.update(_ :+ "lifecycle"),
          command = _ => observed.update(_ :+ "command"),
          theme = _ => observed.update(_ :+ "theme"),
          surface = _ => observed.update(_ :+ "surface"),
          file = _ => IO.raiseError(new IllegalStateException("file failed")),
          explorer = _ => observed.update(_ :+ "explorer"),
          workflow = _ => observed.update(_ :+ "workflow"),
          lspQueue = _ => observed.update(_ :+ "lsp"),
          animation = _ => observed.update(_ :+ "animation")
        )
      )
      _       <- interpreter.interpret(AppEffect.Lifecycle(LifecycleEffect.CompleteQuit))
      failure <- interpreter.interpret(AppEffect.File(FileEffect.DirectLoadFile(Path.of("missing")))).attempt
      entries <- observed.get
    yield
      entries shouldBe List("lifecycle")
      failure.isLeft shouldBe true

    program.unsafeRunSync()
  }

  "AppRuntime input phase" should "depend only on state read, update, and event application capabilities" in {
    val stateRef = Ref.of[IO, AppState](AppState.initial).unsafeRunSync()
    val applied  = Ref.of[IO, List[Event]](Nil).unsafeRunSync()
    val bufferAnimationsRef =
      Ref.of[IO, Map[BufferId, com.serenity.animation.AnimationState]](Map.empty).unsafeRunSync()
    val capabilities = new StateReader with StateUpdater with EventApplier:
      def getCurrentState: IO[AppState]                                                 = stateRef.get
      def getBufferAnimations: IO[Map[BufferId, com.serenity.animation.AnimationState]] = bufferAnimationsRef.get
      def updateState(update: AppState => AppState): IO[Unit]                           = stateRef.update(update)
      def updateBufferAnimations(
        update: Map[BufferId, com.serenity.animation.AnimationState] => Map[
          BufferId,
          com.serenity.animation.AnimationState
        ]
      ): IO[Unit] = bufferAnimationsRef.update(update)
      def applyEvent(event: Event): IO[Unit] = applied.update(_ :+ event)
    val router = InputRouter.create[IO, Event](new TextEntryTranslator(AppConfig.default)).unsafeRunSync()
    val clipboard = new SystemClipboard[IO]:
      def readText: IO[Option[String]]      = IO.pure(Some("pasted"))
      def writeText(text: String): IO[Unit] = IO.unit
    val cursorVisible = Ref.of[IO, Boolean](true).unsafeRunSync()
    val breathIndex   = Ref.of[IO, Int](0).unsafeRunSync()

    AppRuntime
      .inputEventPhase(capabilities, router, clipboard, IO.unit, cursorVisible, breathIndex, (_: Damage) => IO.unit)(
        Stream.emit(Paste)
      )
      .compile
      .drain
      .unsafeRunSync()

    stateRef.get.unsafeRunSync().clipboard.shouldBe(Some("pasted"))
    applied.get.unsafeRunSync().shouldBe(List(Paste))
  }

  "StateManagerOperationBoundary" should "commit a markdown preview render generation after the debounce settles" in {
    val bufferId     = BufferId(1)
    val initialState = AppState.initial.copy(buffers = Map(bufferId -> Buffer.fromString(bufferId, "# hello")))
    val program = for
      stateRef <- Ref.of[IO, AppState](initialState)
      fiberRef <- Ref.of[IO, Option[cats.effect.Fiber[IO, Throwable, Unit]]](None)
      operations <- StateManagerOperationBoundary.create(
        stateRef,
        fiberRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
      )
      _          <- operations.scheduleMarkdownPreviewCommit(bufferId, 1L)
      _          <- IO.sleep(250.millis)
      afterState <- stateRef.get
    yield afterState.buffers(bufferId).markdownPreviewCommittedGeneration

    program.unsafeRunSync() shouldBe 1L
  }

  it should "cancel a pending markdown preview commit when superseded by a newer edit" in {
    val bufferId     = BufferId(1)
    val initialState = AppState.initial.copy(buffers = Map(bufferId -> Buffer.fromString(bufferId, "# hello")))
    val program = for
      stateRef <- Ref.of[IO, AppState](initialState)
      fiberRef <- Ref.of[IO, Option[cats.effect.Fiber[IO, Throwable, Unit]]](None)
      operations <- StateManagerOperationBoundary.create(
        stateRef,
        fiberRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
      )
      _          <- operations.scheduleMarkdownPreviewCommit(bufferId, 1L)
      _          <- operations.scheduleMarkdownPreviewCommit(bufferId, 2L)
      _          <- IO.sleep(250.millis)
      afterState <- stateRef.get
    yield afterState.buffers(bufferId).markdownPreviewCommittedGeneration

    program.unsafeRunSync() shouldBe 2L
  }

  "StateManagerEventPipeline" should "recognize a live markdown preview via a pinned panel surface" in {
    val bufferId = BufferId(1)
    val state = AppState.initial.copy(
      uiSurfaces = List(
        UiSurface(
          SurfaceId("markdown-preview"),
          SurfaceContent.MarkdownPreview(bufferId, "notes.md"),
          SurfacePresentation.Pinned(com.serenity.ui.layout.PanelPosition.Right, 40)
        )
      )
    )
    val stateRef = Ref.of[IO, AppState](state).unsafeRunSync()
    val fiberRef = Ref.of[IO, Option[cats.effect.Fiber[IO, Throwable, Unit]]](None).unsafeRunSync()
    val operations = StateManagerOperationBoundary
      .create(stateRef, fiberRef, org.typelevel.log4cats.noop.NoOpLogger.impl[IO])
      .unsafeRunSync()
    val pipeline = composedPipeline(stateRef, operations, _ => IO.unit)

    pipeline.hasLiveMarkdownPreview(state, bufferId) shouldBe true
    pipeline.hasLiveMarkdownPreview(state, BufferId(2)) shouldBe false
  }

  private def focusedOnBuffer(state: AppState, paneId: PaneId, bufferId: BufferId): AppState =
    state.copy(
      layout = com.serenity.ui.layout.Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId)
    )

  it should "bump markdownPreviewEditGeneration when an edit changes a buffer with a live inline markdown preview" in {
    val bufferId       = BufferId(1)
    val paneId         = PaneId(1)
    val beforeUnstyled = Buffer.fromString(bufferId, "# Before")
    val before =
      beforeUnstyled.copy(document =
        beforeUnstyled.document.copy(language = Some(com.serenity.lsp.config.LanguageId.Markdown))
      )
    val after = before.copy(document = before.document.copy(content = Rope("# After")))
    val prevState = focusedOnBuffer(
      AppState.initial.copy(
        buffers = Map(bufferId -> before),
        config = AppConfig.default.withMarkdownViewMode(com.serenity.config.MarkdownViewMode.InlineLens)
      ),
      paneId,
      bufferId
    )
    val currentState = prevState.copy(buffers = Map(bufferId -> after))
    val program = for
      stateRef <- Ref.of[IO, AppState](currentState)
      fiberRef <- Ref.of[IO, Option[cats.effect.Fiber[IO, Throwable, Unit]]](None)
      operations <- StateManagerOperationBoundary.create(
        stateRef,
        fiberRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
      )
      pipeline = composedPipeline(stateRef, operations, _ => IO.unit)
      _          <- pipeline.scheduleMarkdownPreviewCommits(prevState)
      afterState <- stateRef.get
    yield afterState.buffers(bufferId).markdownPreviewEditGeneration

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
    val prevState    = focusedOnBuffer(AppState.initial.copy(buffers = Map(bufferId -> before)), paneId, bufferId)
    val currentState = prevState.copy(buffers = Map(bufferId -> after))
    val program = for
      stateRef <- Ref.of[IO, AppState](currentState)
      fiberRef <- Ref.of[IO, Option[cats.effect.Fiber[IO, Throwable, Unit]]](None)
      operations <- StateManagerOperationBoundary.create(
        stateRef,
        fiberRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
      )
      pipeline = composedPipeline(stateRef, operations, _ => IO.unit)
      _          <- pipeline.scheduleMarkdownPreviewCommits(prevState)
      afterState <- stateRef.get
    yield afterState.buffers(bufferId).markdownPreviewEditGeneration

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
        buffers = Map(bufferId -> buffer),
        config = AppConfig.default.withMarkdownViewMode(com.serenity.config.MarkdownViewMode.InlineLens)
      ),
      paneId,
      bufferId
    )
    val program = for
      stateRef <- Ref.of[IO, AppState](prevState)
      fiberRef <- Ref.of[IO, Option[cats.effect.Fiber[IO, Throwable, Unit]]](None)
      operations <- StateManagerOperationBoundary.create(
        stateRef,
        fiberRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
      )
      pipeline = composedPipeline(stateRef, operations, _ => IO.unit)
      _          <- pipeline.scheduleMarkdownPreviewCommits(prevState)
      afterState <- stateRef.get
    yield afterState.buffers(bufferId).markdownPreviewEditGeneration

    program.unsafeRunSync() shouldBe 0L
  }
