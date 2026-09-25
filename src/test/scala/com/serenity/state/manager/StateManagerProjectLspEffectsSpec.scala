package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.{LspIntent, ProjectIntent}
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.project.{ProjectTaskCommand, ProjectTaskKind, ProjectTaskTerminal}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.effects.Lane
import com.serenity.state.models.*
import com.serenity.testkit.VirtualTime.runVirtual
import com.serenity.ui.layout.{PanelPosition, PeekContent}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Exercises [[StateManagerProjectLspEffects]] on its own: project-task detection/start/cancel (piped into a pinned
  * terminal panel) and LSP hover/completion/definition dispatch for the focused buffer, each asserted through the
  * pinned terminal text, the queued [[LspEffect]], or the peek shown -- rather than through a fully composed
  * `StateManager`.
  */
class StateManagerProjectLspEffectsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  final private class Harness(
      val stateRef: Ref[IO, AppState],
      val pinCalls: Ref[IO, List[(String, PanelPosition, Int)]],
      val peeks: Ref[IO, List[(PeekContent, CursorPosition)]],
      val modals: Ref[IO, List[Modal]],
      val submitted: Ref[IO, List[Lane.Keyed]],
      val lspQueue: LspEffectQueue,
      val effects: StateManagerProjectLspEffects
  )

  private def harness(): Harness =
    val stateRef  = Ref.of[IO, AppState](AppState.initial).unsafeRunSync()
    val pinCalls  = Ref.of[IO, List[(String, PanelPosition, Int)]](Nil).unsafeRunSync()
    val peeks     = Ref.of[IO, List[(PeekContent, CursorPosition)]](Nil).unsafeRunSync()
    val modals    = Ref.of[IO, List[Modal]](Nil).unsafeRunSync()
    val submitted = Ref.of[IO, List[Lane.Keyed]](Nil).unsafeRunSync()
    val lspQueue  = LspEffectQueue.create.unsafeRunSync()

    // Records where each job would run instead of running it: no process is launched here.
    val lanes = new EffectLanePort:
      def submitEffect(lane: Lane.Keyed, job: IO[Unit]): IO[Unit] = submitted.update(_ :+ lane)
      def dispatchEffectResult(result: EffectResult, onApplied: AppState => IO[Unit]): IO[Unit] =
        stateRef.update(EffectResult.applyIfCurrent(_, result))

    new Harness(
      stateRef,
      pinCalls,
      peeks,
      modals,
      submitted,
      lspQueue,
      new StateManagerProjectLspEffects(
        lspQueue,
        stateRef.get,
        stateRef.update,
        lanes,
        (_, _) => IO.never,
        (text, position, size) => pinCalls.update(_ :+ (text, position, size)),
        (content, cursor) => peeks.update(_ :+ (content, cursor)),
        modal => modals.update(_ :+ modal)
      )
    )

  private def stateFocusedOnFile(path: Path): AppState =
    val buffer = Buffer(BufferId(0), Document(Rope.empty, filePath = Some(path)))
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(BufferId(0) -> buffer)))

  private def bufferWithLanguage(
    path: Option[Path],
    language: Option[LanguageId],
    content: String,
    cursor: CursorPosition
  ): Buffer =
    Buffer(BufferId(0), Document(Rope(content), filePath = path, language = language))
      .copy(editing = EditingState(List(cursor)))

  private def stateWithBuffer(buffer: Buffer): AppState =
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(BufferId(0) -> buffer)))

  private val runningTask =
    RunningProjectTask(0L, ProjectTaskCommand(ProjectTaskKind.Build, "make", Path.of("/tmp"), "make", Nil), "")

  private def withRunningTask(fixture: Harness): Unit =
    fixture.stateRef
      .update(state =>
        state.copy(runtime = state.runtime.copy(projectTasks = ProjectTasks(nextId = 1L, running = Some(runningTask))))
      )
      .unsafeRunSync()

  private def runningProjectTask(fixture: Harness): Option[RunningProjectTask] =
    fixture.stateRef.get.unsafeRunSync().runtime.projectTasks.running

  "StateManagerProjectLspEffects" should "refuse to run a project task in prose mode" in {
    val fixture = harness()
    val proseState = AppState.initial.copy(persisted =
      AppState.initial.persisted
        .copy(config = AppState.initial.persisted.config.withAppMode(com.serenity.config.AppMode.Prose))
    )

    fixture.effects.interpretProject(ProjectIntent.RunProjectTask(ProjectTaskKind.Build), proseState).unsafeRunSync()

    fixture.pinCalls.get.unsafeRunSync() shouldBe List(
      (ProjectTaskTerminal.notAvailableInProseMode(ProjectTaskKind.Build), PanelPosition.Bottom, 14)
    )
    runningProjectTask(fixture) shouldBe None
    fixture.submitted.get.unsafeRunSync() shouldBe Nil
  }

  it should "report no task found when no project marker is detected from the focused buffer's directory" in {
    val directory = Files.createTempDirectory("project-lsp-spec-empty")
    try
      val fixture      = harness()
      val filePath     = directory.resolve("notes.txt")
      val focusedState = stateFocusedOnFile(filePath)

      fixture.effects
        .interpretProject(ProjectIntent.RunProjectTask(ProjectTaskKind.Build), focusedState)
        .unsafeRunSync()

      fixture.pinCalls.get.unsafeRunSync() shouldBe List(
        (ProjectTaskTerminal.noTask(ProjectTaskKind.Build, filePath), PanelPosition.Bottom, 14)
      )
    finally Files.deleteIfExists(directory)
  }

  it should "detect and start the project task for the ecosystem marker found in the buffer's directory" in {
    val directory = Files.createTempDirectory("project-lsp-spec-make")
    val makefile  = directory.resolve("Makefile")
    try
      Files.writeString(makefile, "all:\n\ttrue\n")
      val fixture      = harness()
      val filePath     = directory.resolve("main.c")
      val focusedState = stateFocusedOnFile(filePath)
      fixture.stateRef.set(focusedState).unsafeRunSync()

      fixture.effects
        .interpretProject(ProjectIntent.RunProjectTask(ProjectTaskKind.Build), focusedState)
        .unsafeRunSync()

      val expectedCommand =
        ProjectTaskCommand(ProjectTaskKind.Build, "make", directory.toAbsolutePath.normalize(), "make", Nil)
      fixture.pinCalls.get.unsafeRunSync() shouldBe List(
        (ProjectTaskTerminal.started(expectedCommand), PanelPosition.Bottom, 14)
      )
      runningProjectTask(fixture) shouldBe Some(RunningProjectTask(0L, expectedCommand, ""))
      fixture.submitted.get.unsafeRunSync() shouldBe List(StateManagerProjectLspEffects.TaskLane)
    finally
      Files.deleteIfExists(makefile)
      Files.deleteIfExists(directory)
  }

  it should "refuse to start a second project task while one is already running" in {
    val fixture = harness()
    withRunningTask(fixture)

    fixture.effects
      .interpretProject(ProjectIntent.RunProjectTask(ProjectTaskKind.Build), AppState.initial)
      .unsafeRunSync()

    fixture.pinCalls.get.unsafeRunSync() shouldBe List(
      (
        "A project task is already running. Use Cancel Project Task before starting another.",
        PanelPosition.Bottom,
        14
      )
    )
    runningProjectTask(fixture) shouldBe Some(runningTask)
    fixture.submitted.get.unsafeRunSync() shouldBe Nil
  }

  it should "cancel a running project task and confirm the cancellation" in {
    val fixture = harness()
    withRunningTask(fixture)

    fixture.effects.interpretProject(ProjectIntent.CancelProjectTask, AppState.initial).unsafeRunSync()

    fixture.pinCalls.get.unsafeRunSync() shouldBe List(("Project task cancelled.", PanelPosition.Bottom, 14))
    runningProjectTask(fixture) shouldBe None
    fixture.submitted.get.unsafeRunSync() shouldBe List(StateManagerProjectLspEffects.TaskLane)
  }

  it should "report no project task running when cancel is requested with nothing active" in {
    val fixture = harness()

    fixture.effects.interpretProject(ProjectIntent.CancelProjectTask, AppState.initial).unsafeRunSync()

    fixture.pinCalls.get.unsafeRunSync() shouldBe List(("No project task is running.", PanelPosition.Bottom, 14))
  }

  it should "cancel a running project task silently, without pinning a confirmation" in {
    val fixture = harness()
    withRunningTask(fixture)

    fixture.effects.cancelProjectTaskSilently.unsafeRunSync()

    fixture.pinCalls.get.unsafeRunSync() shouldBe Nil
    runningProjectTask(fixture) shouldBe None
    fixture.submitted.get.unsafeRunSync() shouldBe List(StateManagerProjectLspEffects.TaskLane)
  }

  it should "enqueue an LSP hover request for the focused buffer's cursor" in {
    val fixture = harness()
    val path    = Path.of("/tmp/serenity-lsp-spec/example.scala")
    val cursor  = CursorPosition(3, 5)
    val state   = stateWithBuffer(bufferWithLanguage(Some(path), Some(LanguageId.Scala), "val x = 1", cursor))

    fixture.effects.interpretLsp(LspIntent.RequestLspHover, state).unsafeRunSync()

    val enqueued = fixture.lspQueue.stream.take(1).timeout(2.seconds).compile.toList.unsafeRunSync()
    enqueued shouldBe List(LspEffect.HoverRequested(path.toUri.toString, LanguageId.Scala, 3, 5, cursor))
    fixture.peeks.get.unsafeRunSync() shouldBe Nil
  }

  it should "enqueue an LSP completion request for the focused buffer's cursor" in {
    val fixture = harness()
    val path    = Path.of("/tmp/serenity-lsp-spec/example.py")
    val cursor  = CursorPosition(0, 2)
    val state   = stateWithBuffer(bufferWithLanguage(Some(path), Some(LanguageId.Python), "x = 1", cursor))

    fixture.effects.interpretLsp(LspIntent.RequestLspCompletion, state).unsafeRunSync()

    val enqueued = fixture.lspQueue.stream.take(1).timeout(2.seconds).compile.toList.unsafeRunSync()
    enqueued shouldBe List(LspEffect.CompletionRequested(path.toUri.toString, LanguageId.Python, 0, 2, cursor))
  }

  it should "enqueue an LSP definition request carrying the identifier under the cursor" in {
    val fixture = harness()
    val path    = Path.of("/tmp/serenity-lsp-spec/example.scala")
    val content = "val someValue = 42"
    val cursor  = CursorPosition(0, 8)
    val state   = stateWithBuffer(bufferWithLanguage(Some(path), Some(LanguageId.Scala), content, cursor))

    fixture.effects.interpretLsp(LspIntent.RequestLspDefinition, state).unsafeRunSync()

    val enqueued = fixture.lspQueue.stream.take(1).timeout(2.seconds).compile.toList.unsafeRunSync()
    enqueued shouldBe List(
      LspEffect.DefinitionRequested(path.toUri.toString, LanguageId.Scala, 0, 8, cursor, "someValue")
    )
  }

  it should "enqueue an LSP references request carrying the identifier under the cursor" in {
    val fixture = harness()
    val path    = Path.of("/tmp/serenity-lsp-spec/example.scala")
    val content = "val someValue = 42"
    val cursor  = CursorPosition(0, 8)
    val state   = stateWithBuffer(bufferWithLanguage(Some(path), Some(LanguageId.Scala), content, cursor))

    fixture.effects.interpretLsp(LspIntent.RequestLspReferences, state).unsafeRunSync()

    val enqueued = fixture.lspQueue.stream.take(1).timeout(2.seconds).compile.toList.unsafeRunSync()
    enqueued shouldBe List(
      LspEffect.ReferencesRequested(path.toUri.toString, LanguageId.Scala, 0, 8, cursor, "someValue")
    )
  }

  it should "show an unavailable peek instead of enqueueing an LSP references request when the buffer has no language mode" in {
    val fixture = harness()
    val path    = Path.of("/tmp/serenity-lsp-spec/plain.txt")
    val cursor  = CursorPosition(0, 0)
    val state   = stateWithBuffer(bufferWithLanguage(Some(path), None, "plain text", cursor))

    fixture.effects.interpretLsp(LspIntent.RequestLspReferences, state).unsafeRunSync()

    fixture.peeks.get.unsafeRunSync() shouldBe List(
      (PeekContent.QuickInfo("LSP requests need a saved buffer with a language mode."), cursor)
    )
  }

  it should "open the rename-symbol prompt carrying the identifier under the cursor" in {
    val fixture = harness()
    val path    = Path.of("/tmp/serenity-lsp-spec/example.scala")
    val content = "val someValue = 42"
    val cursor  = CursorPosition(0, 8)
    val state   = stateWithBuffer(bufferWithLanguage(Some(path), Some(LanguageId.Scala), content, cursor))

    fixture.effects.interpretLsp(LspIntent.OpenRenameSymbolPrompt, state).unsafeRunSync()

    fixture.modals.get.unsafeRunSync() shouldBe List(
      Modal.RenameSymbol(path.toUri.toString, LanguageId.Scala, 0, 8, cursor, "someValue")
    )
    fixture.peeks.get.unsafeRunSync() shouldBe Nil
  }

  it should "show an unavailable peek instead of opening the rename prompt when the buffer has no language mode" in {
    val fixture = harness()
    val path    = Path.of("/tmp/serenity-lsp-spec/plain.txt")
    val cursor  = CursorPosition(0, 0)
    val state   = stateWithBuffer(bufferWithLanguage(Some(path), None, "plain text", cursor))

    fixture.effects.interpretLsp(LspIntent.OpenRenameSymbolPrompt, state).unsafeRunSync()

    fixture.peeks.get.unsafeRunSync() shouldBe List(
      (PeekContent.QuickInfo("LSP requests need a saved buffer with a language mode."), cursor)
    )
    fixture.modals.get.unsafeRunSync() shouldBe Nil
  }

  it should "show an unavailable peek instead of enqueueing an LSP request when the buffer has no language mode" in {
    val fixture = harness()
    val path    = Path.of("/tmp/serenity-lsp-spec/plain.txt")
    val cursor  = CursorPosition(0, 0)
    val state   = stateWithBuffer(bufferWithLanguage(Some(path), None, "plain text", cursor))

    fixture.effects.interpretLsp(LspIntent.RequestLspHover, state).unsafeRunSync()

    fixture.peeks.get.unsafeRunSync() shouldBe List(
      (PeekContent.QuickInfo("LSP requests need a saved buffer with a language mode."), cursor)
    )
    val nothingEnqueued = runVirtual(fixture.lspQueue.stream.take(1).timeout(200.millis).compile.toList.attempt)
    nothingEnqueued.isLeft shouldBe true
  }
