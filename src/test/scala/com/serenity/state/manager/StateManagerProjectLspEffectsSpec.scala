package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.std.Semaphore
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.command.{LspIntent, ProjectIntent}
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.project.{ProjectTaskCommand, ProjectTaskKind, ProjectTaskTerminal}
import com.serenity.rope.{Balance, Rope}
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
      val projectTaskFiberRef: Ref[IO, Option[ManagedProjectTask]],
      val lspQueue: LspEffectQueue,
      val effects: StateManagerProjectLspEffects
  )

  private def harness(): Harness =
    val stateRef             = Ref.of[IO, AppState](AppState.initial).unsafeRunSync()
    val pinCalls             = Ref.of[IO, List[(String, PanelPosition, Int)]](Nil).unsafeRunSync()
    val peeks                = Ref.of[IO, List[(PeekContent, CursorPosition)]](Nil).unsafeRunSync()
    val projectTaskFiberRef  = Ref.of[IO, Option[ManagedProjectTask]](None).unsafeRunSync()
    val projectTaskSemaphore = Semaphore[IO](1).unsafeRunSync()
    val lspQueue             = LspEffectQueue.create.unsafeRunSync()

    new Harness(
      stateRef,
      pinCalls,
      peeks,
      projectTaskFiberRef,
      lspQueue,
      new StateManagerProjectLspEffects(
        lspQueue,
        projectTaskFiberRef,
        projectTaskSemaphore,
        (text, position, size) => pinCalls.update(_ :+ (text, position, size)),
        (content, cursor) => peeks.update(_ :+ (content, cursor))
      )
    )

  /** Polls `io` until `pred` holds, for behavior that lands via a forked fiber (project-task start) rather than
    * synchronously within the returned `IO`.
    */
  private def eventually[A](io: IO[A])(pred: A => Boolean): A =
    def loop(remaining: Int): IO[A] =
      io.flatMap { a =>
        if pred(a) || remaining <= 0 then IO.pure(a)
        else IO.sleep(20.millis) >> loop(remaining - 1)
      }
    loop(100).unsafeRunSync()

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
      .copy(editing = EditingState(cursors = List(cursor)))

  private def stateWithBuffer(buffer: Buffer): AppState =
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(BufferId(0) -> buffer)))

  private def runningManagedTask(): ManagedProjectTask =
    val finished = Deferred[IO, Unit].unsafeRunSync()
    val fiber    = IO.never[Unit].start.unsafeRunSync()
    ManagedProjectTask(finished, fiber)

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
    fixture.projectTaskFiberRef.get.unsafeRunSync() shouldBe None
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

      fixture.effects
        .interpretProject(ProjectIntent.RunProjectTask(ProjectTaskKind.Build), focusedState)
        .unsafeRunSync()

      val expectedCommand =
        ProjectTaskCommand(ProjectTaskKind.Build, "make", directory.toAbsolutePath.normalize(), "make", Nil)
      val pinned = eventually(fixture.pinCalls.get)(_.nonEmpty)
      pinned.head shouldBe (ProjectTaskTerminal.started(expectedCommand), PanelPosition.Bottom, 14)

      eventually(fixture.projectTaskFiberRef.get)(_.isDefined).isDefined shouldBe true

      // Clean up the forked task/renderer fiber rather than leaving it running past the test.
      fixture.effects.cancelProjectTaskSilently.unsafeRunSync()
    finally
      Files.deleteIfExists(makefile)
      Files.deleteIfExists(directory)
  }

  it should "refuse to start a second project task while one is already running" in {
    val fixture = harness()
    val running = runningManagedTask()
    fixture.projectTaskFiberRef.set(Some(running)).unsafeRunSync()

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
    fixture.projectTaskFiberRef.get.unsafeRunSync() shouldBe Some(running)
    running.fiber.cancel.unsafeRunSync()
  }

  it should "cancel a running project task and confirm the cancellation" in {
    val fixture = harness()
    val running = runningManagedTask()
    fixture.projectTaskFiberRef.set(Some(running)).unsafeRunSync()

    fixture.effects.interpretProject(ProjectIntent.CancelProjectTask, AppState.initial).unsafeRunSync()

    fixture.pinCalls.get.unsafeRunSync() shouldBe List(("Project task cancelled.", PanelPosition.Bottom, 14))
    fixture.projectTaskFiberRef.get.unsafeRunSync() shouldBe None
  }

  it should "report no project task running when cancel is requested with nothing active" in {
    val fixture = harness()

    fixture.effects.interpretProject(ProjectIntent.CancelProjectTask, AppState.initial).unsafeRunSync()

    fixture.pinCalls.get.unsafeRunSync() shouldBe List(("No project task is running.", PanelPosition.Bottom, 14))
  }

  it should "cancel a running project task silently, without pinning a confirmation" in {
    val fixture = harness()
    val running = runningManagedTask()
    fixture.projectTaskFiberRef.set(Some(running)).unsafeRunSync()

    fixture.effects.cancelProjectTaskSilently.unsafeRunSync()

    fixture.pinCalls.get.unsafeRunSync() shouldBe Nil
    fixture.projectTaskFiberRef.get.unsafeRunSync() shouldBe None
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
