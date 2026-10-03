package com.serenity.app

import java.nio.file.{Files, Path}
import java.util.Comparator
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext

import cats.effect.{Deferred, IO, Resource}
import cats.syntax.all.*
import com.serenity.config.AppConfig
import com.serenity.frontend.FrontendRuntime
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.manager.{DamageProducer, StateManager}
import com.serenity.state.models.{AppState, Buffer, CursorPosition, EditingState, Focus}
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.{NoOpFactory, NoOpLogger}

/** Runs the typing hot paths once the first frame is up, so the JIT has compiled them before the user's first
  * keystrokes would otherwise pay for it.
  *
  * Everything happens on a throwaway editor -- its own `StateManager`, session directory and render caches, seeded with
  * a synthetic document -- drawn into a surface that is never presented. The user's state, undo history, files and
  * language servers are never reachable from here.
  */
object StartupWarmUp:

  final case class Plan(rounds: Int, paragraphs: Int)

  object Plan:
    val default: Plan = Plan(rounds = 12, paragraphs = 60)

  enum Outcome:
    case Completed(steps: Int)
    case Interrupted

  /** Typing a word and taking it back, then the moves a writer makes most. */
  val round: List[Event] =
    "lorem ".toList.map(InsertChar(_)) ++ List.fill(6)(DeleteBackward) ++ List(
      MoveDown,
      MoveDown,
      MoveWordRight,
      MoveRight,
      NewLine,
      DeleteBackward,
      MoveUp,
      MoveUp,
      MoveWordLeft,
      MoveLeft,
      PageDown,
      PageUp
    )

  /** Pointer hovering and system notifications are not someone starting to work. */
  def interruptsWarmUp(event: Event): Boolean =
    event match
      case _: MouseMove | _: LspEvent | _: ResizeEvent => false
      case _                                           => true

  private val Words = Vector(
    "lorem",
    "ipsum",
    "dolor",
    "sit",
    "amet",
    "consectetur",
    "adipiscing",
    "elit",
    "sed",
    "do",
    "eiusmod",
    "tempor",
    "incididunt",
    "ut",
    "labore",
    "et",
    "dolore",
    "magna",
    "aliqua",
    "enim",
    "ad",
    "minim",
    "veniam",
    "quis",
    "nostrud",
    "exercitation",
    "ullamco",
    "laboris"
  )

  /** Paragraphs of 40-120 words separated by blank lines, the same every time. */
  def document(paragraphs: Int): String =
    val random = new scala.util.Random(1798L)
    List
      .fill(paragraphs) {
        Vector.fill(40 + random.nextInt(81))(Words(random.nextInt(Words.size))).mkString(" ").capitalize + "."
      }
      .mkString("\n\n")

  /** `state` with `content` as the active pane's buffer, the caret near the top and the pane focused. */
  def seeded(state: AppState, content: String)(using Balance): Option[AppState] =
    for
      paneId   <- state.persisted.layout.activeEditorPaneId
      bufferId <- state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId)
      buffer   <- state.persisted.buffers.get(bufferId)
    yield
      val filled = Buffer
        .fromString(bufferId, content)
        .copy(viewport = buffer.viewport, editing = EditingState(List(CursorPosition(2, 0))))
      state.copy(persisted =
        state.persisted.copy(
          buffers = state.persisted.buffers.updated(bufferId, filled),
          focus = Focus.EditorPane(paneId)
        )
      )

  /** Warms up until `plan` is done or `firstInput` completes, whichever is first. */
  def run(
    config: AppConfig,
    theme: Theme,
    viewport: ViewportSize,
    frames: Resource[IO, FrontendRuntime.OffscreenFrames],
    firstInput: Deferred[IO, Unit],
    plan: Plan = Plan.default
  )(using Balance): IO[Outcome] =
    lowPriorityThread.use { warmUpThread =>
      val warmUp = (throwawayEditor(config, theme, viewport, document(plan.paragraphs)), frames).tupled
        .use((editor, offscreen) => exercise(editor, offscreen, plan, firstInput))
        .evalOn(warmUpThread)
      IO.race(firstInput.get, warmUp).map(_.fold(_ => Outcome.Interrupted, identity))
    }

  /** Checks for input before every step as well as racing it: the race only cancels once its fiber is scheduled, and on
    * a loaded machine that is late enough for several more steps to compete with the user's first keystrokes.
    */
  private def exercise(
    editor: StateManager,
    frames: FrontendRuntime.OffscreenFrames,
    plan: Plan,
    firstInput: Deferred[IO, Unit]
  )(using Balance): IO[Outcome] =
    def step(event: Event): IO[Unit] =
      for
        before <- editor.getCurrentState
        _      <- editor.applyEvent(event)
        after  <- editor.getCurrentState
        _      <- frames.full(after, DamageProducer.forTransition(before, after), editor.renderCaches)
        _      <- frames.cursorOnly(after, editor.renderCaches)
        _      <- IO.cede
      yield ()
    def remaining(events: List[Event], steps: Int): IO[Outcome] =
      events match
        case Nil => IO.pure(Outcome.Completed(steps))
        case event :: rest =>
          firstInput.tryGet.flatMap {
            case Some(_) => IO.pure(Outcome.Interrupted)
            case None    => step(event) >> remaining(rest, steps + 1)
          }
    remaining(List.fill(plan.rounds)(round).flatten, 0)

  private def throwawayEditor(config: AppConfig, theme: Theme, viewport: ViewportSize, content: String)(using
    Balance
  ): Resource[IO, StateManager] =
    given LoggerFactory[IO] = NoOpFactory[IO]
    val noSessionWrites     = SessionManager.SessionPolicy(saveOnFileChange = false, saveOnAppClose = false)
    for
      sessionRoot <- Resource.make(IO.blocking(Files.createTempDirectory("serenity-warm-up")))(deleteTree)
      editor <- Resource.make(
        StateManager(NoOpLogger[IO], noSessionWrites, sessionRootOverride = Some(sessionRoot), initialConfig = config)
      )(_.runtimeLifecycle.forceQuit)
      _ <- Resource.eval(
        editor.updateStateValidated(state => state.copy(persisted = state.persisted.copy(theme = theme))) >>
          editor.applyEvent(ResizeEvent(viewport)) >>
          editor.updateStateValidated(state => seeded(state, content).getOrElse(state))
      )
    yield editor

  private def deleteTree(root: Path): IO[Unit] =
    IO.blocking {
      val paths = Files.walk(root)
      try paths.sorted(Comparator.reverseOrder[Path]()).forEach(path => Files.deleteIfExists(path): Unit)
      finally paths.close()
    }

  // Linux ignores Java thread priorities unless the JVM is started with -XX:ThreadPriorityPolicy=1; stopping at the
  // first real input is what actually keeps this out of the way.
  private val lowPriorityThread: Resource[IO, ExecutionContext] =
    Resource
      .make(IO(Executors.newSingleThreadExecutor { runnable =>
        val thread = new Thread(runnable, "serenity-warm-up")
        thread.setDaemon(true)
        thread.setPriority(Thread.MIN_PRIORITY)
        thread
      }))(executor => IO(executor.shutdown()))
      .map(ExecutionContext.fromExecutorService)
