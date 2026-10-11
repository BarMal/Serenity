package com.serenity.app

import java.nio.file.{Files, Path}
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext

import cats.effect.{Deferred, IO, Ref, Resource}
import cats.syntax.all.*
import com.serenity.config.AppConfig
import com.serenity.diagnostics.FrameTimings
import com.serenity.frontend.FrontendRuntime
import com.serenity.input.{InputRouter, PendingInput, SystemClipboard}
import com.serenity.io.DirectoryTree
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.spellcheck.{DictionaryCache, DictionaryLoader}
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{AppState, Buffer, CursorPosition, Damage, EditingState, Focus}
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import fs2.{Chunk, Stream}
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

  private val BurstKeys  = 40
  private val SingleKeys = 12
  private val TakenBack  = 6
  private val LetterSeed = 1931L

  private val movements: List[Event] = List(
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

  /** Each round is the shapes input takes in the app, one input batch each: a burst of typed keys, keys arriving one at
    * a time, some taken back, then the moves a writer makes most. The letters never repeat from one round to the next,
    * so wrapping and measuring see text they have not cached.
    */
  def script(plan: Plan): List[List[PendingInput]] =
    val random = new scala.util.Random(LetterSeed)
    def keystroke(): PendingInput =
      val character = if random.nextInt(6) == 0 then ' ' else ('a' + random.nextInt(26)).toChar
      PendingInput.Keystroke(KeyStrokeInfo(InputKey.Character, Some(character), Set.empty))
    def round: List[List[PendingInput]] =
      val burst    = List.fill(BurstKeys)(keystroke())
      val singles  = List.fill(SingleKeys)(List(keystroke()))
      val takeBack = List.fill(TakenBack)(PendingInput.Ready(DeleteBackward))
      burst :: singles ::: takeBack :: movements.map(event => List(PendingInput.Ready(event)))
    List.fill(plan.rounds)(()).flatMap(_ => round)

  def pendingInputInterruptsWarmUp(input: PendingInput): Boolean =
    input match
      case PendingInput.Keystroke(_) => true
      case PendingInput.Ready(event) => interruptsWarmUp(event)

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

  /** `state` with `content` as the active pane's buffer, the caret near the top and the pane focused. The content
    * version moves on, or a commit that replaces the content is rejected and the warm-up types into an empty document.
    */
  def seeded(state: AppState, content: String)(using Balance): Option[AppState] =
    for
      paneId   <- state.persisted.layout.activeEditorPaneId
      bufferId <- state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId)
      buffer   <- state.persisted.buffers.get(bufferId)
    yield
      val fresh = Buffer.fromString(bufferId, content)
      val filled = fresh.copy(
        document = fresh.document.copy(contentVersion = buffer.document.contentVersion + 1),
        viewport = buffer.viewport,
        editing = EditingState(List(CursorPosition(2, 0)))
      )
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
    plan: Plan = Plan.default,
    scratchRoot: Path = systemTempDirectory,
    dictionaryCache: DictionaryCache = DictionaryCache()
  )(using Balance): IO[Outcome] =
    lowPriorityThread.use { warmUpThread =>
      val warmUp = (
        throwawayEditor(config, theme, viewport, document(plan.paragraphs), scratchRoot, dictionaryCache),
        frames
      ).tupled
        .use((editor, offscreen) => exercise(editor, offscreen, config, plan, firstInput))
        .evalOn(warmUpThread)
      IO.race(firstInput.get, warmUp).map(_.fold(_ => Outcome.Interrupted, identity))
    }

  /** Checks for input before every batch as well as racing it: the race only cancels once its fiber is scheduled, and
    * on a loaded machine that is late enough for several more batches to compete with the user's first keystrokes. Each
    * batch goes through the input loop's own phase, so what is drawn is what that path produced.
    */
  private def exercise(
    editor: StateManager,
    frames: FrontendRuntime.OffscreenFrames,
    config: AppConfig,
    plan: Plan,
    firstInput: Deferred[IO, Unit]
  )(using Balance): IO[Outcome] =
    for
      router          <- InputRouter.create[IO, Event](new TextEntryTranslator(config))
      cursorVisible   <- Ref.of[IO, Boolean](true)
      translatorCache <- Ref.of[IO, Option[AppRuntimeRenderLoops.FocusedTranslatorCacheEntry]](None)
      damage          <- Ref.of[IO, Damage](Damage.Nothing)
      context = AppRuntimeRenderLoops.InputBatchContext(
        editor,
        router,
        SystemClipboard[IO](readText = IO.pure(None), writeText = _ => IO.unit),
        IO.unit,
        cursorVisible,
        emitted => damage.update(_ |+| emitted),
        translatorCache,
        FrameTimings()
      )
      apply = (batch: List[PendingInput]) =>
        Stream.emit(Chunk.from(batch)).through(AppRuntimeRenderLoops.inputBatchPhase(context)).compile.drain
      draw = for
        painted <- damage.getAndSet(Damage.Nothing)
        state   <- editor.getCurrentState
        _       <- frames.full(state, painted, editor.renderCaches)
        _       <- frames.cursorOnly(state, editor.renderCaches)
        _       <- IO.cede
      yield ()
      outcome <- remaining(script(plan), 0, firstInput, apply(_) >> draw)
    yield outcome

  private def remaining(
    batches: List[List[PendingInput]],
    done: Int,
    firstInput: Deferred[IO, Unit],
    step: List[PendingInput] => IO[Unit]
  ): IO[Outcome] =
    batches match
      case Nil => IO.pure(Outcome.Completed(done))
      case batch :: rest =>
        firstInput.tryGet.flatMap {
          case Some(_) => IO.pure(Outcome.Interrupted)
          case None    => step(batch) >> remaining(rest, done + 1, firstInput, step)
        }

  /** Parses the configured spell-check dictionaries into `cache` ahead of the first analysis, which would otherwise
    * start the parse (hundreds of milliseconds of allocation) in the middle of the first keystrokes. Analysis runs off
    * the typing path, so a keystroke arriving meanwhile waits for nothing; its diagnostics just appear once the
    * dictionary is in.
    */
  def preloadSpellDictionary(config: AppConfig, cache: DictionaryCache): IO[Unit] =
    val spellCheck = config.languageToolsConfig.spellCheck
    IO.whenA(spellCheck.enabled)(IO.blocking(DictionaryLoader.loadSnapshot(spellCheck, cache)).void)

  def systemTempDirectory: Path = Path.of(System.getProperty("java.io.tmpdir"))

  private def throwawayEditor(
    config: AppConfig,
    theme: Theme,
    viewport: ViewportSize,
    content: String,
    scratchRoot: Path,
    dictionaryCache: DictionaryCache
  )(using Balance): Resource[IO, StateManager] =
    given LoggerFactory[IO] = NoOpFactory[IO]
    val noSessionWrites     = SessionManager.SessionPolicy(saveOnFileChange = false, saveOnAppClose = false)
    for
      sessionRoot <- Resource.make(IO.blocking(Files.createTempDirectory(scratchRoot, "serenity-warm-up")))(
        DirectoryTree.deleteRecursively
      )
      editor <- Resource.make(
        StateManager(
          NoOpLogger[IO],
          noSessionWrites,
          sessionRootOverride = Some(sessionRoot),
          initialConfig = config,
          dictionaryCache = dictionaryCache
        )
      )(_.runtimeLifecycle.forceQuit)
      _ <- Resource.eval(
        editor.updateStateValidated(state => state.copy(persisted = state.persisted.copy(theme = theme))) >>
          editor.applyEvent(ResizeEvent(viewport)) >>
          editor.updateStateValidated(state => seeded(state, content).getOrElse(state))
      )
    yield editor

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
