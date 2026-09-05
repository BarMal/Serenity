package com.serenity.ui.tui

import java.io.{ByteArrayOutputStream, PipedInputStream, PipedOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.std.Queue
import cats.effect.{FiberIO, IO, Ref, Resource}
import cats.syntax.all.*
import com.serenity.app.{AppRuntime, AppStartup}
import com.serenity.input.{FocusedInputTranslator, InProcessClipboard, InputRouter, Osc52Clipboard, SystemClipboard}
import com.serenity.keystroke.events.{Event, MousePress}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.rope.Balance
import com.serenity.state.manager.{DamageProducer, StateManager}
import com.serenity.state.models.{AppState, Damage}
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.RenderController
import com.serenity.ui.theme.config.AppThemeManager
import org.jline.terminal.Size
import org.jline.terminal.impl.DumbTerminal
import org.typelevel.log4cats.{Logger, LoggerFactory}

/** A running TUI session under test: a real JLine terminal over in-memory streams, wrapped in the real
  * [[TerminalShell]], fed by the real [[TerminalInputHandler]], translated by the real `InputRouter`, applied to a real
  * `StateManager` through `AppRuntime`'s own input phase, and painted by [[TuiRuntime]]'s own paint call onto a real
  * [[TerminalRenderSurface]]. The only thing this harness supplies itself is the loop: everything a byte passes through
  * on its way to a cell is production code.
  *
  * Input is delivered synchronously. Each [[feed]] writes its bytes followed by a sentinel the input handler cannot
  * translate away, and returns once that sentinel has come back out the other end of the event pipeline -- so by the
  * time `feed` completes, every event those bytes produced has been applied to the state manager, with no polling and
  * no sleeping.
  */
final class TuiSession private (
    shell: TerminalShell,
    handler: TerminalInputHandler,
    surfaces: TuiRuntime.SurfaceHolder,
    output: ByteArrayOutputStream,
    input: PipedOutputStream,
    terminal: DumbTerminal,
    sentinels: Queue[IO, Unit],
    appliedSignals: Queue[IO, Unit],
    damage: Ref[IO, Damage],
    screenRef: Ref[IO, TerminalEmulator],
    consumed: Ref[IO, Int],
    applied: Ref[IO, Vector[Event]],
    val stateManager: StateManager,
    val clipboard: SystemClipboard[IO],
    val workspace: Path
)(using Balance):
  import TuiSession.*

  def state: IO[AppState] = stateManager.getCurrentState

  def eventsApplied: IO[Vector[Event]] = applied.get

  /** Deliver a key's bytes and wait until every event they produced has been applied. */
  def feed(key: TuiKey): IO[Unit] =
    if endsOnLoneEscape(key.bytes) then feedEscape(key)
    else write(key.bytes) >> write(SentinelKey.bytes) >> awaitSentinel(key)

  /** A lone `ESC` is genuinely ambiguous at a real terminal: the input handler holds it for
    * [[TerminalInputHandler.EscDisambiguationDeadline]] to see whether an escape sequence follows. Sending the sentinel
    * during that window would let the handler read `ESC` and the sentinel's own `CSI` introducer as one sequence, and
    * the Escape keystroke would vanish -- so the sentinel is held back until the Escape has actually been applied,
    * which is an observation rather than a sleep long enough to hope.
    */
  private def feedEscape(key: TuiKey): IO[Unit] =
    require(
      key.bytes.length == 1,
      s"${key.name} ends on a lone ESC but carries other bytes too; press Escape on its own so it can be disambiguated"
    )
    for
      _ <- appliedSignals.tryTakeN(None)
      _ <- write(key.bytes)
      _ <- appliedSignals.take.timeoutTo(
        SentinelTimeout,
        IO.raiseError(new AssertionError(s"Timed out waiting for ${key.name} to be applied"))
      )
      _ <- write(SentinelKey.bytes)
      _ <- awaitSentinel(key)
    yield ()

  def feedAll(keys: Seq[TuiKey]): IO[Unit] = keys.toList.traverse_(feed)

  /** Paint one frame exactly as the runtime's render loop would, and return what the terminal now shows. */
  def screen: IO[TuiScreen] = renderFrame(cursorVisible = true)

  def screenWithoutCaret: IO[TuiScreen] = renderFrame(cursorVisible = false)

  /** One frame painted the way `AppRuntime`'s fast render phase paints it, branch included: the cursor-only path while
    * the window sitter is active and nothing else needs a full content repaint, the full path otherwise.
    *
    * [[screen]] always paints the full frame, which is what most scenarios want -- what is on screen once the runtime
    * has caught up. This one reproduces the runtime's own per-frame choice, and so can show a frame the user would
    * actually see mid-typing.
    */
  def runtimeScreen: IO[TuiScreen] =
    for
      size       <- shell.viewportSize
      current    <- state
      animations <- stateManager.getBufferAnimations
      pending    <- damage.getAndSet(Damage.Nothing)
      surface    = surfaces.forSize(size)
      cursorOnly = AppRuntime.canStandDownToCursorOnly(current, animations, pending)
      _ <- IO(
        if cursorOnly then TuiRuntime.paintCursorOnly(current, surface, size, true, None, animations)
        else TuiRuntime.paintFrame(current, surface, size, true, None, pending)
      )
      emitted <- drainOutput
      updated <- screenRef.get
    yield TuiScreen(updated, emitted)

  /** Advance the animation clock until the runtime would paint content again -- that is, until the window sitter has
    * finished its run. How many ticks that takes depends on how fast the keystrokes arrived
    * (`WindowSitterConfig.fastActiveTicks` against `activeTicks`), so scenarios wait for the branch rather than
    * guessing a count.
    */
  def advanceUntilFullRepaint: IO[Unit] =
    def loop(remaining: Int): IO[Unit] =
      paintsCursorOnly.flatMap {
        case false => IO.unit
        case _ if remaining <= 0 =>
          IO.raiseError(new AssertionError(s"Window sitter still active after $AnimationTickLimit animation ticks"))
        case _ => tickAnimations >> loop(remaining - 1)
      }
    loop(AnimationTickLimit)

  /** Whether the runtime's fast phase would paint the cursor-only path on the next frame -- asked of `AppRuntime`
    * itself rather than restated here, so a change to that decision shows up in these scenarios instead of leaving the
    * harness describing a branch production no longer takes.
    */
  def paintsCursorOnly: IO[Boolean] =
    (state, stateManager.getBufferAnimations, damage.get).mapN(AppRuntime.canStandDownToCursorOnly)

  /** Let the interface finish moving, then paint until the frame stops changing.
    *
    * Two things settle here. Surfaces animate in and out (`AppState.runtime.surfaceAnimations`), so a dismissed command
    * palette is still drawn for as many frames as its exit animation lasts -- exactly as in a real session, where the
    * render loop advances one animation tick per painted frame. And a frame painted with `Damage.Everything` -- the
    * first of a session, and the first after a resize -- is followed by one further frame that rewrites blank cells
    * whose foreground colour differed invisibly.
    *
    * Scenarios that assert on what is finally on screen, or on emitted bytes, want this rather than a single frame.
    */
  def settledScreen: IO[TuiScreen] =
    advanceAnimationsToRest(AnimationTickLimit) >> repaintUntilQuiet(SettleAttempts)

  /** Advance the animation clock by `ticks` frames, folding in the damage each one produces, exactly as the render
    * loop's fast phase does per painted frame. Returns whether anything is still animating.
    */
  def advanceAnimations(ticks: Int): IO[Boolean] =
    (0 until ticks).toList.foldLeft(IO.pure(false))((previous, _) => previous >> tickAnimations)

  def animationsActive: IO[Boolean] =
    (state, stateManager.getBufferAnimations).mapN(AppRuntime.hasActiveAnimations)

  private def tickAnimations: IO[Boolean] =
    for
      before           <- state
      beforeAnimations <- stateManager.getBufferAnimations
      stillActive      <- stateManager.advanceAnimationsOnTick()
      after            <- state
      afterAnimations  <- stateManager.getBufferAnimations
      _ <- damage.update(_ |+| DamageProducer.forTransition(before, after, beforeAnimations, afterAnimations))
    yield stillActive

  private def advanceAnimationsToRest(remaining: Int): IO[Unit] =
    if remaining <= 0 then IO.unit
    else tickAnimations.flatMap(active => if active then advanceAnimationsToRest(remaining - 1) else IO.unit)

  private def repaintUntilQuiet(remaining: Int): IO[TuiScreen] =
    screen.flatMap { current =>
      if current.emitted.isEmpty || remaining <= 0 then IO.pure(current)
      else repaintUntilQuiet(remaining - 1)
    }

  /** Resize the terminal underneath the session, the way a window manager does: the shell observes `SIGWINCH`, the
    * runtime picks the new size up through `checkResize`, and the surface is rebuilt for it.
    */
  def resize(size: ViewportSize): IO[Unit] =
    for
      _ <- IO(terminal.setSize(new Size(size.width, size.height)))
      _ <- IO(shell.handleWinch())
      _ <- checkResizeAndHandle
      _ <- drainOutput
      _ <- screenRef.set(TerminalEmulator.blank(size.width, size.height))
    yield ()

  def viewportSize: IO[ViewportSize] = shell.viewportSize

  /** Everything the session has written to the terminal, from startup onwards. */
  def allBytesWritten: IO[String] = IO(output.toString(StandardCharsets.UTF_8))

  private def renderFrame(cursorVisible: Boolean): IO[TuiScreen] =
    for
      size    <- shell.viewportSize
      current <- state
      pending <- damage.getAndSet(Damage.Nothing)
      surface = surfaces.forSize(size)
      _       <- IO(TuiRuntime.paintFrame(current, surface, size, cursorVisible, None, pending))
      emitted <- drainOutput
      updated <- screenRef.get
    yield TuiScreen(updated, emitted)

  /** Pull whatever the session has written since the last drain into the mock terminal, and return it. Called after
    * every render and every feed, so escapes written outside a paint -- the shell's own startup sequences, an OSC 52
    * clipboard write -- reach the emulator too rather than being silently skipped.
    */
  private def drainOutput: IO[String] =
    for
      offset <- consumed.get
      bytes  <- IO(output.toByteArray)
      fresh = new String(bytes.drop(offset), StandardCharsets.UTF_8)
      _ <- consumed.set(bytes.length)
      _ <- screenRef.update(_.consume(fresh)).whenA(fresh.nonEmpty)
    yield fresh

  private def write(bytes: Array[Byte]): IO[Unit] =
    IO.blocking {
      input.write(bytes)
      input.flush()
    }

  private def awaitSentinel(key: TuiKey): IO[Unit] =
    sentinels.take
      .timeoutTo(
        SentinelTimeout,
        IO.raiseError(
          new AssertionError(
            s"Timed out after $SentinelTimeout waiting for input to settle after ${key.name}. " +
              "The input pipeline never delivered the harness sentinel, so the keystroke's events may never have " +
              "been applied."
          )
        )
      ) >> drainOutput.void

  /** Stops the input handler's read loop and disables the input modes it enabled, exactly as the runtime's own shutdown
    * does, before [[TerminalShell]]'s release restores the terminal.
    */
  private[tui] def handlerShutdown: IO[Unit] = handler.shutdown.attempt.void

  private[tui] def checkResizeAndHandle: IO[Unit] =
    shell.checkResize.flatMap(RenderController.handleResize(_, stateManager, damage.update(_ |+| Damage.Everything)))

object TuiSession:

  /** How long [[TuiSession.feed]] waits for the input pipeline to settle before failing the test outright. Generous: it
    * is a deadlock detector, not a timing dependency -- the sentinel normally arrives in microseconds.
    */
  private val SentinelTimeout: FiniteDuration = 15.seconds

  /** How many paints [[TuiSession.settledScreen]] will make before giving up and returning the latest frame. A frame
    * settles after one repaint in practice; more than a handful would itself be the bug worth seeing.
    */
  private val SettleAttempts = 8

  /** A ceiling on how many animation frames [[TuiSession.settledScreen]] will advance before giving up. Surface
    * animations run for a fraction of a second at the configured frame rate; a scenario needing more than this is
    * either animating forever or waiting for something that is not an animation.
    */
  private val AnimationTickLimit = 600

  /** A mouse press so far off-screen that nothing can be under it. It is the input barrier every [[TuiSession.feed]]
    * ends with: a mouse report is delivered as a direct event, never passed through a translator, so unlike any key it
    * cannot be reinterpreted by whichever surface currently has focus -- and it is dropped before reaching the state
    * manager, so the application never sees it.
    */
  private val SentinelCol = 9997
  private val SentinelRow = 9997

  val SentinelKey: TuiKey = TuiKeys.mousePress(SentinelCol, SentinelRow)

  private def isSentinel(event: Event): Boolean = event match
    case press: MousePress => press.col == SentinelCol && press.row == SentinelRow
    case _                 => false

  private def endsOnLoneEscape(bytes: Array[Byte]): Boolean =
    TerminalInputDecoder.decode(bytes).remainder.toList == List(0x1b.toByte)

  private def deleteRecursively(root: Path): Unit =
    if Files.exists(root) then
      Files
        .walk(root)
        .sorted(java.util.Comparator.reverseOrder[Path]())
        .forEach(path => scala.util.Try(Files.delete(path)).fold(_ => (), _ => ()))

  final private case class Streams(terminal: DumbTerminal, input: PipedOutputStream, output: ByteArrayOutputStream)

  /** A pipe wide enough that feeding a large paste or a long line never blocks the writer waiting on the reader. */
  private val InputPipeBytes = 1 << 16

  private def openTerminal(size: ViewportSize): IO[Streams] =
    IO.blocking {
      val readEnd  = new PipedInputStream(InputPipeBytes)
      val writeEnd = new PipedOutputStream(readEnd)
      val output   = new ByteArrayOutputStream()
      val terminal = new DumbTerminal("tui-session", "xterm-256color", readEnd, output, StandardCharsets.UTF_8)
      terminal.setSize(new Size(size.width, size.height))
      Streams(terminal, writeEnd, output)
    }

  /** Start a session and tear it down afterwards: the terminal is restored through [[TerminalShell]]'s own release, the
    * input handler's read loop and the event consumer are cancelled, and the temporary workspace is deleted.
    */
  def resource(environment: TuiEnvironment)(using
    Logger[IO],
    LoggerFactory[IO],
    Balance
  ): Resource[IO, TuiSession] =
    for
      workspace <- Resource.make(IO.blocking(Files.createTempDirectory("tui-session")))(root =>
        IO.blocking(deleteRecursively(root)).attempt.void
      )
      streams <- Resource.eval(openTerminal(environment.viewport))
      shell   <- TerminalShell.forTerminal(streams.terminal)
      built   <- Resource.eval(assemble(environment, workspace, streams, shell))
      _       <- Resource.make(built.consumer.start)((fiber: FiberIO[Unit]) => fiber.cancel)
      _       <- Resource.onFinalize(built.session.handlerShutdown)
    yield built.session

  final private case class Built(session: TuiSession, consumer: IO[Unit])

  private def assemble(
    environment: TuiEnvironment,
    workspace: Path,
    streams: Streams,
    shell: TerminalShell
  )(using logger: Logger[IO], loggerFactory: LoggerFactory[IO], balance: Balance): IO[Built] =
    val terminalConfig =
      environment.config.withHotkeyConfig(environment.config.inputConfig.hotkeyConfig.forTerminalUse)
    for
      openPath  <- environment.materialise(workspace)
      clipboard <- buildClipboard(shell, environment)
      router    <- InputRouter.create[IO, Event](new TextEntryTranslator(terminalConfig))
      handler <- TerminalInputHandler.create(
        streams.terminal,
        router,
        clipboard,
        shell.pendingInputPrefix,
        environment.config.inputConfig.wheelScrollLines,
        environment.escDeadline
      )
      stateManager <- TuiRuntime.makeStateManager(
        terminalConfig,
        sessionRootOverride = Some(workspace.resolve("session")),
        configPersistencePath = None,
        previewWindowAvailability = MarkdownPreviewWindowAvailability.Unavailable
      )(logger)
      theme    <- AppStartup.startupTheme(stateManager, AppThemeManager.create)
      viewport <- shell.viewportSize
      initialState <- AppStartup.initializeState(
        stateManager,
        theme,
        viewport,
        terminalConfig,
        openPath,
        isTuiMode = true,
        keyboardFidelityTier = TuiRuntime.keyboardFidelityTier(shell.keyboardProtocolTier)
      )
      _              <- router.setActiveTranslator(FocusedInputTranslator.forState(initialState))
      sentinels      <- Queue.unbounded[IO, Unit]
      appliedSignals <- Queue.unbounded[IO, Unit]
      damage         <- Ref.of[IO, Damage](Damage.Everything)
      screenRef      <- Ref.of[IO, TerminalEmulator](TerminalEmulator.blank(viewport.width, viewport.height))
      consumed       <- Ref.of[IO, Int](0)
      applied        <- Ref.of[IO, Vector[Event]](Vector.empty)
      cursorVisible  <- Ref.of[IO, Boolean](true)
      breathIndex    <- Ref.of[IO, Int](0)
      session = new TuiSession(
        shell = shell,
        handler = handler,
        surfaces = new TuiRuntime.SurfaceHolder(shell),
        output = streams.output,
        input = streams.input,
        terminal = streams.terminal,
        sentinels = sentinels,
        appliedSignals = appliedSignals,
        damage = damage,
        screenRef = screenRef,
        consumed = consumed,
        applied = applied,
        stateManager = stateManager,
        clipboard = clipboard,
        workspace = workspace
      )
      funnel = AppRuntime.inputEventPhase(
        stateManager,
        router,
        clipboard,
        session.checkResizeAndHandle,
        cursorVisible,
        breathIndex,
        (next: Damage) => damage.update(_ |+| next)
      )
    yield Built(session, consume(handler, funnel, sentinels, applied, appliedSignals))

  /** The event pipeline, shaped exactly like the runtime's: every event goes through `AppRuntime`'s own input phase,
    * one at a time, so the focused translator is refreshed between events the way it is in a real session. The sentinel
    * is the one event that is counted rather than applied.
    */
  private def consume(
    handler: TerminalInputHandler,
    funnel: fs2.Stream[IO, Event] => fs2.Stream[IO, Unit],
    sentinels: Queue[IO, Unit],
    applied: Ref[IO, Vector[Event]],
    appliedSignals: Queue[IO, Unit]
  ): IO[Unit] =
    handler.eventStream
      .flatMap { event =>
        if isSentinel(event) then fs2.Stream.eval(sentinels.offer(()))
        else
          fs2.Stream.emit(event).through(funnel) ++
            fs2.Stream.eval(applied.update(_ :+ event) >> appliedSignals.offer(()))
      }
      .compile
      .drain

  /** The clipboard a headless terminal session would resolve to: OSC 52 through the terminal's own writer, with an
    * in-process clipboard behind it for reads -- so a copy is observable as a real escape sequence on the wire, exactly
    * as it would be over SSH.
    */
  private def buildClipboard(shell: TerminalShell, environment: TuiEnvironment)(using
    Logger[IO]
  ): IO[SystemClipboard[IO]] =
    InProcessClipboard[IO].map { inProcess =>
      if environment.useOsc52Clipboard then
        Osc52Clipboard[IO](
          write = text => IO.blocking { shell.writer.write(text); shell.writer.flush() },
          fallback = inProcess
        )
      else inProcess
    }

end TuiSession
