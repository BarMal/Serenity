package com.serenity.app

import java.awt.Color

import scala.concurrent.duration.*

import cats.effect.*
import cats.syntax.foldable.*
import com.serenity.config.AppConfig
import com.serenity.diagnostics.{FrameTimings, KeyLatencyTrace, Trace}
import com.serenity.input.*
import com.serenity.keystroke.events.Event
import com.serenity.state.manager.*
import com.serenity.state.models.{AppState, Damage}
import fs2.concurrent.SignallingRef
import fs2.{Chunk, Stream}
import org.typelevel.log4cats.Logger

/** The render loop internals `AppRuntime.run` drives: the idle phase (cursor-only ticks while nothing changed), the
  * fast phase (one full-content frame per burst of damage), and the diagnostics/supervision wrappers both share. Split
  * out of `AppRuntime.scala` (which stayed the orchestration entry point, buffer-load/quit wiring, and the background
  * loops) purely to keep both files under this repo's architecture-ratchet file-length limit -- no behavior changed by
  * this split.
  */
private[serenity] object AppRuntimeRenderLoops:

  /** Blinks the caret until the cursor blink timeout passes, then holds it solid and parks on `fastModeSignal` with no
    * timer left to wake it (#1883).
    */
  private[serenity] def idleRenderPhase(
    loadModel: IO[Model],
    fastModeSignal: SignallingRef[IO, Boolean],
    windowFocused: SignallingRef[IO, Boolean],
    pendingPaintDamage: Ref[IO, Damage],
    currentStateForDiagnostics: IO[Option[AppState]],
    checkResizeAndHandle: IO[Unit],
    cursorVisible: Ref[IO, Boolean],
    renderCursorOnly: AppRuntime.RenderFn,
    requestFastRender: IO[Unit],
    cursorIdleInterval: AppConfig => Option[FiniteDuration],
    renderCaches: com.serenity.state.manager.RenderCaches
  )(using Logger[IO]): Stream[IO, Unit] =
    Stream
      .eval(Ref.of[IO, Int](0))
      .flatMap(blinks =>
        blinkTicks(loadModel, windowFocused, cursorIdleInterval, blinks) ++
          Stream.emit(IdleTick.HoldSolid) ++ Stream.never[IO]
      )
      .interruptWhen(fastModeSignal.discrete)
      .evalMap {
        case IdleTick.Blink =>
          runIdleRenderStep(
            currentStateForDiagnostics,
            loadModel,
            pendingPaintDamage,
            checkResizeAndHandle,
            cursorVisible,
            renderCursorOnly,
            requestFastRender,
            cursorIdleInterval,
            renderCaches
          )
        case IdleTick.HoldSolid =>
          holdCaretSolid(
            currentStateForDiagnostics,
            loadModel,
            pendingPaintDamage,
            cursorVisible,
            renderCursorOnly,
            requestFastRender,
            renderCaches
          )
      }

  private enum IdleTick:
    case Blink, HoldSolid

  /** Counts from the start of the idle phase, which input, any repaint and losing focus all restart -- so the timeout
    * runs from the last activity, and runs afresh after the window is refocused.
    */
  private def blinkTicks(
    loadModel: IO[Model],
    windowFocused: SignallingRef[IO, Boolean],
    cursorIdleInterval: AppConfig => Option[FiniteDuration],
    blinks: Ref[IO, Int]
  ): Stream[IO, IdleTick] =
    Stream
      .repeatEval(
        for
          _     <- AppRuntime.awaitFocusedIdleTick(loadModel.map(_.app), windowFocused, cursorIdleInterval)
          count <- blinks.updateAndGet(_ + 1)
          model <- loadModel
          config = model.app.persisted.config
        yield cursorIdleInterval(config).forall(AppRuntime.keepsBlinking(config, _, count))
      )
      .takeWhile(identity)
      .as(IdleTick.Blink)

  /** Paints the caret visible if the last blink left it hidden, so it stays solid while the phase waits. Like a blink
    * frame, it reads content damage without draining it.
    */
  private def holdCaretSolid(
    currentStateForDiagnostics: IO[Option[AppState]],
    loadModel: IO[Model],
    pendingPaintDamage: Ref[IO, Damage],
    cursorVisible: Ref[IO, Boolean],
    renderCursorOnly: AppRuntime.RenderFn,
    requestFastRender: IO[Unit],
    renderCaches: com.serenity.state.manager.RenderCaches
  )(using Logger[IO]): IO[Unit] =
    val paintVisibleCaret =
      for
        model       <- withRuntimeDiagnostics("render loop", "idle.state", currentStateForDiagnostics)(loadModel)
        paintDamage <- pendingPaintDamage.get
        _ <- withRuntimeDiagnostics("render loop", "idle.cursor-render", IO.pure(Some(model.app)))(
          renderCursorOnly(model.app, true, None, paintDamage, renderCaches)
        )
      yield ()
    cursorVisible.getAndSet(true).flatMap { wasVisible =>
      IO.unlessA(wasVisible)(paintVisibleCaret.handleErrorWith(recoverIdleCursorRenderFailure(_, requestFastRender)))
    }

  /** One event per batch, for callers holding events that are already translated. */
  private[serenity] def inputEventPhase(
    stateManager: StateEngine,
    inputRouter: InputRouter[IO, Event],
    systemClipboard: SystemClipboard[IO],
    checkResizeAndHandle: IO[Unit],
    cursorVisible: Ref[IO, Boolean],
    emitDamage: Damage => IO[Unit],
    translatorCache: Ref[IO, Option[FocusedTranslatorCacheEntry]] = Ref.unsafe[IO, Option[FocusedTranslatorCacheEntry]](
      None
    ),
    frameTimings: FrameTimings = FrameTimings()
  )(using balance: com.serenity.rope.Balance): Stream[IO, Event] => Stream[IO, Unit] =
    _.map(event => Chunk.singleton(PendingInput.Ready(event))).through(
      inputBatchPhase(
        InputBatchContext(
          stateManager,
          inputRouter,
          systemClipboard,
          checkResizeAndHandle,
          cursorVisible,
          emitDamage,
          translatorCache,
          frameTimings
        )
      )
    )

  /** What the input loop needs to apply a batch. `onUserInput` runs as a batch containing real input starts;
    * `observeBatch` is told each dispatch's states either side, since the commit observer never sees the commits inside
    * a batch; `logEvent` sees every applied event with the focus its dispatch left.
    */
  final private[serenity] case class InputBatchContext(
      stateManager: StateEngine,
      inputRouter: InputRouter[IO, Event],
      systemClipboard: SystemClipboard[IO],
      checkResizeAndHandle: IO[Unit],
      cursorVisible: Ref[IO, Boolean],
      emitDamage: Damage => IO[Unit],
      translatorCache: Ref[IO, Option[FocusedTranslatorCacheEntry]],
      frameTimings: FrameTimings,
      onUserInput: IO[Unit] = IO.unit,
      observeBatch: (AppState, AppState) => IO[Unit] = (_, _) => IO.unit,
      logEvent: (Event, com.serenity.state.models.Focus) => IO[Unit] = (_, _) => IO.unit
  )

  /** Applies each batch in as few dispatches as its clipboard events allow, and reports its damage once. */
  private[serenity] def inputBatchPhase(
    context: InputBatchContext
  )(using balance: com.serenity.rope.Balance): Stream[IO, Chunk[PendingInput]] => Stream[IO, Unit] =
    _.evalMap(batch => applyInputBatch(context, batch.toList)).drain

  private def applyInputBatch(context: InputBatchContext, inputs: List[PendingInput])(using
    com.serenity.rope.Balance
  ): IO[Unit] =
    for
      _           <- IO.whenA(inputs.exists(StartupWarmUp.pendingInputInterruptsWarmUp))(context.onUserInput)
      _           <- IO.whenA(inputs.exists(isPointerInput))(context.checkResizeAndHandle)
      _           <- AppRuntime.resetCursorActivity(context.cursorVisible)
      config      <- context.stateManager.getCurrentState.map(_.persisted.config)
      translators <- cachedTranslators(config, context.translatorCache)
      steps = EventBatchSteps[PendingInput](
        decode = decodeInput(config, translators),
        isolate = ClipboardEventSync.touchesSystemClipboard,
        slice = AppRuntime.fastFrameInterval(config.surfaceConfig.renderFpsTarget) / 2
      )
      _ <- dispatchInputs(context, steps, inputs)
      _ <- refreshFocusedInputTranslator(context.stateManager, context.inputRouter, context.translatorCache)
    yield ()

  /** Each dispatch's damage is published as soon as it lands, so the oldest input of a long batch shows in the next
    * frame instead of waiting for the inputs queued behind it.
    */
  private def dispatchInputs(
    context: InputBatchContext,
    steps: EventBatchSteps[PendingInput],
    inputs: List[PendingInput]
  )(using com.serenity.rope.Balance): IO[Unit] =
    for
      _     <- IO(context.frameTimings.inputApplyStarted())
      batch <- context.stateManager.applyEventBatch(inputs, steps)
      _     <- IO(context.frameTimings.inputApplyFinished())
      _     <- IO(traceAppliedKeys(context.frameTimings.keyLatency, inputs, batch.remaining))
      _     <- batch.applied.traverse_(context.logEvent(_, batch.after.app.persisted.focus))
      _     <- context.observeBatch(batch.before.app, batch.after.app)
      _ <- IO.whenA(batch.applied.nonEmpty)(
        context.emitDamage(DamageProducer.forTransition(batch.before.app, batch.after.app)) >>
          IO(context.frameTimings.keyLatency.damageEmitted())
      )
      _ <- batch.isolated.fold(IO.unit)(applyIsolated(context, steps, _))
      _ <- IO.whenA(batch.remaining.nonEmpty)(dispatchInputs(context, steps, batch.remaining))
    yield ()

  /** Counts the isolated input as applied with its slice, as it leaves `remaining` there, so later keystrokes keep
    * their place in arrival order.
    */
  private def traceAppliedKeys(
    trace: KeyLatencyTrace,
    inputs: List[PendingInput],
    remaining: List[PendingInput]
  ): Unit =
    if trace.isEnabled then
      trace.keysApplied(inputs.take(inputs.size - remaining.size).count {
        case PendingInput.Keystroke(_) => true
        case PendingInput.Ready(_)     => false
      })

  private def applyIsolated(context: InputBatchContext, steps: EventBatchSteps[PendingInput], event: Event)(using
    com.serenity.rope.Balance
  ): IO[Unit] =
    ClipboardEventSync.beforeEvent(event, context.stateManager, context.systemClipboard) >>
      dispatchInputs(context, steps.copy(isolate = _ => false), List(PendingInput.Ready(event))) >>
      ClipboardEventSync.afterEvent(event, context.stateManager, context.systemClipboard)

  private def isPointerInput(input: PendingInput): Boolean =
    input match
      case PendingInput.Ready(_: com.serenity.keystroke.events.MouseInputEvent) => true
      case _                                                                    => false

  /** A keystroke is translated against the state the events ahead of it in the batch left, exactly as the focused
    * translator would have been refreshed between them. Only a batch that changes the config itself pays for a fresh
    * translator set.
    */
  private def decodeInput(config: AppConfig, translators: FocusedInputTranslator.TranslatorSet)(
    state: AppState,
    input: PendingInput
  ): Event =
    input match
      case PendingInput.Ready(event) => event
      case PendingInput.Keystroke(info) =>
        val current =
          if state.persisted.config == config then translators
          else FocusedInputTranslator.TranslatorSet.forConfig(state.persisted.config)
        FocusedInputTranslator.forState(state, current).translate(info)

  /** Keyed on `AppConfig` identity (structural equality): the focused-translator set changes only when the config does,
    * far less often than every keystroke/mouse-move that flows through `refreshFocusedInputTranslator` (issue #1409).
    */
  private[serenity] type FocusedTranslatorCacheEntry = (AppConfig, FocusedInputTranslator.TranslatorSet)

  private def refreshFocusedInputTranslator(
    stateManager: StateReader,
    inputRouter: InputRouter[IO, Event],
    translatorCache: Ref[IO, Option[FocusedTranslatorCacheEntry]]
  ): IO[Unit] =
    stateManager.getCurrentState.flatMap { state =>
      cachedTranslators(state.persisted.config, translatorCache).flatMap(translators =>
        inputRouter.setActiveTranslator(FocusedInputTranslator.forState(state, translators)) >>
          inputRouter.setCursorPeekEnabled(state.persisted.config.surfaceConfig.commandRunnerCursorPeekEnabled)
      )
    }

  private def cachedTranslators(
    config: AppConfig,
    translatorCache: Ref[IO, Option[FocusedTranslatorCacheEntry]]
  ): IO[FocusedInputTranslator.TranslatorSet] =
    translatorCache.get.flatMap {
      case Some((cachedConfig, cachedTranslators)) if cachedConfig == config =>
        IO.pure(cachedTranslators)
      case _ =>
        val translators = FocusedInputTranslator.TranslatorSet.forConfig(config)
        translatorCache.set(Some(config -> translators)).as(translators)
    }

  /** One fast frame: waits out the frame deadline, paints the damage accumulated so far, and ends. The loop returns to
    * idle afterwards unless more damage arrived while the frame was being painted, in which case fast mode stays set
    * and the next frame follows (one frame interval after this one started).
    */
  private[serenity] def fastRenderPhase(
    stateManager: StateReader,
    fastModeSignal: SignallingRef[IO, Boolean],
    pendingDamage: Ref[IO, Damage],
    pendingPaintDamage: Ref[IO, Damage],
    currentStateForDiagnostics: IO[Option[AppState]],
    checkResizeAndHandle: IO[Unit],
    renderFull: AppRuntime.RenderFn,
    renderCaches: com.serenity.state.manager.RenderCaches,
    sleep: FiniteDuration => IO[Unit] = IO.sleep,
    frameClock: IO[FiniteDuration] = IO.monotonic,
    lastFrameStart: Ref[IO, Option[FiniteDuration]] = Ref.unsafe[IO, Option[FiniteDuration]](None),
    keyLatency: KeyLatencyTrace = KeyLatencyTrace()
  )(using logger: Logger[IO]): Stream[IO, Unit] =
    Stream
      .eval(
        for
          _            <- IO(keyLatency.frameWoke())
          _            <- pendingDamage.set(Damage.Nothing)
          stateAtStart <- stateManager.getCurrentState
          interval = AppRuntime.fastFrameInterval(stateAtStart.persisted.config.surfaceConfig.renderFpsTarget)
          _ <- awaitFrameDeadline(interval, lastFrameStart, frameClock, sleep)
          _ <- IO(keyLatency.frameDeadlineReached())
          _ <- withRuntimeDiagnostics("render loop", "fast.resize", currentStateForDiagnostics)(checkResizeAndHandle)
          // Before the read, so a keystroke whose damage lands during it is credited to the next frame, never to one
          // that may not show it.
          _ <- IO(keyLatency.frameModelRead())
          model <- withRuntimeDiagnostics("render loop", "fast.state", currentStateForDiagnostics)(
            stateManager.getModel
          )
          _           <- pendingDamage.set(Damage.Nothing)
          paintDamage <- pendingPaintDamage.getAndSet(Damage.Nothing)
          _ <- withRuntimeDiagnostics("render loop", "fast.full-render", IO.pure(Some(model.app)))(
            renderFull(model.app, true, None, paintDamage, renderCaches)
          )
          _ <- IO(keyLatency.frameEnded())
        yield ()
      )
      .onFinalize(settleFastMode(fastModeSignal, pendingDamage))

  /** Clears fast mode unless damage arrived since the phase drained it -- and re-raises it if damage lands between that
    * check and the clear, so no wake-up is lost.
    */
  private def settleFastMode(fastModeSignal: SignallingRef[IO, Boolean], pendingDamage: Ref[IO, Damage]): IO[Unit] =
    pendingDamage.get.flatMap { damage =>
      IO.whenA(AppRuntime.shouldClearFastMode(damage))(
        fastModeSignal.set(false) >>
          pendingDamage.get.flatMap(late => IO.whenA(late != Damage.Nothing)(fastModeSignal.set(true)))
      )
    }

  /** Idle, then one fast frame, forever: the idle phase ends when damage raises fast mode, the fast phase paints it. */
  private[serenity] def renderLoop(idlePhase: Stream[IO, Unit], fastPhase: Stream[IO, Unit]): Stream[IO, Unit] =
    Stream.repeatEval(IO.unit).flatMap(_ => idlePhase ++ fastPhase)

  /** Waits out the rest of the frame interval measured from the previous fast frame's start (shared across fast phases,
    * so input landing just after a frame still waits for the next deadline), records this frame's start, and returns
    * the time since the previous one. The recorded start is the deadline itself whenever a wait happened, so sleep
    * overshoot doesn't accumulate into a slower cadence.
    */
  private def awaitFrameDeadline(
    interval: FiniteDuration,
    lastFrameStart: Ref[IO, Option[FiniteDuration]],
    frameClock: IO[FiniteDuration],
    sleep: FiniteDuration => IO[Unit]
  ): IO[FiniteDuration] =
    for
      previous <- lastFrameStart.get
      now      <- frameClock
      delay = AppRuntime.fastFrameDelay(interval, previous, now)
      _ <- sleep(delay)
      start = now + delay
      _ <- lastFrameStart.set(Some(start))
    yield previous.fold(interval)(start - _)

  private[serenity] def withRuntimeDiagnostics[A](
    loopName: String,
    phase: String,
    stateForDiagnostics: IO[Option[AppState]]
  )(effect: IO[A])(using logger: Logger[IO]): IO[A] =
    Trace.timed(s"$loopName.$phase") {
      effect.handleErrorWith {
        case failure: AppRuntime.RuntimeFailure =>
          IO.raiseError(failure)
        case error =>
          stateForDiagnostics.attempt.flatMap {
            case Right(Some(state)) =>
              IO.raiseError(
                AppRuntime.RuntimeFailure(loopName, phase, AppRuntime.describeStateForDiagnostics(state), error)
              )
            case Right(None) =>
              IO.raiseError(AppRuntime.RuntimeFailure(loopName, phase, "state=unavailable", error))
            case Left(stateError) =>
              val reason = Option(stateError.getMessage).getOrElse(stateError.getClass.getSimpleName)
              IO.raiseError(AppRuntime.RuntimeFailure(loopName, phase, s"state=unavailable reason=$reason", error))
          }
      }
    }

  private[serenity] def superviseLoop(
    name: String,
    forceQuit: IO[Unit]
  )(effect: IO[Unit])(using logger: Logger[IO]): IO[Unit] =
    effect.handleErrorWith { error =>
      val (phase, diagnostics, loggedError) = error match
        case AppRuntime.RuntimeFailure(_, failedPhase, failureDiagnostics, cause) =>
          (s" phase=$failedPhase", s"; $failureDiagnostics", cause)
        case other =>
          ("", "", other)
      logger.error(loggedError)(s"[RUNTIME] $name failed$phase$diagnostics; forcing safe shutdown") >>
        forceQuit.attempt.void
    }

  private[serenity] def computeIdleCursorFrame(cursorVisible: Ref[IO, Boolean]): IO[(Boolean, Option[Color])] =
    cursorVisible.updateAndGet(!_).map(visible => (visible, None))

  private[serenity] def recoverIdleCursorRenderFailure(
    error: Throwable,
    requestFastRender: IO[Unit]
  )(using logger: Logger[IO]): IO[Unit] =
    val (phase, diagnostics, cause) = error match
      case AppRuntime.RuntimeFailure(_, failedPhase, failureDiagnostics, failureCause) =>
        (failedPhase, failureDiagnostics, failureCause)
      case other =>
        ("idle.cursor-render", "state=unavailable", other)
    logger.warn(cause)(
      s"[RUNTIME] idle cursor render failed phase=$phase; $diagnostics; requesting full render"
    ) >> requestFastRender

  private[serenity] def runIdleRenderStep(
    currentStateForDiagnostics: IO[Option[AppState]],
    loadModel: IO[Model],
    pendingPaintDamage: Ref[IO, Damage],
    checkResizeAndHandle: IO[Unit],
    cursorVisible: Ref[IO, Boolean],
    renderCursorOnly: AppRuntime.RenderFn,
    requestFastRender: IO[Unit],
    cursorIdleInterval: AppConfig => Option[FiniteDuration],
    renderCaches: com.serenity.state.manager.RenderCaches
  )(using logger: Logger[IO]): IO[Unit] =
    for
      _ <- withRuntimeDiagnostics("render loop", "idle.resize", currentStateForDiagnostics)(
        checkResizeAndHandle
      )
      model <- withRuntimeDiagnostics("render loop", "idle.state", currentStateForDiagnostics)(
        loadModel
      )
      state = model.app
      _ <- cursorIdleInterval(state.persisted.config) match
        case Some(_) =>
          for
            (visible, cursor) <- withRuntimeDiagnostics(
              "render loop",
              "idle.cursor",
              IO.pure(Some(state))
            )(computeIdleCursorFrame(cursorVisible))
            // Read without draining: this frame paints the cursor overlay, never content, so consuming content
            // damage here would lose it -- an input event that lands just as an idle tick fires would have its
            // glyphs dropped until something else damaged the same rows. The fast phase that same event wakes
            // drains it instead.
            paintDamage <- pendingPaintDamage.get
            _ <- withRuntimeDiagnostics(
              "render loop",
              "idle.cursor-render",
              IO.pure(Some(state))
            )(renderCursorOnly(state, visible, cursor, paintDamage, renderCaches))
              .handleErrorWith(recoverIdleCursorRenderFailure(_, requestFastRender))
          yield ()
        case None =>
          IO.unit
    yield ()
