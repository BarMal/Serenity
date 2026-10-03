package com.serenity.app

import java.awt.Color

import scala.concurrent.duration.*

import cats.effect.*
import com.serenity.config.AppConfig
import com.serenity.diagnostics.{FrameTimings, Trace}
import com.serenity.input.*
import com.serenity.keystroke.events.Event
import com.serenity.state.manager.*
import com.serenity.state.models.{AppState, Damage}
import fs2.Stream
import fs2.concurrent.SignallingRef
import org.typelevel.log4cats.Logger

/** The render loop internals `AppRuntime.run` drives: the idle phase (cursor-only ticks while nothing changed), the
  * fast phase (one full-content frame per burst of damage), and the diagnostics/supervision wrappers both share. Split
  * out of `AppRuntime.scala` (which stayed the orchestration entry point, buffer-load/quit wiring, and the background
  * loops) purely to keep both files under this repo's architecture-ratchet file-length limit -- no behavior changed by
  * this split.
  */
private[serenity] object AppRuntimeRenderLoops:

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
      .repeatEval(AppRuntime.awaitFocusedIdleTick(loadModel.map(_.app), windowFocused, cursorIdleInterval))
      .interruptWhen(fastModeSignal.discrete)
      .evalMap(_ =>
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
      )

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
    _.evalMap { event =>
      for
        before <- stateManager.getModel
        _ <-
          checkResizeBeforeInput(event, checkResizeAndHandle) >>
            ClipboardEventSync.beforeEvent(event, stateManager, systemClipboard) >>
            IO(frameTimings.inputApplyStarted()) >>
            stateManager.applyEvent(event) >>
            IO(frameTimings.inputApplyFinished()) >>
            ClipboardEventSync.afterEvent(event, stateManager, systemClipboard) >>
            refreshFocusedInputTranslator(stateManager, inputRouter, translatorCache) >>
            AppRuntime.resetCursorActivity(cursorVisible)
        after <- stateManager.getModel
        _ <- emitDamage(
          DamageProducer.forTransition(before.app, after.app)
        )
      yield ()
    }.drain

  /** Keyed on `AppConfig` identity (structural equality): the focused-translator set changes only when the config does,
    * far less often than every keystroke/mouse-move that flows through `refreshFocusedInputTranslator` (issue #1409).
    */
  private[serenity] type FocusedTranslatorCacheEntry = (AppConfig, FocusedInputTranslator.TranslatorSet)

  private def checkResizeBeforeInput(event: Event, checkResizeAndHandle: IO[Unit]): IO[Unit] =
    event match
      case _: com.serenity.keystroke.events.MouseInputEvent => checkResizeAndHandle
      case _                                                => IO.unit

  private def refreshFocusedInputTranslator(
    stateManager: StateReader,
    inputRouter: InputRouter[IO, Event],
    translatorCache: Ref[IO, Option[FocusedTranslatorCacheEntry]]
  ): IO[Unit] =
    stateManager.getCurrentState.flatMap { state =>
      val config = state.persisted.config
      translatorCache.get
        .flatMap {
          case Some((cachedConfig, cachedTranslators)) if cachedConfig == config =>
            IO.pure(cachedTranslators)
          case _ =>
            val translators = FocusedInputTranslator.TranslatorSet.forConfig(config)
            translatorCache.set(Some(config -> translators)).as(translators)
        }
        .flatMap(translators => inputRouter.setActiveTranslator(FocusedInputTranslator.forState(state, translators)))
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
    lastFrameStart: Ref[IO, Option[FiniteDuration]] = Ref.unsafe[IO, Option[FiniteDuration]](None)
  )(using logger: Logger[IO]): Stream[IO, Unit] =
    Stream
      .eval(
        for
          _            <- pendingDamage.set(Damage.Nothing)
          stateAtStart <- stateManager.getCurrentState
          interval = AppRuntime.fastFrameInterval(stateAtStart.persisted.config.surfaceConfig.renderFpsTarget)
          _ <- awaitFrameDeadline(interval, lastFrameStart, frameClock, sleep)
          _ <- withRuntimeDiagnostics("render loop", "fast.resize", currentStateForDiagnostics)(checkResizeAndHandle)
          model <- withRuntimeDiagnostics("render loop", "fast.state", currentStateForDiagnostics)(
            stateManager.getModel
          )
          _           <- pendingDamage.set(Damage.Nothing)
          paintDamage <- pendingPaintDamage.getAndSet(Damage.Nothing)
          _ <- withRuntimeDiagnostics("render loop", "fast.full-render", IO.pure(Some(model.app)))(
            renderFull(model.app, true, None, paintDamage, renderCaches)
          )
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
