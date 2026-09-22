package com.serenity.app

import java.awt.Color

import scala.concurrent.duration.*

import cats.effect.*
import cats.syntax.semigroup.*
import com.serenity.diagnostics.Trace
import com.serenity.input.*
import com.serenity.keystroke.events.Event
import com.serenity.config.{AppConfig, CursorMode}
import com.serenity.state.manager.*
import com.serenity.state.models.{AppState, BufferId, Damage}
import com.serenity.ui.theme.ColorFormat.withAlpha
import fs2.Stream
import fs2.concurrent.SignallingRef
import org.typelevel.log4cats.Logger

/** The render loop internals `AppRuntime.run` drives: the idle phase (cursor-only ticks while nothing is animating),
  * the fast phase (full-content frames while something is), and the diagnostics/supervision wrappers both share.
  * Split out of `AppRuntime.scala` (which stayed the orchestration entry point, buffer-load/quit wiring, and the
  * background loops) purely to keep both files under this repo's architecture-ratchet file-length limit -- no
  * behavior changed by this split.
  */
private[serenity] object AppRuntimeRenderLoops:

  private[serenity] def idleRenderPhase(
    loadState: IO[AppState],
    loadBufferAnimations: IO[Map[BufferId, com.serenity.animation.AnimationState]],
    fastModeSignal: SignallingRef[IO, Boolean],
    windowFocused: SignallingRef[IO, Boolean],
    pendingPaintDamage: Ref[IO, Damage],
    currentStateForDiagnostics: IO[Option[AppState]],
    checkResizeAndHandle: IO[Unit],
    cursorVisible: Ref[IO, Boolean],
    breathIndex: Ref[IO, Int],
    renderCursorOnly: AppRuntime.RenderFn,
    requestFastRender: IO[Unit]
  )(using Logger[IO]): Stream[IO, Unit] =
    Stream
      .repeatEval(AppRuntime.awaitFocusedIdleTick(loadState, windowFocused))
      .interruptWhen(fastModeSignal.discrete)
      .evalMap(_ =>
        runIdleRenderStep(
          currentStateForDiagnostics,
          loadState,
          loadBufferAnimations,
          pendingPaintDamage,
          checkResizeAndHandle,
          cursorVisible,
          breathIndex,
          renderCursorOnly,
          requestFastRender
        )
      )

  private[serenity] def inputEventPhase(
    stateManager: StateEngine,
    inputRouter: InputRouter[IO, Event],
    systemClipboard: SystemClipboard[IO],
    checkResizeAndHandle: IO[Unit],
    cursorVisible: Ref[IO, Boolean],
    breathIndex: Ref[IO, Int],
    emitDamage: Damage => IO[Unit],
    translatorCache: Ref[IO, Option[FocusedTranslatorCacheEntry]] = Ref.unsafe[IO, Option[FocusedTranslatorCacheEntry]](
      None
    )
  )(using balance: com.serenity.rope.Balance): Stream[IO, Event] => Stream[IO, Unit] =
    _.evalMap { event =>
      for
        before           <- stateManager.getCurrentState
        beforeAnimations <- stateManager.getBufferAnimations
        _ <-
          checkResizeBeforeInput(event, checkResizeAndHandle) >>
            ClipboardEventSync.beforeEvent(event, stateManager, systemClipboard) >>
            observeCompanionSpriteTyping(event, stateManager) >>
            stateManager.applyEvent(event) >>
            ClipboardEventSync.afterEvent(event, stateManager, systemClipboard) >>
            refreshFocusedInputTranslator(stateManager, inputRouter, translatorCache) >>
            AppRuntime.resetCursorActivity(cursorVisible, breathIndex)
        after           <- stateManager.getCurrentState
        afterAnimations <- stateManager.getBufferAnimations
        _               <- emitDamage(DamageProducer.forTransition(before, after, beforeAnimations, afterAnimations))
      yield ()
    }.drain

  /** Keyed on `AppConfig` identity (structural equality): the focused-translator set changes only when the config does,
    * far less often than every keystroke/mouse-move that flows through `refreshFocusedInputTranslator` (issue #1409).
    */
  private[serenity] type FocusedTranslatorCacheEntry = (AppConfig, FocusedInputTranslator.TranslatorSet)

  private[serenity] def observeCompanionSpriteTyping(
    event: Event,
    stateManager: StateUpdater
  ): IO[Unit] =
    event match
      case _: com.serenity.keystroke.events.InsertChar =>
        stateManager.updateStateValidated(state =>
          state.copy(runtime = state.runtime.observeTyping(System.nanoTime(), state.persisted.config))
        )
      case _ => IO.unit

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

  private[serenity] def fastRenderPhase(
    stateManager: StateReader,
    animationTicker: AnimationTicker,
    fastModeSignal: SignallingRef[IO, Boolean],
    pendingDamage: Ref[IO, Damage],
    pendingPaintDamage: Ref[IO, Damage],
    animationTickCadence: Ref[IO, AppRuntime.AnimationTickCadence],
    currentStateForDiagnostics: IO[Option[AppState]],
    checkResizeAndHandle: IO[Unit],
    renderFull: AppRuntime.RenderFn,
    sleep: FiniteDuration => IO[Unit] = IO.sleep
  )(using logger: Logger[IO], balance: com.serenity.rope.Balance): Stream[IO, Unit] =
    Stream.eval(pendingDamage.getAndSet(Damage.Nothing)).flatMap { _ =>
      Stream
        .repeatEval(stateManager.getCurrentState)
        .zipWithIndex
        .evalMap {
          case (stateAtFrameStart, frameIndex) =>
            for
              isInitialFrame <- IO.pure(frameIndex == 0L)
              interval <-
                IO.pure(AppRuntime.fastFrameInterval(stateAtFrameStart.persisted.config.surfaceConfig.renderFpsTarget))
              _        <- sleep(AppRuntime.fastFrameDelay(interval, isInitialFrame))
              _ <- withRuntimeDiagnostics("render loop", "fast.resize", currentStateForDiagnostics)(
                checkResizeAndHandle
              )
              active <-
                if isInitialFrame then
                  for
                    initialState     <- stateManager.getCurrentState
                    bufferAnimations <- stateManager.getBufferAnimations
                  yield hasActiveAnimations(initialState, bufferAnimations)
                else
                  animationTickCadence.modify(_.advance(interval)).flatMap { animationTicks =>
                    withRuntimeDiagnostics("render loop", "fast.animation-tick", currentStateForDiagnostics)(
                      advanceAnimationsForCadence(animationTicks, stateManager, animationTicker, pendingPaintDamage)
                    )
                  }
              state <- withRuntimeDiagnostics("render loop", "fast.state", currentStateForDiagnostics)(
                stateManager.getCurrentState
              )
              bufferAnimations <- stateManager.getBufferAnimations
              paintDamage      <- pendingPaintDamage.getAndSet(Damage.Nothing)
              _ <- withRuntimeDiagnostics("render loop", "fast.full-render", IO.pure(Some(state)))(
                renderFull(state, true, None, paintDamage, bufferAnimations)
              )
            yield active
        }
        .takeWhile(identity)
        .map(_ => ())
        .onFinalize {
          stateManager.getCurrentState.flatMap { state =>
            stateManager.getBufferAnimations.flatMap { bufferAnimations =>
              pendingDamage.get.flatMap { damage =>
                if AppRuntime.shouldClearFastMode(hasActiveAnimations(state, bufferAnimations), damage) then
                  fastModeSignal.set(false)
                else IO.unit
              }
            }
          }
        }
    }

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
              IO.raiseError(AppRuntime.RuntimeFailure(loopName, phase, AppRuntime.describeStateForDiagnostics(state), error))
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

  private[serenity] def computeIdleCursorFrame(
    state: AppState,
    cursorVisible: Ref[IO, Boolean],
    breathIndex: Ref[IO, Int]
  ): IO[(Boolean, Option[Color])] =
    state.persisted.config.cursorMode match
      case CursorMode.Blink =>
        cursorVisible.updateAndGet(!_).map(vis => (vis, None))
      case CursorMode.Breathe =>
        for
          i <- breathIndex.updateAndGet(i => (i + 1) % 48)
          c     = state.persisted.config.cursorColors.activeOr(state.persisted.theme.cursor)
          alpha = ((math.sin(i * math.Pi / 24) + 1.0) / 2.0 * 255).toInt
        yield (true, Some(c.withAlpha(alpha)))

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
    loadState: IO[AppState],
    loadBufferAnimations: IO[Map[BufferId, com.serenity.animation.AnimationState]],
    pendingPaintDamage: Ref[IO, Damage],
    checkResizeAndHandle: IO[Unit],
    cursorVisible: Ref[IO, Boolean],
    breathIndex: Ref[IO, Int],
    renderCursorOnly: AppRuntime.RenderFn,
    requestFastRender: IO[Unit]
  )(using logger: Logger[IO]): IO[Unit] =
    for
      _ <- withRuntimeDiagnostics("render loop", "idle.resize", currentStateForDiagnostics)(
        checkResizeAndHandle
      )
      state <- withRuntimeDiagnostics("render loop", "idle.state", currentStateForDiagnostics)(
        loadState
      )
      _ <- AppRuntime.cursorIdleInterval(state.persisted.config, state.runtime.isTuiMode) match
        case Some(_) =>
          for
            (visible, cursor) <- withRuntimeDiagnostics(
              "render loop",
              "idle.cursor",
              IO.pure(Some(state))
            )(computeIdleCursorFrame(state, cursorVisible, breathIndex))
            // Read without draining: this frame paints the cursor overlay, never content, so consuming content
            // damage here would lose it -- an input event that lands just as an idle tick fires would have its
            // glyphs dropped until something else damaged the same rows. The fast phase that same event wakes
            // drains it instead.
            paintDamage      <- pendingPaintDamage.get
            bufferAnimations <- loadBufferAnimations
            _ <- withRuntimeDiagnostics(
              "render loop",
              "idle.cursor-render",
              IO.pure(Some(state))
            )(renderCursorOnly(state, visible, cursor, paintDamage, bufferAnimations))
              .handleErrorWith(recoverIdleCursorRenderFailure(_, requestFastRender))
          yield ()
        case None =>
          IO.unit
    yield ()

  private def advanceAnimationsForCadence(
    ticks: Int,
    stateManager: StateReader,
    animationTicker: AnimationTicker,
    pendingPaintDamage: Ref[IO, Damage]
  )(using balance: com.serenity.rope.Balance): IO[Boolean] =
    if ticks <= 0 then
      for
        state            <- stateManager.getCurrentState
        bufferAnimations <- stateManager.getBufferAnimations
      yield hasActiveAnimations(state, bufferAnimations)
    else
      for
        before           <- stateManager.getCurrentState
        beforeAnimations <- stateManager.getBufferAnimations
        stillActive <- (0 until ticks).toList.foldLeft(IO.pure(false)) { (previous, _) =>
          previous.flatMap(_ => animationTicker.advanceAnimationsOnTick)
        }
        after           <- stateManager.getCurrentState
        afterAnimations <- stateManager.getBufferAnimations
        _ <- pendingPaintDamage.update(
          _ |+| DamageProducer.forTransition(before, after, beforeAnimations, afterAnimations)
        )
      yield stillActive

  private[serenity] def hasActiveAnimations(
    state: AppState,
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState]
  ): Boolean =
    needsFullContentRender(state, bufferAnimations) || state.runtime.typingActivity.isActive

  /** Whether the fast render loop's current frame needs a full content repaint, as opposed to the cheaper cursor-only
    * overlay path. Character-reveal animations paint into document glyphs, and a theme transition cross-fades every
    * visible glyph/background colour (see RendererEntryPoints.withEffectiveTheme) -- both require the full canvas.
    * Surface animations (command palette, panel fades) are drawn through the same overlay-scene machinery as full
    * renders, not the cursor-only path, so they need it too. A column-to-column sweep (issue #1338) repaints the whole
    * pane's content for as long as it is mid-flight, for the same reason.
    *
    * The retired window sitter (issue #934 v2) used to be the one exception here: its glyph lived entirely in the
    * window chrome and never touched the canvas, so `canStandDownToCursorOnly` could skip a full repaint while it alone
    * was animating. Its typing-reactivity now lives in the companion sprite panel instead, which paints into panel
    * content like any other pinned panel -- there is no longer a canvas-free animation source, so that cursor-only
    * shortcut no longer applies to anything and has been removed rather than left checking a condition nothing can
    * satisfy.
    */
  private[serenity] def needsFullContentRender(
    state: AppState,
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState]
  ): Boolean =
    state.persisted.buffers.keys.exists(id => bufferAnimations.get(id).exists(_.hasActiveAnimations)) ||
      state.runtime.themeTransition.isDefined ||
      state.runtime.surfaceAnimations.nonEmpty ||
      state.runtime.columnTransitions.nonEmpty
