package com.serenity

import scala.concurrent.duration.*

import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.{AppRuntime, AppRuntimeRenderLoops}
import com.serenity.config.AppConfig
import com.serenity.frontend.{Frontend, GuiFrontend, TuiFrontend}
import com.serenity.keystroke.events.Event
import com.serenity.rope.Balance
import com.serenity.state.manager.{Model, RenderCaches, StateEngine}
import com.serenity.state.models.{AppState, Damage}
import com.serenity.state.undo.UndoState
import com.serenity.testkit.VirtualTime.runVirtual
import com.serenity.ui.color.RenderColor
import fs2.Stream
import fs2.concurrent.SignallingRef
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** The caret stays solid while typing and blinks at a constant rhythm once input has paused for
  * [[Frontend.BlinkPauseDelay]]; only user input restarts that pause.
  */
class AppRuntimeCaretPauseSpec extends AnyFlatSpec with Matchers:

  given Balance    = Balance.default
  given Logger[IO] = NoOpLogger.impl[IO]

  private type Painted = Vector[(FiniteDuration, Boolean)]

  private def stateFor(config: AppConfig): AppState = AppState.initial(config)

  private def engineFor(state: AppState): StateEngine = new StateEngine:
    def getCurrentState: IO[AppState]                                = IO.pure(state)
    def getModel: IO[Model]                                          = IO.pure(Model(state, UndoState()))
    def updateStateValidated(update: AppState => AppState): IO[Unit] = IO.unit
    def applyEvent(event: Event): IO[Unit]                           = IO.unit

  final private case class Rig(
      signal: SignallingRef[IO, Boolean],
      windowFocused: SignallingRef[IO, Boolean],
      cursorVisible: Ref[IO, Boolean],
      lastInput: Ref[IO, Option[FiniteDuration]],
      painted: Ref[IO, Painted]
  ):

    /** What a real input batch does: restart the pause, show the caret solid, and wake the render loop. */
    def userInput: IO[Unit] =
      IO.monotonic.flatMap(now => lastInput.set(Some(now))) >> AppRuntime.resetCursorActivity(cursorVisible) >>
        signal.set(true)

    def nonInputDamage: IO[Unit] = signal.set(true)

  private def rig(focused: Boolean = true): IO[Rig] =
    for
      signal        <- SignallingRef.of[IO, Boolean](false)
      windowFocused <- SignallingRef.of[IO, Boolean](focused)
      cursorVisible <- Ref.of[IO, Boolean](true)
      lastInput     <- Ref.of[IO, Option[FiniteDuration]](Some(Duration.Zero))
      painted       <- Ref.of[IO, Painted](Vector.empty)
    yield Rig(signal, windowFocused, cursorVisible, lastInput, painted)

  private def idlePhase(
    rig: Rig,
    config: AppConfig,
    frontend: Frontend = GuiFrontend,
    renderCost: FiniteDuration = Duration.Zero
  ): Stream[IO, Unit] =
    val state = stateFor(config)
    AppRuntimeRenderLoops.idleRenderPhase(
      loadModel = engineFor(state).getModel,
      fastModeSignal = rig.signal,
      windowFocused = rig.windowFocused,
      pendingPaintDamage = Ref.unsafe[IO, Damage](Damage.Nothing),
      currentStateForDiagnostics = IO.pure(Some(state)),
      checkResizeAndHandle = IO.unit,
      cursorVisible = rig.cursorVisible,
      renderCursorOnly = (_: AppState, visible: Boolean, _: Option[RenderColor], _: Damage, _: RenderCaches) =>
        IO.monotonic.flatMap(now => rig.painted.update(_ :+ (now -> visible))) >> IO.sleep(renderCost),
      requestFastRender = IO.unit,
      cursorIdleInterval = frontend.cursorIdleInterval,
      renderCaches = RenderCaches.create(),
      lastInput = rig.lastInput
    )

  private val neverTimingOut = AppConfig.default.withCursorBlinkTimeoutMillis(0L)

  private def stamps(offsetsMillis: (Long, Boolean)*): Painted =
    offsetsMillis.toVector.map((millis, visible) => millis.millis -> visible)

  "The idle phase" should "keep the caret solid for the whole pause delay" in {
    Frontend.BlinkPauseDelay shouldBe 1000.millis

    val program = for
      r      <- rig()
      fiber  <- idlePhase(r, neverTimingOut).compile.drain.start
      _      <- IO.sleep(1490.millis)
      frames <- r.painted.get
      _      <- fiber.cancel
    yield frames shouldBe Vector.empty

    runVirtual(program)
  }

  it should "hide the caret one blink interval after the pause delay, then toggle every blink interval" in {
    val program = for
      r      <- rig()
      fiber  <- idlePhase(r, neverTimingOut).compile.drain.start
      _      <- IO.sleep(3600.millis)
      frames <- r.painted.get
      _      <- fiber.cancel
    yield frames shouldBe stamps(1500L -> false, 2000L -> true, 2500L -> false, 3000L -> true, 3500L -> false)

    runVirtual(program)
  }

  it should "keep the rhythm constant however long each blink frame takes to paint" in {
    val program = for
      r      <- rig()
      fiber  <- idlePhase(r, neverTimingOut, renderCost = 80.millis).compile.drain.start
      _      <- IO.sleep(3600.millis)
      frames <- r.painted.get
      _      <- fiber.cancel
    yield frames shouldBe stamps(1500L -> false, 2000L -> true, 2500L -> false, 3000L -> true, 3500L -> false)

    runVirtual(program)
  }

  it should "count the blink timeout from the start of blinking, then hold solid" in {
    val twoSeconds = AppConfig.default.withCursorBlinkTimeoutMillis(2000L)

    val program = for
      r      <- rig()
      fiber  <- idlePhase(r, twoSeconds).compile.drain.start
      _      <- IO.sleep(30.seconds)
      frames <- r.painted.get
      _      <- fiber.cancel
    yield frames shouldBe stamps(1500L -> false, 2000L -> true, 2500L -> false, 3000L -> true)

    runVirtual(program)
  }

  it should "never paint a hidden caret while input keeps arriving faster than the pause delay" in {
    val program = for
      r <- rig()
      fiber <- AppRuntimeRenderLoops
        .renderLoop(idlePhase(r, neverTimingOut), Stream.exec(r.signal.set(false)))
        .compile
        .drain
        .start
      // 700ms apart: slower than a blink interval, faster than the pause delay.
      _      <- (IO.sleep(700.millis) >> r.userInput).replicateA_(20)
      frames <- r.painted.get
      _      <- fiber.cancel
    yield frames shouldBe Vector.empty

    runVirtual(program)
  }

  it should "blink on the schedule the last input set, whatever the gaps before it were" in {
    val program = for
      r <- rig()
      fiber <- AppRuntimeRenderLoops
        .renderLoop(idlePhase(r, neverTimingOut), Stream.exec(r.signal.set(false)))
        .compile
        .drain
        .start
      _      <- IO.sleep(700.millis) >> r.userInput
      _      <- IO.sleep(1300.millis) >> r.userInput
      _      <- IO.sleep(1.second)
      before <- r.painted.get
      _      <- IO.sleep(1900.millis)
      frames <- r.painted.get
      _      <- fiber.cancel
    yield
      // Last input at 2000ms: solid until 3000ms, hidden at 3500ms, shown at 4000ms, hidden at 4500ms.
      before shouldBe Vector.empty
      frames shouldBe stamps(3500L -> false, 4000L -> true, 4500L -> false)

    runVirtual(program)
  }

  it should "carry on with the same blink phase when damage that is not input repaints mid-blink" in {
    val program = for
      r <- rig()
      fiber <- AppRuntimeRenderLoops
        .renderLoop(idlePhase(r, neverTimingOut), Stream.exec(r.signal.set(false)))
        .compile
        .drain
        .start
      _      <- IO.sleep(1700.millis)
      hidden <- r.cursorVisible.get
      _      <- r.nonInputDamage
      _      <- IO.sleep(50.millis)
      stayed <- r.cursorVisible.get
      _      <- IO.sleep(2000.millis)
      frames <- r.painted.get
      _      <- fiber.cancel
    yield
      hidden shouldBe false
      stayed shouldBe false
      frames shouldBe stamps(1500L -> false, 2000L -> true, 2500L -> false, 3000L -> true, 3500L -> false)

    runVirtual(program)
  }

  it should "repaint a tick that fell inside a fast frame as soon as the idle phase resumes" in {
    val program = for
      r <- rig()
      fiber <- AppRuntimeRenderLoops
        .renderLoop(
          idlePhase(r, neverTimingOut),
          Stream.exec(IO.sleep(600.millis) >> r.signal.set(false))
        )
        .compile
        .drain
        .start
      _      <- IO.sleep(1700.millis)
      _      <- r.nonInputDamage
      _      <- IO.sleep(2000.millis)
      frames <- r.painted.get
      _      <- fiber.cancel
    yield
      // The fast frame runs 1700-2300ms and swallows the 2000ms tick; the idle phase resumes at 2300ms and paints
      // the caret as the schedule has it (shown), then carries on at 2500ms.
      frames.take(3) shouldBe stamps(1500L -> false, 2300L -> true, 2500L -> false)

    runVirtual(program)
  }

  it should "wait out a fresh pause delay once the window regains focus" in {
    val program = for
      r     <- rig(focused = false)
      fiber <- idlePhase(r, neverTimingOut).compile.drain.start
      _     <- IO.sleep(5.seconds)
      _ <- AppRuntime.onWindowFocusChanged(
        focused = true,
        windowFocused = r.windowFocused,
        cursorVisible = r.cursorVisible,
        requestFastRender = IO.unit,
        markActivity = IO.monotonic.flatMap(now => r.lastInput.set(Some(now)))
      )
      _      <- IO.sleep(1400.millis)
      early  <- r.painted.get
      _      <- IO.sleep(200.millis)
      frames <- r.painted.get
      _      <- fiber.cancel
    yield
      early shouldBe Vector.empty
      frames shouldBe stamps(6500L -> false)

    runVirtual(program)
  }

  it should "schedule no timer at all, pause delay included, when the frontend owns the caret" in {
    def parked(config: AppConfig, frontend: Frontend): (Option[Any], Boolean) =
      val program = rig().flatMap(r => idlePhase(r, config, frontend).compile.drain)
      (for
        control  <- TestControl.execute(program)
        _        <- control.tickFor(1.minute)
        finished <- control.results
        idle     <- control.isDeadlocked
      yield (finished, idle)).unsafeRunSync()

    parked(AppConfig.default, TuiFrontend(com.serenity.keystroke.KeyboardFidelityTier.Full)) shouldBe ((None, true))
    parked(AppConfig.default.withCursorMode(com.serenity.config.CursorMode.Steady), GuiFrontend) shouldBe
      ((None, true))
  }

  "The fast phase" should "paint the caret as the blink left it, so damage that is not input does not make it solid" in {
    val state = stateFor(AppConfig.default)

    def paintedWith(visible: Boolean): IO[Boolean] =
      for
        signal             <- SignallingRef.of[IO, Boolean](true)
        pendingDamage      <- Ref.of[IO, Damage](Damage.Everything)
        pendingPaintDamage <- Ref.of[IO, Damage](Damage.Everything)
        drawn              <- Ref.of[IO, Option[Boolean]](None)
        _ <- AppRuntimeRenderLoops
          .fastRenderPhase(
            engineFor(state),
            signal,
            pendingDamage,
            pendingPaintDamage,
            IO.pure(Some(state)),
            IO.unit,
            (_: AppState, cursorVisible: Boolean, _: Option[RenderColor], _: Damage, _: RenderCaches) =>
              drawn.set(Some(cursorVisible)),
            RenderCaches.create(),
            cursorVisible = IO.pure(visible)
          )
          .compile
          .drain
        result <- drawn.get
      yield result.contains(visible)

    runVirtual(paintedWith(false).flatMap(hidden => paintedWith(true).map(hidden -> _))) shouldBe ((true, true))
  }

  "AppRuntime.onWindowFocusChanged" should "restart the pause when focus is gained, and not when it is lost" in {
    val program = for
      windowFocused <- SignallingRef.of[IO, Boolean](false)
      cursorVisible <- Ref.of[IO, Boolean](true)
      marks         <- Ref.of[IO, Int](0)
      _ <- AppRuntime.onWindowFocusChanged(
        true,
        windowFocused,
        cursorVisible,
        IO.unit,
        markActivity = marks.update(_ + 1)
      )
      afterGain <- marks.get
      _ <- AppRuntime.onWindowFocusChanged(
        false,
        windowFocused,
        cursorVisible,
        IO.unit,
        markActivity = marks.update(_ + 1)
      )
      afterLoss <- marks.get
    yield (afterGain, afterLoss)

    runVirtual(program) shouldBe ((1, 1))
  }
