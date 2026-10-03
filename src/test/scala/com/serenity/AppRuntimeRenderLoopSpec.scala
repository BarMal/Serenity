package com.serenity

import java.awt.Color

import scala.concurrent.duration.*

import cats.effect.{IO, Ref}
import com.serenity.app.{AppRuntime, AppRuntimeRenderLoops}
import com.serenity.config.*
import com.serenity.config.AppConfigOps.*
import com.serenity.keystroke.events.Event
import com.serenity.rope.Balance
import com.serenity.state.manager.{Model, RenderCaches, StateEngine}
import com.serenity.state.models.{AppState, Damage}
import com.serenity.state.undo.UndoState
import com.serenity.testkit.VirtualTime.runVirtual
import fs2.concurrent.SignallingRef
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** The render loop renders once per damage: the fast phase paints one frame for the damage accumulated so far, then
  * returns to idle unless more damage arrived meanwhile, and nothing but the cursor's own cadence wakes the idle phase.
  */
class AppRuntimeRenderLoopSpec extends AnyFlatSpec with Matchers:

  given Balance    = Balance.default
  given Logger[IO] = NoOpLogger.impl[IO]

  private val state = AppState.initial.copy(persisted =
    AppState.initial.persisted
      .copy(config = AppState.initial.persisted.config.withRenderFpsTarget(RenderFpsTarget.Fps60))
  )

  private val engine: StateEngine = new StateEngine:
    def getCurrentState: IO[AppState]                                = IO.pure(state)
    def getModel: IO[Model]                                          = IO.pure(Model(state, UndoState()))
    def updateStateValidated(update: AppState => AppState): IO[Unit] = IO.unit
    def applyEvent(event: Event): IO[Unit]                           = IO.unit

  final private case class Rig(
      signal: SignallingRef[IO, Boolean],
      windowFocused: SignallingRef[IO, Boolean],
      cursorVisible: Ref[IO, Boolean],
      pendingDamage: Ref[IO, Damage],
      pendingPaintDamage: Ref[IO, Damage],
      fullFrames: Ref[IO, Vector[(FiniteDuration, Damage)]],
      cursorFrames: Ref[IO, Int]
  ):

    def emitDamage(damage: Damage): IO[Unit] =
      pendingDamage.update(_.combineWith(damage)) >> pendingPaintDamage.update(_.combineWith(damage)) >>
        signal.set(true)

  extension (damage: Damage)

    private def combineWith(other: Damage): Damage =
      import cats.syntax.semigroup.*
      damage |+| other

  private def rig: IO[Rig] =
    for
      signal             <- SignallingRef.of[IO, Boolean](false)
      windowFocused      <- SignallingRef.of[IO, Boolean](true)
      cursorVisible      <- Ref.of[IO, Boolean](true)
      pendingDamage      <- Ref.of[IO, Damage](Damage.Nothing)
      pendingPaintDamage <- Ref.of[IO, Damage](Damage.Nothing)
      fullFrames         <- Ref.of[IO, Vector[(FiniteDuration, Damage)]](Vector.empty)
      cursorFrames       <- Ref.of[IO, Int](0)
    yield Rig(signal, windowFocused, cursorVisible, pendingDamage, pendingPaintDamage, fullFrames, cursorFrames)

  private def fastPhase(rig: Rig, onFrame: IO[Unit] = IO.unit, lastStart: Ref[IO, Option[FiniteDuration]]) =
    AppRuntimeRenderLoops.fastRenderPhase(
      engine,
      rig.signal,
      rig.pendingDamage,
      rig.pendingPaintDamage,
      IO.pure(Some(state)),
      IO.unit,
      (_: AppState, _: Boolean, _: Option[Color], damage: Damage, _: RenderCaches) =>
        IO.monotonic.flatMap(now => rig.fullFrames.update(_ :+ (now -> damage))) >> onFrame,
      RenderCaches.create(),
      lastFrameStart = lastStart
    )

  private def idlePhase(rig: Rig, cursorIdleInterval: AppConfig => Option[FiniteDuration]) =
    AppRuntimeRenderLoops.idleRenderPhase(
      loadModel = engine.getModel,
      fastModeSignal = rig.signal,
      windowFocused = rig.windowFocused,
      pendingPaintDamage = rig.pendingPaintDamage,
      currentStateForDiagnostics = IO.pure(Some(state)),
      checkResizeAndHandle = IO.unit,
      cursorVisible = rig.cursorVisible,
      renderCursorOnly =
        (_: AppState, _: Boolean, _: Option[Color], _: Damage, _: RenderCaches) => rig.cursorFrames.update(_ + 1),
      requestFastRender = rig.emitDamage(Damage.Everything),
      cursorIdleInterval = cursorIdleInterval,
      renderCaches = RenderCaches.create()
    )

  private def loop(rig: Rig, cursorIdleInterval: AppConfig => Option[FiniteDuration]) =
    for lastStart <- Ref.of[IO, Option[FiniteDuration]](None)
    yield AppRuntimeRenderLoops.renderLoop(idlePhase(rig, cursorIdleInterval), fastPhase(rig, lastStart = lastStart))

  "The fast phase" should "render one frame for the accumulated damage, then clear fast mode" in {
    val program = for
      r         <- rig
      lastStart <- Ref.of[IO, Option[FiniteDuration]](None)
      _         <- r.emitDamage(Damage.Everything)
      _         <- fastPhase(r, lastStart = lastStart).compile.drain
      frames    <- r.fullFrames.get
      fastMode  <- r.signal.get
      pending   <- r.pendingPaintDamage.get
    yield
      frames.map(_._2) shouldBe Vector(Damage.Everything)
      fastMode shouldBe false
      pending shouldBe Damage.Nothing

    runVirtual(program)
  }

  it should "stay in fast mode when damage arrives while its frame is being painted" in {
    val program = for
      r         <- rig
      lastStart <- Ref.of[IO, Option[FiniteDuration]](None)
      _         <- r.emitDamage(Damage.Everything)
      _         <- fastPhase(r, onFrame = r.emitDamage(Damage.Everything), lastStart = lastStart).compile.drain
      fastMode  <- r.signal.get
    yield fastMode shouldBe true

    runVirtual(program)
  }

  "The render loop" should "paint nothing while idle with no damage and no cursor cadence" in {
    val program = for
      r      <- rig
      l      <- loop(r, _ => None)
      _      <- l.interruptAfter(30.seconds).compile.drain
      full   <- r.fullFrames.get
      cursor <- r.cursorFrames.get
    yield
      full shouldBe empty
      cursor shouldBe 0

    runVirtual(program)
  }

  it should "wake the idle phase only for the cursor's own cadence, never for a full frame" in {
    val program = for
      r      <- rig
      l      <- loop(r, _ => Some(500.millis))
      _      <- l.interruptAfter(10250.millis).compile.drain
      full   <- r.fullFrames.get
      cursor <- r.cursorFrames.get
    yield
      full shouldBe empty
      cursor shouldBe 20

    runVirtual(program)
  }

  it should "paint exactly one full frame for one damage burst and go back to idle" in {
    val program = for
      r      <- rig
      l      <- loop(r, _ => None)
      driver <- (IO.sleep(5.seconds) >> r.emitDamage(Damage.Everything) >> r.emitDamage(Damage.Everything)).start
      _      <- l.interruptAfter(30.seconds).compile.drain
      _      <- driver.join
      full   <- r.fullFrames.get
    yield full should have size 1

    runVirtual(program)
  }

  it should "schedule another frame, one frame interval later, for damage that arrives during a frame" in {
    val interval = AppRuntime.fastFrameInterval(RenderFpsTarget.Fps60)
    val program = for
      r         <- rig
      lastStart <- Ref.of[IO, Option[FiniteDuration]](None)
      arrived   <- Ref.of[IO, Boolean](false)
      onFrame = arrived.getAndSet(true).flatMap(already => IO.whenA(!already)(r.emitDamage(Damage.Everything)))
      l       = AppRuntimeRenderLoops.renderLoop(idlePhase(r, _ => None), fastPhase(r, onFrame, lastStart))
      driver <- (IO.sleep(5.seconds) >> r.emitDamage(Damage.Everything)).start
      _      <- l.interruptAfter(30.seconds).compile.drain
      _      <- driver.join
      full   <- r.fullFrames.get
    yield
      full should have size 2
      (full(1)._1 - full(0)._1) shouldBe interval

    runVirtual(program)
  }
