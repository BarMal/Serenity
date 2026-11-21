package com.serenity

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.semigroup.catsSyntaxSemigroup
import com.serenity.app.AppRuntimeRenderLoops
import com.serenity.keystroke.events.Event
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.manager.{Model, RenderCaches, StateEngine}
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.testkit.TestMeasure
import com.serenity.ui.color.RenderColor
import fs2.concurrent.SignallingRef
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** An input that lands while a fast frame is being made must show in this frame or in one that follows it. The input
  * path commits the key to the model and then emits its damage, so the spec places that pair at a chosen point in the
  * frame, with no concurrency to make it flaky: `fastRenderPhase` clears `pendingDamage` after it reads the model, so
  * damage emitted between the read and the clear is lost, and nothing wakes another frame to draw the key.
  */
class FastRenderPhaseLateDamageSpec extends AnyFlatSpec with Matchers:

  given Balance    = Balance.default
  given Logger[IO] = NoOpLogger.impl[IO]

  private val bufferId = BufferId(0)

  private def typed(text: String): AppState =
    val initial = AppState.initial
    val buffer  = initial.persisted.buffers(bufferId)
    initial.copy(persisted =
      initial.persisted.copy(buffers =
        initial.persisted.buffers.updated(bufferId, buffer.copy(document = buffer.document.copy(content = Rope(text))))
      )
    )

  private enum Landing:
    case BeforeModelRead, AfterModelRead

  /** What the frame drew and whether fast mode is still raised, so another frame follows, once the phase has ended. */
  final private case class Outcome(drewNewestModel: Boolean, anotherFrameFollows: Boolean):
    def showsTheKey: Boolean = drewNewestModel || anotherFrameFollows

  private def frameWithKeyLanding(landing: Landing): Outcome =
    val before = typed("a")
    val after  = typed("ab")
    val program = for
      current            <- Ref.of[IO, AppState](before)
      fastMode           <- SignallingRef.of[IO, Boolean](true)
      pendingDamage      <- Ref.of[IO, Damage](Damage.Nothing)
      pendingPaintDamage <- Ref.of[IO, Damage](Damage.Nothing)
      lastDrawn          <- Ref.of[IO, Option[AppState]](None)
      drawn              <- Ref.of[IO, Vector[AppState]](Vector.empty)
      landed             <- Ref.of[IO, Boolean](false)
      keyLands = landed.getAndSet(true).flatMap { already =>
        IO.unlessA(already)(
          current.set(after) >>
            pendingDamage.update(_ |+| Damage.Everything) >>
            pendingPaintDamage.update(_ |+| Damage.Everything) >>
            fastMode.set(true)
        )
      }
      readModel = current.get.map(Model(_, UndoState()))
      engine = new StateEngine:
        def getCurrentState: IO[AppState] = current.get
        def getModel: IO[Model] = landing match
          case Landing.BeforeModelRead => keyLands *> readModel
          case Landing.AfterModelRead  => readModel <* keyLands
        def updateStateValidated(update: AppState => AppState): IO[Unit] = IO.unit
        def applyEvent(event: Event): IO[Unit]                           = IO.unit
      phase = AppRuntimeRenderLoops.fastRenderPhase(
        engine,
        fastMode,
        pendingDamage,
        pendingPaintDamage,
        IO.pure(None),
        IO.unit,
        (state: AppState, _: Boolean, _: Option[RenderColor], _: Damage, _: RenderCaches) => drawn.update(_ :+ state),
        TestMeasure.renderCaches(),
        sleep = _ => IO.unit,
        lastDrawnState = lastDrawn
      )
      _      <- phase.compile.drain
      raised <- fastMode.get
      frames <- drawn.get
    yield Outcome(drewNewestModel = frames.exists(_ eq after), anotherFrameFollows = raised)
    program.unsafeRunTimed(10.seconds).getOrElse(fail("the fast render phase did not finish"))

  "The fast render phase" should "show a key whose damage lands just before it reads the model" in {
    frameWithKeyLanding(Landing.BeforeModelRead).showsTheKey shouldBe true
  }

  it should "show a key whose damage lands just after it reads the model, in this frame or in a following one" in {
    frameWithKeyLanding(Landing.AfterModelRead).showsTheKey shouldBe true
  }
