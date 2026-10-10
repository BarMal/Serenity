package com.serenity

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.foldable.*
import com.serenity.app.AppRuntimeRenderLoops
import com.serenity.keystroke.events.Event
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.manager.{DamageProducer, Model, RenderCaches, StateEngine}
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.testkit.EditingStateFixtures
import com.serenity.ui.color.RenderColor
import fs2.concurrent.SignallingRef
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** A batch commits every event to the model as it is applied but publishes damage once per slice, so a frame can read a
  * model newer than the damage it drains (#1906). The frame must still be handed damage for every row that differs
  * between the model it draws and the model it drew last.
  */
class RenderLoopModelDamagePairingSpec extends AnyFlatSpec with Matchers:

  given Balance    = Balance.default
  given Logger[IO] = NoOpLogger.impl[IO]

  private val bufferId = BufferId(0)

  private val drawn: AppState =
    val initial = AppState.initial
    initial.copy(persisted =
      initial.persisted.copy(buffers =
        initial.persisted.buffers.updated(
          bufferId,
          initial.persisted
            .buffers(bufferId)
            .copy(
              document =
                initial.persisted.buffers(bufferId).document.copy(content = Rope("first\nsecond\nthird\nfourth")),
              editing = EditingState(List(CursorPosition(3, 2)))
            )
        )
      )
    )

  private val selectionOnlyNewer: AppState =
    drawn.copy(persisted =
      drawn.persisted.copy(buffers =
        drawn.persisted.buffers.updated(
          bufferId,
          drawn.persisted
            .buffers(bufferId)
            .copy(editing =
              EditingStateFixtures(selection = Some(Selection(CursorPosition(1, 0), CursorPosition(3, 2))))
            )
        )
      )
    )

  private def damagePerFrame(models: List[AppState], publishedDamage: Damage): List[Damage] =
    val program = for
      current            <- Ref.of[IO, AppState](models.head)
      fastModeSignal     <- SignallingRef.of[IO, Boolean](true)
      pendingDamage      <- Ref.of[IO, Damage](Damage.Nothing)
      pendingPaintDamage <- Ref.of[IO, Damage](Damage.Nothing)
      lastDrawn          <- Ref.of[IO, Option[AppState]](None)
      frames             <- Ref.of[IO, Vector[Damage]](Vector.empty)
      engine = new StateEngine:
        def getCurrentState: IO[AppState]                                = current.get
        def getModel: IO[Model]                                          = current.get.map(Model(_, UndoState()))
        def updateStateValidated(update: AppState => AppState): IO[Unit] = IO.unit
        def applyEvent(event: Event): IO[Unit]                           = IO.unit
      phase = AppRuntimeRenderLoops.fastRenderPhase(
        engine,
        fastModeSignal,
        pendingDamage,
        pendingPaintDamage,
        IO.pure(None),
        IO.unit,
        (_: AppState, _: Boolean, _: Option[RenderColor], damage: Damage, _: RenderCaches) =>
          frames.update(_ :+ damage),
        RenderCaches.create(),
        sleep = _ => IO.unit,
        lastDrawnState = lastDrawn
      )
      _ <- models.zipWithIndex.traverse_ {
        case (model, index) =>
          current.set(model) >>
            IO.whenA(index > 0)(pendingPaintDamage.set(publishedDamage)) >>
            phase.compile.drain
      }
      rendered <- frames.get
    yield rendered.toList
    program.unsafeRunTimed(10.seconds).getOrElse(fail("the fast render phase did not finish"))

  "The fast render phase" should "cover rows that differ in the model it draws even when only older damage was published" in {
    val frames = damagePerFrame(List(drawn, selectionOnlyNewer), Damage.Nothing)

    frames.last shouldBe DamageProducer.forTransition(drawn, selectionOnlyNewer)
    frames.last should not be Damage.Nothing
  }

  it should "pass published damage through unchanged when the model did not move" in {
    val published = Damage.BufferRows(bufferId, Set(0))

    damagePerFrame(List(drawn, drawn), published).last shouldBe published
  }
