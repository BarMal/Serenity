package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** The render tick's animation advance (#1697 Wave 4): `ModelCommit.advanceTick` is a commit like any other, through
  * the same `AppStateValidation` every other message goes through -- there is no more `updateUnvalidated` bypass that
  * could commit an invalid state (e.g. a surface whose exit animation finished dropped from `uiSurfaces` while a
  * workspace-tree node still names it).
  */
class AnimationTickValidationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val quietLogger: Logger[IO] = NoOpLogger.impl[IO]

  private def operationsOver(initial: Model): IO[StateManagerOperationBoundary] =
    Ref.of[IO, Model](initial).flatMap(StateManagerOperationBoundary.create(_, quietLogger))

  "An animation tick" should "commit its advance through the same validated path as any other message" in {
    val before = Model(AppState.initial, UndoState(), Map.empty)
    val advance: Model => Model =
      model => model.copy(app = model.app.copy(runtime = model.app.runtime.copy(nextBufferId = BufferId(7))))
    val program =
      for
        operations <- operationsOver(before)
        after      <- operations.modelCommit.advanceTick(advance)
      yield after

    program.unsafeRunSync().app.runtime.nextBufferId.shouldBe(BufferId(7))
  }

  "An animation tick that would corrupt state" should
    "be rejected and leave the model unchanged, like any other invalid commit" in {
      val before = Model(AppState.initial, UndoState(), Map.empty)
      val corrupting: Model => Model = model =>
        model.copy(app = model.app.copy(persisted = model.app.persisted.copy(focus = Focus.EditorPane(PaneId(999)))))
      val program =
        for
          operations <- operationsOver(before)
          _          <- operations.modelCommit.advanceTick(corrupting)
          after      <- operations.modelCommit.model
        yield after

      program.unsafeRunSync() shouldBe before
    }

  "A tick with no active animation" should "leave the model untouched" in {
    val before = Model(AppState.initial, UndoState(), Map.empty)
    val program =
      for
        operations <- operationsOver(before)
        _          <- operations.modelCommit.advanceTick(identity)
        after      <- operations.modelCommit.model
      yield after

    program.unsafeRunSync() shouldBe before
  }
