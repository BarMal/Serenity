package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.state.models.AppState
import com.serenity.state.undo.UndoState

/** `Ref.lens` views onto one part of a spec's own model ref, for seeding and observing it. Main code has no such view:
  * only `ModelCommit` reaches the model ref (#1697).
  */
private[manager] object ModelViews:

  def modelOf(state: AppState): IO[Ref[IO, Model]] =
    Ref.of[IO, Model](Model(state, UndoState()))

  def appRef(model: Ref[IO, Model]): Ref[IO, AppState] =
    Ref.lens[IO, Model, AppState](model)(_.app, current => app => current.copy(app = app))

  def undoRef(model: Ref[IO, Model]): Ref[IO, UndoState] =
    Ref.lens[IO, Model, UndoState](model)(_.undo, current => undo => current.copy(undo = undo))
