package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.animation.AnimationState
import com.serenity.state.models.{AppState, BufferId}
import com.serenity.state.undo.UndoState

/** Everything the dispatcher owns and the renderer reads (#1697), held in one `Ref` so a change spanning several parts
  * commits in one write and a reader always sees one consistent snapshot.
  */
final case class Model(app: AppState, undo: UndoState, bufferAnimations: Map[BufferId, AnimationState])

object Model:

  // Views onto one part of the model: every write through them is a single atomic update of the whole model
  // (`Ref.lens` delegates `set`/`update`/`modify` to the underlying ref's `update`/`modify`).
  private[manager] def appRef(model: Ref[IO, Model]): Ref[IO, AppState] =
    Ref.lens[IO, Model, AppState](model)(_.app, current => app => current.copy(app = app))

  private[manager] def undoRef(model: Ref[IO, Model]): Ref[IO, UndoState] =
    Ref.lens[IO, Model, UndoState](model)(_.undo, current => undo => current.copy(undo = undo))

  private[manager] def bufferAnimationsRef(model: Ref[IO, Model]): Ref[IO, Map[BufferId, AnimationState]] =
    Ref.lens[IO, Model, Map[BufferId, AnimationState]](model)(
      _.bufferAnimations,
      current => animations => current.copy(bufferAnimations = animations)
    )
