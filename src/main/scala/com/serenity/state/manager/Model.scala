package com.serenity.state.manager

import com.serenity.animation.AnimationState
import com.serenity.state.models.{AppState, BufferId}
import com.serenity.state.undo.UndoState

/** Everything the dispatcher owns and the renderer reads (#1697), held in one `Ref` so a change spanning several parts
  * commits in one write and a reader always sees one consistent snapshot.
  */
final case class Model(app: AppState, undo: UndoState, bufferAnimations: Map[BufferId, AnimationState])
