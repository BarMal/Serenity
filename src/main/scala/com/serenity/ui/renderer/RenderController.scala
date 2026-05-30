package com.serenity.ui.renderer

import cats.effect.IO
import com.serenity.keystroke.events.ResizeEvent
import com.serenity.state.manager.StateManager
import com.serenity.ui.layout.ViewportSize

object RenderController:

  /** Apply a detected viewport resize to state and invoke onResized. Separating the backend query from this
    * method makes the logic unit-testable: callers pass the already-extracted Option[ViewportSize] rather than the
    * backend handle itself.
    */
  def handleResize(
    newSize: Option[ViewportSize],
    stateManager: StateManager,
    onResized: IO[Unit]
  ): IO[Unit] =
    newSize match
      case None     => IO.unit
      case Some(sz) => stateManager.applyEvent(ResizeEvent(sz)) >> onResized
