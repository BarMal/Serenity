package com.serenity.ui.renderer

import cats.effect.IO
import com.serenity.keystroke.events.ResizeEvent
import com.serenity.state.manager.StateManager
import com.serenity.ui.layout.TerminalSize

object RenderController:

  /** Apply a detected terminal resize to state and invoke onResized. Separating the Lanterna Screen query from this
    * method makes the logic unit-testable: callers pass the already-extracted Option[TerminalSize] rather than the
    * Screen itself.
    */
  def handleResize(
    newSize: Option[TerminalSize],
    stateManager: StateManager,
    onResized: IO[Unit]
  ): IO[Unit] =
    newSize match
      case None     => IO.unit
      case Some(sz) => stateManager.applyEvent(ResizeEvent(sz)) >> onResized
