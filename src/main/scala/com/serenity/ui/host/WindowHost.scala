package com.serenity.ui.host

import scala.annotation.unused

import cats.effect.{IO, Resource}
import com.serenity.config.MenuBarMode
import com.serenity.diagnostics.FrameTimings
import com.serenity.frontend.FrontendRuntime
import com.serenity.input.{ImeDiagnostics, InputHandler, InputRouter}
import com.serenity.io.{FileDialog, FileDialogSelection}
import com.serenity.keystroke.events.Event
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.RenderTarget

/** What an input handler is built over, apart from the router it feeds. */
final case class InputSettings(
    metrics: () => CellMetrics,
    uiMetrics: () => CellMetrics,
    wheelScrollLines: Int,
    frameTimings: FrameTimings,
    imeDiagnostics: Option[ImeDiagnostics.Sink]
)

/** The window the GUI runs in, by capability: where frames are painted, how the window is controlled, and the input,
  * menus and dialogs that belong to the window system it lives in. The application is written against this; a window
  * system provides it.
  */
trait WindowHost:
  def target: RenderTarget
  def control: WindowControl

  /** The handler for the window's keyboard, mouse and input-method events, feeding `router`. */
  def inputHandler(router: InputRouter[IO, Event], settings: InputSettings): IO[InputHandler[IO]]

  /** Queues one typed character as if it had been typed in the window, for the start-up probe (#2420), which has no
    * window focus to type into. A window system that cannot do this fails, and the probe reports that.
    */
  def typeCharacter(@unused char: Char): IO[Unit] =
    IO.raiseError(new UnsupportedOperationException("This window host cannot inject a typed character."))

  /** The menu bar for `mode`, or `None` when this window system shows none. */
  def menus(mode: MenuBarMode): Option[FrontendRuntime.MenuHost => Resource[IO, Unit]]

  def fileDialog(selection: FileDialogSelection.Decision): FileDialog
