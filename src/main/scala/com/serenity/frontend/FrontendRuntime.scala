package com.serenity.frontend

import java.awt.Color

import cats.effect.IO
import com.serenity.diagnostics.FrameTimings
import com.serenity.input.{InputHandler, InputRouter}
import com.serenity.keystroke.events.Event
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.{AppState, Damage}

object FrontendRuntime:

  /** Paints one frame: the current state, whether the caret is visible this tick, the caret's colour override (if any),
    * the damage accumulated since the last frame, and the `RenderCaches` instance to paint with. Owned here rather than
    * inline in `AppRuntime` because it is exactly the shape
    * [[FrontendRuntime.renderFull]]/[[FrontendRuntime.renderCursorOnly]] share -- moved from `AppRuntime.RenderFn`
    * alongside the rest of this issue's remaining #1669 scope. `RenderCaches` itself stays a call-time argument rather
    * than something closed over when a concrete `FrontendRuntime` is built (`Main.runGui`/`TuiRuntime.run`, both before
    * the owning `StateManager` -- and so its `RenderCaches` -- exists): `AppRuntimeRenderLoops` supplies it from
    * `stateManager.renderCaches` at each call.
    */
  type RenderFn =
    (
      AppState,
      Boolean,
      Option[Color],
      Damage,
      RenderCaches
    ) => IO[Unit]

/** The rendering/input bundle a concrete launch builds once its real Swing or terminal resource is acquired
  * (`Main.runGui`/`TuiRuntime.run`), and `AppRuntime.run` takes to get its rendering and input behaviour from -- issue
  * #1669's remaining scope: `AppRuntime.run` used to take `makeInputHandler`/`renderFull`/`renderCursorOnly` as three
  * separately-threaded closures alongside a `Frontend` that only supplied `capabilities`/`logRouting`/
  * `cursorIdleInterval`. This is that ownership transfer.
  *
  * Deliberately its own type rather than three more members on [[Frontend]] itself: `GuiFrontend`'s `capabilities`/
  * `logRouting`/`cursorIdleInterval`/`markdownPreviewWindow` are cheap, stateless facts read throughout the test suite
  * (`FrontendSpec`, `AppRuntimeCursorCadenceSpec`, `AppRuntimeFocusIdleSpec`, `AppRuntimeIdleCursorRenderSpec`) via the
  * bare `GuiFrontend` singleton, with no session ever constructed -- that is exactly why `GuiFrontend` is a `case
  * object` and `TuiFrontend`'s own extra field (`keyboardFidelityTier`) is cheap session-negotiated *data*, not a live
  * resource. A real render/input session instead closes over a genuinely live `SwingWindow` or `TerminalShell` that
  * only exists once `Main.runGui`/`TuiRuntime.run` have acquired it inside their own `Resource.use` block, so putting
  * these three members directly on `Frontend` would force `GuiFrontend` to stop being a reusable stateless singleton --
  * turning every one of those call sites into either a real Swing window construction or a `Frontend` instance carrying
  * closures nobody uses. Bundling them separately keeps `Frontend` cheap and keeps this bundle exactly as effectful and
  * exactly as session-scoped as the resources it closes over.
  */
final case class FrontendRuntime(
    inputHandler: InputRouter[IO, Event] => IO[InputHandler[IO]],
    renderFull: FrontendRuntime.RenderFn,
    renderCursorOnly: FrontendRuntime.RenderFn,
    frameTimings: FrameTimings = FrameTimings()
)
