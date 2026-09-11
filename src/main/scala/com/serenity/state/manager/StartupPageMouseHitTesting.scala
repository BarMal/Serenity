package com.serenity.state.manager

import cats.effect.IO
import com.serenity.keystroke.events.*
import com.serenity.state.models.*

/** State the event pipeline exposes for routing a primary click on the startup page's launch actions, as a capability
  * record rather than a trait -- nothing here breaks a construction-order cycle (#1389), so mockability is the only
  * reason this needs an interface at all, and a record fakes trivially without one (#1017).
  */
final private[manager] case class StartupPageMouseHitTestingPort(
    executeCommand: com.serenity.command.Command => IO[Unit]
)

/** Hit-tests a primary click against the startup page's launch actions and, on a hit, executes the selected action's
  * command.
  */
final private[manager] class StartupPageMouseHitTesting(port: StartupPageMouseHitTestingPort):
  import port.*

  def handleStartupPageMouseClick(click: MouseClick, state: AppState): IO[Boolean] =
    val action = state.startPageSurface.flatMap { surface =>
      surface.content match
        case SurfaceContent.StartPage(page) =>
          for
            viewportSize <- state.runtime.viewportSize
            pixelX       <- click.pixelX
            pixelY       <- click.pixelY
            metrics      <- click.renderMetrics
            actionIndex  <- page.actionIndexAtPixel(pixelX, pixelY, viewportSize, metrics.code, metrics.ui)
            action       <- page.launchActions.lift(actionIndex)
          yield action
        case _ =>
          None
    }
    action.fold(IO.pure(false))(selected => executeCommand(selected.command).as(true))
