package com.serenity.diagnostics

import java.awt.{Toolkit, Window}

import scala.util.Try

/** What AWT still holds after a quit, for the watchdog's report (#2507). The JVM stays up while AWT's auto-shutdown
  * sees a displayable window or a busy toolkit, and a thread dump shows neither. Read from the watchdog thread: it
  * never waits for the event thread.
  */
object AwtStateProbe:

  final case class WindowState(description: String, displayable: Boolean, showing: Boolean)

  final case class State(windows: List[WindowState], pendingEvent: Option[String])

  def current(): State =
    State(
      Window.getWindows.toList.map(window =>
        WindowState(window.getClass.getName, window.isDisplayable, window.isShowing)
      ),
      Option(Toolkit.getDefaultToolkit.getSystemEventQueue.peekEvent()).map(_.getClass.getName)
    )

  def describe(moment: String, state: State): List[String] =
    val event = state.pendingEvent.fold("no event waiting on the event queue")(name => s"event waiting: $name")
    s"[SHUTDOWN] awt $moment: ${state.windows.size} window(s), $event" ::
      state.windows.map(window =>
        s"[SHUTDOWN]   ${window.description} displayable=${window.displayable} showing=${window.showing}"
      )

  def describeRead(moment: String, read: () => State): List[String] =
    Try(read()).fold(
      failure => List(s"[SHUTDOWN] awt state unavailable: ${failure.getMessage}"),
      describe(moment, _)
    )
