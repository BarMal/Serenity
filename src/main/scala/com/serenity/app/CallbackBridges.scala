package com.serenity.app

import cats.effect.IO
import cats.effect.std.{Dispatcher, Semaphore}
import com.serenity.state.manager.QuitOutcome

/** Native callbacks (AWT, JLine) handed to the runtime as plain functions. */
object CallbackBridges:

  /** Dispatches an AWT/JLine callback's effect onto the runtime, tolerating the dispatcher having already shut down.
    *
    * On quit the runtime's `Dispatcher` closes while native callbacks (WINCH resize, window focus-lost) can still fire.
    * `unsafeRunAndForget` then throws `IllegalStateException: Dispatcher already closed` synchronously -- before the
    * effect's own error handler can run -- onto the AWT/pump thread, where `CrashReporter` logs it as a crash. A
    * callback arriving after shutdown has nothing left to do, so swallow only that specific closed-dispatcher state and
    * let every other failure propagate.
    */
  private[app] def dispatchIfRunning(dispatcher: Dispatcher[IO])(effect: IO[Unit]): Unit =
    try dispatcher.unsafeRunAndForget(effect)
    catch case _: IllegalStateException => ()

  /** What closing the window does: ask the quit workflow Ctrl+Q asks, so unsaved buffers are asked about. A request
    * made while an earlier one is still waiting for its answer is dropped, so a second click does not stack a second
    * prompt; once it is answered (Cancel included) the next one is heard.
    */
  def closeRequest(requestQuit: IO[QuitOutcome], dispatcher: Dispatcher[IO]): IO[() => Unit] =
    Semaphore[IO](1).map { inFlight => () =>
      dispatchIfRunning(dispatcher)(inFlight.tryPermit.use(acquired => IO.whenA(acquired)(requestQuit.void)))
    }
