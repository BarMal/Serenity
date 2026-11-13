package com.serenity.ui.tui

import cats.effect.IO
import com.serenity.diagnostics.FrameCounts
import com.serenity.rope.Balance
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

/** One typing frame in a real TUI session, counted by the same [[com.serenity.diagnostics.FrameCounter]]s the GUI
  * frames report, for the frame-count ratchet.
  */
object TuiFrameCountScenario extends TuiScriptSyntax:

  private given Balance           = Balance.default
  private given LoggerFactory[IO] = Slf4jFactory.create[IO]
  private given Logger[IO]        = LoggerFactory[IO].getLogger(using LoggerName("TuiFrameCountScenario"))

  /** The typed frame and the frame after it: a terminal writes the caret after its content, and the bytes of that write
    * are charged to the next frame, so the two together hold the whole cost of the keystroke.
    */
  def typingFrame: IO[FrameCounts] =
    val script =
      for
        counters <- session.map(_.stateManager.renderCaches.frameCounters)
        _        <- settledScreen
        _        <- lineEnd
        _        <- settledScreen
        _        <- liftIO(IO(counters.setEnabled(true)))
        _        <- typeText("d")
        _        <- screen
        _        <- screen
      yield counters.drain()
    TuiSession
      .resource(TuiEnvironment.withLines(200))
      .use(script.run)
      .flatMap { window =>
        if window.frames == 2 then IO.pure(FrameCounts(window.totals))
        else
          IO.raiseError(new IllegalStateException(s"expected the typed frame and the next, counted ${window.frames}"))
      }
