package com.serenity.ui.tui

import java.io.ByteArrayOutputStream

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.testkit.RealBoundaryTest
import com.serenity.ui.layout.ViewportSize
import org.jline.terminal.Size
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Milliseconds, Seconds, Span}

/** The real-OS-signal counterpart to `TerminalShellSpec`'s `SIGINT`/`SIGWINCH` coverage. Every other spec drives
  * `TerminalShell`'s signal handling through JLine's own `Terminal.raise()`, which just invokes the registered
  * `Terminal.SignalHandler` in-process -- fast and deterministic, but it never touches the OS, so it can't catch a
  * regression in the assumption that `terminal.handle(Signal, handler)` genuinely reaches a real `sun.misc.Signal`
  * dispatch (the exact mechanism `PosixSysTerminal` -- the terminal `TerminalShell.resource` builds in production --
  * relies on). This spec closes that gap with [[NativeSignalTerminal]], which hooks real OS signals the same way
  * `PosixSysTerminal` does, and delivers them with a genuine `sun.misc.Signal.raise` rather than any in-process
  * simulation.
  *
  * Tagged [[RealBoundaryTest]] and excluded from the default parallel suite (see `build.sbt`): a process-wide native
  * signal handler is real OS state shared by the whole JVM, so this must never run concurrently with anything else that
  * touches the same signal names -- not because raising a signal in-process is itself flaky. Run explicitly via the
  * `realBoundaryTest` command.
  */
class TerminalShellRealSignalIntegrationSpec extends AnyFlatSpec with Matchers with Eventually:

  implicit override val patienceConfig: PatienceConfig =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(20, Milliseconds))

  private def nativeSignalTerminal() = NativeSignalTerminal.resource(new Size(80, 24), new ByteArrayOutputStream())

  "TerminalShell.checkResize" should "observe a size reported by a genuinely OS-delivered SIGWINCH" taggedAs RealBoundaryTest in {
    val before = nativeSignalTerminal()
      .use { terminal =>
        TerminalShell.forTerminal(terminal).use { shell =>
          for
            before <- shell.checkResize
            _ <- IO {
              terminal.setSize(new Size(120, 40))
              sun.misc.Signal.raise(new sun.misc.Signal("WINCH"))
            }
            _ <- IO(eventually(shell.checkResize.unsafeRunSync() shouldBe Some(ViewportSize(120, 40))))
          yield before
        }
      }
      .unsafeRunSync()

    before shouldBe None
  }

  it should "keep reporting nothing when no real signal has fired" taggedAs RealBoundaryTest in {
    val before = nativeSignalTerminal()
      .use(terminal => TerminalShell.forTerminal(terminal).use(_.checkResize))
      .unsafeRunSync()

    before shouldBe None
  }

  "TerminalShell.awaitExternalQuit" should "complete when a genuine OS SIGINT is delivered" taggedAs RealBoundaryTest in {
    // sun.misc.Signal.raise only *queues* the signal for the JVM's own signal-dispatch thread -- it returns long
    // before that thread actually invokes our handler, which itself hops through TerminalShell's Dispatcher. So this
    // waits for awaitExternalQuit to complete (bounded, since the dispatch is asynchronous) rather than racing the
    // raise call itself against it: releasing the shell (and closing its Dispatcher) before the real, asynchronous
    // delivery has actually reached handleInt is exactly what a race here would risk.
    val completed = nativeSignalTerminal()
      .use { terminal =>
        TerminalShell.forTerminal(terminal).use { shell =>
          IO(sun.misc.Signal.raise(new sun.misc.Signal("INT"))) >> shell.awaitExternalQuit.timeout(5.seconds).attempt
        }
      }
      .unsafeRunSync()

    completed shouldBe Right(())
  }
