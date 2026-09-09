package com.serenity.ui.tui

import java.io.{InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicReference

import cats.effect.{IO, Resource}
import org.jline.terminal.Size
import org.jline.terminal.Terminal.{Signal, SignalHandler}
import org.jline.terminal.impl.DumbTerminal
import org.jline.utils.Signals

/** A [[DumbTerminal]] whose `handle` additionally hooks a genuine OS signal through JLine's own [[Signals]] utility --
  * exactly what `org.jline.terminal.impl.PosixSysTerminal.handle` does for a real system terminal (confirmed by
  * disassembling jline-terminal 3.27.1's class file: on a non-default handler it calls
  * `Signals.register(signal.name(), () -> raise(signal))`, which reflectively calls `sun.misc.Signal.handle` under the
  * hood). A handler registered through this terminal fires from the JVM's actual signal-dispatch thread on a real
  * `sun.misc.Signal.raise`, unlike every other spec's `Terminal.raise()`, which just invokes the registered
  * `Terminal.SignalHandler` in-process without touching the OS at all.
  *
  * Exists only for [[TerminalShellRealSignalIntegrationSpec]]: registering a process-wide native signal handler is
  * exactly the kind of real OS-level state [[RealBoundaryTest]] carves out of the fast parallel suite, so this type
  * deliberately stays out of [[FakeTerminalReader]].
  */
final class NativeSignalTerminal private (
    in: InputStream,
    out: OutputStream
) extends DumbTerminal("test", "xterm-256color", in, out, StandardCharsets.UTF_8):

  private val nativeTokens = new AtomicReference(Map.empty[Signal, AnyRef])

  override def handle(signal: Signal, handler: SignalHandler): SignalHandler =
    val previous = super.handle(signal, handler)
    nativeTokens.get().get(signal).foreach(token => Signals.unregister(signal.name(), token))
    val token: AnyRef =
      if handler eq SignalHandler.SIG_DFL then Signals.registerDefault(signal.name())
      else Signals.register(signal.name(), () => raise(signal))
    nativeTokens.updateAndGet(_ + (signal -> token))
    previous

  override def doClose(): Unit =
    nativeTokens.getAndSet(Map.empty).foreach((signal, token) => Signals.unregister(signal.name(), token))
    super.doClose()

object NativeSignalTerminal:

  /** Built over an empty, never-fed input stream: this terminal exists to exercise real signal delivery, not keystroke
    * decoding, so nothing ever needs to be read from it.
    */
  def resource(size: Size, out: OutputStream): Resource[IO, NativeSignalTerminal] =
    Resource.make(IO.blocking {
      val terminal = new NativeSignalTerminal(InputStream.nullInputStream(), out)
      terminal.setSize(size)
      terminal
    })(terminal => IO.blocking(terminal.close()).attempt.void)
