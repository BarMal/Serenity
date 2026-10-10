package com.serenity.ui.tui

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.config.AppConfig
import com.serenity.input.{InProcessClipboard, InputRouter}
import com.serenity.keystroke.events.Event
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.testkit.VirtualTime.runVirtual
import org.jline.terminal.Size
import org.jline.terminal.impl.DumbTerminal
import org.jline.utils.NonBlockingReader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1709: the lone-`ESC` deadline must not read a pump that is merely late getting back to the reader as "nothing
  * followed". Sibling of [[TerminalInputHandlerSpec]], which covers the decoding itself.
  */
class TerminalInputHandlerEscLivenessSpec extends AnyFlatSpec with Matchers:

  private val StreamTimeout = 10.seconds
  private val esc           = 0x1b.toByte

  private val translator = new TextEntryTranslator(
    AppConfig.default.withHotkeyConfig(AppConfig.default.inputConfig.hotkeyConfig.forTerminalUse)
  )

  private def bytes(s: String): Array[Byte] = s.getBytes(StandardCharsets.UTF_8)
  private def csi(s: String): Array[Byte]   = esc +: bytes(s"[$s")

  private def structuralTerminal(): DumbTerminal =
    val terminal =
      new DumbTerminal(
        "test",
        "xterm-256color",
        new ByteArrayInputStream(Array.emptyByteArray),
        new ByteArrayOutputStream(),
        StandardCharsets.UTF_8
      )
    terminal.setSize(new Size(80, 24))
    terminal

  private def fakeTerminalWithReader(): (DumbTerminal, FakeTerminalReader) =
    (structuralTerminal(), new FakeTerminalReader)

  /** The pump is the one fiber that reads, and it can be late getting back to the reader: the blocking pool is busy, or
    * the fiber is simply not scheduled. The whole of `ESC [ A` is already waiting in the terminal buffer, but the pump
    * does not read the `[A` until 70ms after the `ESC` -- past the 50ms deadline. A deadline that only asks "has a byte
    * been pumped yet?" answers no and turns the arrow into Escape plus a literal `[A`; one that also asks whether the
    * pump was parked in a read, where nothing could have been waiting, sees a pump that was not and waits for it.
    * Virtual time, so the 70ms is exact and no scheduler load can move it.
    */
  "an arrow key whose bytes the pump reads after the deadline" should "still decode as one ArrowUp" in {
    val (terminal, reader) = fakeTerminalWithReader()
    val program = for
      reads     <- Ref.of[IO, Int](0)
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      lateAfterEsc = reads.getAndUpdate(_ + 1).flatMap(n => IO.whenA(n == 1)(IO.sleep(70.millis)))
      handler <- TerminalInputHandler.create(
        terminal,
        router,
        clipboard,
        escDeadline = 50.millis,
        readerOverride = Some(reader),
        beforeEachRead = lateAfterEsc
      )
      _      <- IO { reader.feed(csi("A")); reader.feedEof() }
      events <- handler.eventStream.take(1).compile.toList
    yield events

    runVirtual(program) shouldBe List(translator.translate(KeyStrokeInfo(InputKey.ArrowUp, None, Set.empty)))
  }

  /** The wait for a late pump is bounded: a pump that never reads again must not hold an `ESC` forever. */
  "a lone ESC whose pump never reads again" should "still resolve to Escape" in {
    val (terminal, reader) = fakeTerminalWithReader()
    val program = for
      reads     <- Ref.of[IO, Int](0)
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      stallsAfterEsc = reads.getAndUpdate(_ + 1).flatMap(n => IO.whenA(n == 1)(IO.never))
      handler <- TerminalInputHandler.create(
        terminal,
        router,
        clipboard,
        escDeadline = 50.millis,
        readerOverride = Some(reader),
        beforeEachRead = stallsAfterEsc
      )
      _      <- IO(reader.feed(Array(esc)))
      events <- handler.eventStream.take(1).compile.toList
    yield events

    runVirtual(program) shouldBe List(translator.translate(KeyStrokeInfo(InputKey.Escape, None, Set.empty)))
  }

  /** A pump that is healthy and parked in a read finds nothing after a lone `ESC`, and that is the answer. */
  "a lone ESC read by a healthy pump" should "resolve to Escape on the real clock" in {
    val (terminal, reader) = fakeTerminalWithReader()
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler <- TerminalInputHandler.create(
        terminal,
        router,
        clipboard,
        escDeadline = 100.millis,
        readerOverride = Some(reader)
      )
      _      <- IO(reader.feed(Array(esc)))
      events <- handler.eventStream.take(1).compile.toList
    yield events

    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out")) shouldBe List(
      translator.translate(KeyStrokeInfo(InputKey.Escape, None, Set.empty))
    )
  }

  /** Shutting the handler down interrupts the blocked read: no reader thread is left parked in `read`. */
  "shutting down a handler whose pump is blocked in a read" should "release the reader" in {
    val inFlight = new AtomicInteger(0)
    val inner    = new FakeTerminalReader
    val reader = new NonBlockingReader:
      override def read(timeout: Long, isPeek: Boolean): Int =
        inFlight.incrementAndGet()
        try inner.read(timeout, isPeek)
        finally inFlight.decrementAndGet()
      override def readBuffered(b: Array[Char], off: Int, len: Int, timeout: Long): Int =
        throw new UnsupportedOperationException("not used by the handler's read loop")

    def awaitInFlight(expected: Int): IO[Unit] =
      IO(inFlight.get).flatMap(n => if n == expected then IO.unit else IO.sleep(5.millis) >> awaitInFlight(expected))

    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler   <- TerminalInputHandler.create(structuralTerminal(), router, clipboard, readerOverride = Some(reader))
      _         <- awaitInFlight(1)
      _         <- handler.shutdown
      _         <- awaitInFlight(0)
    yield ()

    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("reader still parked in read after shutdown"))
  }
