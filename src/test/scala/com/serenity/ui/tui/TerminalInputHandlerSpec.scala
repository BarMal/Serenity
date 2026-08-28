package com.serenity.ui.tui

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.input.{InProcessClipboard, InputRouter, SystemClipboard}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import org.jline.terminal.Size
import org.jline.terminal.impl.DumbTerminal
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers #1108's JLine wiring shell over `TerminalInputDecoder`: real key/mouse/paste byte sequences fed through a
  * real JLine `DumbTerminal` (streams-backed, the same harness `TerminalShellSpec` uses for #1107), decoded to the same
  * events the real translator stack (`InputRouter` + a `Translator[Event]`) would produce for the equivalent
  * `KeyStrokeInfo` -- the same one `SwingInputHandlerSpec` exercises for the AWT path -- plus EOF-to-graceful-
  * shutdown.
  */
class TerminalInputHandlerSpec extends AnyFlatSpec with Matchers:

  private val StreamTimeout = 10.seconds
  private val esc           = 0x1b.toByte
  private val translator    = new TextEntryTranslator

  private def bytes(s: String): Array[Byte] = s.getBytes(StandardCharsets.UTF_8)
  private def csi(s: String): Array[Byte]   = esc +: bytes(s"[$s")

  private def dumbTerminal(input: Array[Byte]): DumbTerminal =
    val in       = new ByteArrayInputStream(input)
    val out      = new ByteArrayOutputStream()
    val terminal = new DumbTerminal("test", "xterm-256color", in, out, StandardCharsets.UTF_8)
    terminal.setSize(new Size(80, 24))
    terminal

  private def handlerFor(input: Array[Byte]): IO[(TerminalInputHandler, SystemClipboard[IO])] =
    for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler   <- TerminalInputHandler.create(dumbTerminal(input), router, clipboard)
    yield (handler, clipboard)

  private def eventsFrom(input: Array[Byte], count: Int): List[Event] =
    val program = handlerFor(input).flatMap((handler, _) => handler.eventStream.take(count.toLong).compile.toList)
    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out waiting for events"))

  "typing a character" should "decode and translate identically to the equivalent Swing KeyStrokeInfo" in {
    eventsFrom(bytes("a"), 1) shouldBe List(
      translator.translate(KeyStrokeInfo(InputKey.Character, Some('a'), Set.empty))
    )
  }

  "an arrow key" should "decode and translate identically to the equivalent Swing KeyStrokeInfo" in {
    eventsFrom(csi("D"), 1) shouldBe List(translator.translate(KeyStrokeInfo(InputKey.ArrowLeft, None, Set.empty)))
  }

  "Home, End, PageUp/Down and the function keys" should "decode and translate identically to the Swing path" in {
    val cases = List(
      csi("H")             -> KeyStrokeInfo(InputKey.Home, None, Set.empty),
      csi("5~")            -> KeyStrokeInfo(InputKey.PageUp, None, Set.empty),
      (esc +: bytes("OP")) -> KeyStrokeInfo(InputKey.F1, None, Set.empty)
    )
    cases.foreach {
      case (input, info) =>
        eventsFrom(input, 1) shouldBe List(translator.translate(info))
    }
  }

  "Ctrl+Q" should "decode and translate to Quit, the same graceful-shutdown event the Swing path produces" in {
    eventsFrom(Array(0x11.toByte), 1) shouldBe List(com.serenity.keystroke.events.Quit)
  }

  "an SGR mouse press and release" should "decode to a MousePress followed by a MouseClick, bypassing the translator" in {
    val input = csi("<0;3;2M") ++ csi("<0;3;2m")
    eventsFrom(input, 2) shouldBe List(
      MousePress(col = 2, row = 1, shiftDown = false, button = MouseButton.Primary),
      MouseClick(col = 2, row = 1, clickCount = 1, shiftDown = false, button = MouseButton.Primary)
    )
  }

  "a burst of SGR any-motion moves" should "deliver only the latest position, mirroring Swing's latest-wins dedup" in {
    val input = csi("<35;1;1M") ++ csi("<35;5;5M") ++ csi("<35;10;10M")
    val events =
      // Let the whole (already in-memory) byte stream decode before we start pulling, so all three moves land on
      // the same un-claimed movement slot and collapse to one -- exactly what a slow consumer sees in production,
      // made deterministic here instead of racing a live consumer against the producer fiber.
      val program = for
        (handler, _) <- handlerFor(input)
        _            <- IO.sleep(200.millis)
        events       <- handler.eventStream.take(1).compile.toList
      yield events
      program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out waiting for events"))
    events shouldBe List(MouseMove(col = 9, row = 9, shiftDown = false))
  }

  "a bracketed paste" should "write the pasted text to the clipboard and emit a single Paste event, not individual keystrokes" in {
    val pasteText = "if (x) {\n  doSomething()\n}"
    val input     = (esc +: bytes("[200~")) ++ bytes(pasteText) ++ (esc +: bytes("[201~"))
    val program = for
      (handler, clipboard) <- handlerFor(input)
      events               <- handler.eventStream.take(1).compile.toList
      pasted               <- clipboard.readText
    yield (events, pasted)

    val (events, pasted) = program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out"))
    events shouldBe List(com.serenity.keystroke.events.Paste)
    pasted shouldBe Some(pasteText)
  }

  "EOF on stdin" should "translate to the same graceful-shutdown Quit event Ctrl+Q produces, and complete the stream" in {
    val program = handlerFor(Array.emptyByteArray).flatMap((handler, _) => handler.eventStream.compile.toList)
    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out")) shouldBe List(com.serenity.keystroke.events.Quit)
  }

  it should "terminate the raw keyStrokeInfoStream with an EOF KeyStrokeInfo" in {
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler   <- TerminalInputHandler.create(dumbTerminal(Array.emptyByteArray), router, clipboard)
      strokes   <- handler.keyStrokeInfoStream.compile.toList
    yield strokes

    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out")) shouldBe
      List(KeyStrokeInfo(InputKey.EOF, None, Set.empty))
  }

  /** Never yields a byte and never reaches EOF on its own -- unlike the empty `ByteArrayInputStream` the other specs
    * use, which EOFs immediately and would otherwise race `shutdown`'s cancellation against the read loop's own natural
    * EOF-driven completion. This is what a real, still-open terminal's stdin looks like between keystrokes: the read
    * loop is genuinely blocked in `reader.read()`, so only cancellation can end it.
    */
  private def blockingForever(): java.io.InputStream = new java.io.InputStream:
    override def read(): Int =
      Thread.sleep(Long.MaxValue)
      -1

  private def blockingDumbTerminal(): DumbTerminal =
    val terminal =
      new DumbTerminal("test", "xterm-256color", blockingForever(), new ByteArrayOutputStream(), StandardCharsets.UTF_8)
    terminal.setSize(new Size(80, 24))
    terminal

  "shutdown" should "terminate the event stream without waiting for EOF" in {
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler   <- TerminalInputHandler.create(blockingDumbTerminal(), router, clipboard)
      _         <- IO.sleep(50.millis) // let the read loop actually start blocking in `reader.read()` first
      _         <- handler.shutdown
      events    <- handler.eventStream.compile.toList
    yield events

    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out")) shouldBe Nil
  }
