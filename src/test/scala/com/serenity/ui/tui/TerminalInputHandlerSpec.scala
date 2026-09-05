package com.serenity.ui.tui

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PipedInputStream, PipedOutputStream}
import java.nio.charset.StandardCharsets

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
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

  // #1213: mirrors `TuiRuntime.run`'s own `HotkeyConfig.forTerminalUse` rewrite -- a real terminal cannot deliver
  // Cmd/Meta as an ordinary keystroke the way AWT does for a focused Swing window, so TUI wiring never hands
  // `TextEntryTranslator` a config still at its macOS/Cmd-conditioned hotkey defaults. Building the translator any
  // other way here would test a translator no real TUI session ever actually uses.
  private val translator = new TextEntryTranslator(
    AppConfig.default.withHotkeyConfig(AppConfig.default.inputConfig.hotkeyConfig.forTerminalUse)
  )

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

  "a kitty-protocol bare Ctrl press, release, press" should "double-tap-emit InputKey.Ctrl via the shared detector" in {
    val input = csi("57442u") ++ csi("57442;1:3u") ++ csi("57442u")
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler   <- TerminalInputHandler.create(dumbTerminal(input), router, clipboard)
      strokes   <- handler.keyStrokeInfoStream.take(1).compile.toList
    yield strokes

    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out")) shouldBe
      List(KeyStrokeInfo(InputKey.Ctrl, None, Set.empty))
  }

  "a kitty-protocol Ctrl press with no release before the second press" should "not fire the double-tap" in {
    val input = csi("57442u") ++ csi("57442u") ++ bytes("a")
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler   <- TerminalInputHandler.create(dumbTerminal(input), router, clipboard)
      strokes   <- handler.keyStrokeInfoStream.take(1).compile.toList
    yield strokes

    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out")) shouldBe
      List(KeyStrokeInfo(InputKey.Character, Some('a'), Set.empty))
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

  // ===Terminal focus reporting (CSI ?1004h/l, #1171): CSI I/CSI O decode to a side-channel focus callback, not the
  // ordinary key/event streams -- fed through a live pipe so bytes only arrive after the callback is registered,
  // avoiding a race against the read loop's own start.===

  private def livePipeTerminal(): (DumbTerminal, PipedOutputStream) =
    val pipeIn  = new PipedInputStream()
    val pipeOut = new PipedOutputStream(pipeIn)
    val terminal =
      new DumbTerminal("test", "xterm-256color", pipeIn, new ByteArrayOutputStream(), StandardCharsets.UTF_8)
    terminal.setSize(new Size(80, 24))
    (terminal, pipeOut)

  private def focusCallbackResultFor(inputAfterRegistration: Array[Byte]): Option[Boolean] =
    val (terminal, pipeOut) = livePipeTerminal()
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler   <- TerminalInputHandler.create(terminal, router, clipboard)
      flag      <- cats.effect.Ref.of[IO, Option[Boolean]](None)
      _         <- IO(handler.registerFocusCallback(focused => flag.set(Some(focused)).unsafeRunAndForget()))
      _         <- IO(pipeOut.write(inputAfterRegistration)) >> IO(pipeOut.flush())
      _         <- IO.sleep(100.millis)
      value     <- flag.get
    yield value
    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out waiting for focus callback"))

  "a terminal focus-in escape sequence (CSI I)" should "invoke the registered focus callback with true" in {
    focusCallbackResultFor(csi("I")) shouldBe Some(true)
  }

  "a terminal focus-out escape sequence (CSI O)" should "invoke the registered focus callback with false" in {
    focusCallbackResultFor(csi("O")) shouldBe Some(false)
  }

  "a focus-out sequence" should "not appear on the ordinary event stream" in {
    val (terminal, pipeOut) = livePipeTerminal()
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler   <- TerminalInputHandler.create(terminal, router, clipboard)
      _         <- IO(pipeOut.write(csi("O") ++ bytes("a"))) >> IO(pipeOut.flush())
      events    <- handler.eventStream.take(1).compile.toList
    yield events

    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out")) shouldBe List(
      translator.translate(KeyStrokeInfo(InputKey.Character, Some('a'), Set.empty))
    )
  }

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

  /** The deadline still does its own job: an `ESC` that nothing follows is a bare Escape. */
  "a lone ESC with nothing following" should "still resolve to Escape once the deadline passes" in {
    val (terminal, pipeOut) = livePipeTerminal()
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler   <- TerminalInputHandler.create(terminal, router, clipboard)
      _         <- IO(pipeOut.write(Array(esc))) >> IO(pipeOut.flush())
      events    <- handler.eventStream.take(1).compile.toList
    yield events

    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out")) shouldBe List(
      translator.translate(KeyStrokeInfo(InputKey.Escape, None, Set.empty))
    )
  }

  /** And a sequence split across two writes, as a slow pty can deliver it, is still one sequence. */
  "an escape sequence split across two writes" should "decode as one sequence" in {
    val (terminal, pipeOut) = livePipeTerminal()
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler   <- TerminalInputHandler.create(terminal, router, clipboard)
      _         <- IO(pipeOut.write(Array(esc))) >> IO(pipeOut.flush())
      _         <- IO(pipeOut.write(bytes("[A"))) >> IO(pipeOut.flush())
      events    <- handler.eventStream.take(1).compile.toList
    yield events

    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out")) shouldBe List(
      translator.translate(KeyStrokeInfo(InputKey.ArrowUp, None, Set.empty))
    )
  }

  /** A lone `ESC` has to be told from the first byte of an escape sequence, and the only thing that distinguishes them
    * is whether more of the sequence follows. The deadline says how long to wait for that answer; it is not the answer.
    *
    * It was treated as one. Escape sequences reach the decoder a character at a time, so an arrow key always leaves it
    * holding a lone `ESC` with the rest of the sequence already queued behind -- and the old code decided that `ESC` by
    * racing those bytes against a 50ms timer. Under load the timer could win, and `ESC` `[` `A` became Escape followed
    * by a literal `[A` typed into the document.
    *
    * The fault itself cannot be reproduced through the handler: it needs a stalled scheduler, and simulating one by
    * shrinking the deadline proved as unreliable as the code it was testing -- the same suite passed at 5ms, failed at
    * 10ms and passed at 25ms. So it is held here at the seam where the answer is not a matter of timing at all.
    */
  "the rest of a sequence" should "be taken from input already in hand, whatever the deadline says" in {
    val program = for
      queue <- Queue.unbounded[IO, TerminalInputHandler.ReadOutcome]
      _     <- queue.offer(TerminalInputHandler.ReadOutcome.Bytes(bytes("[A")))
      // Zero: the deadline is already spent. It still may not overrule input that has arrived.
      found <- TerminalInputHandler.sequenceRemainder(queue, Duration.Zero)
    yield found

    outcomeBytes(program) shouldBe Some(bytes("[A").toList)
  }

  it should "report nothing once the deadline passes with no input at all" in {
    val program = for
      queue <- Queue.unbounded[IO, TerminalInputHandler.ReadOutcome]
      found <- TerminalInputHandler.sequenceRemainder(queue, 20.millis)
    yield found

    outcomeBytes(program) shouldBe None
  }

  it should "wait out the deadline for input that has not arrived yet" in {
    val program = for
      queue <- Queue.unbounded[IO, TerminalInputHandler.ReadOutcome]
      _     <- (IO.sleep(10.millis) >> queue.offer(TerminalInputHandler.ReadOutcome.Bytes(bytes("[A")))).start
      found <- TerminalInputHandler.sequenceRemainder(queue, 5.seconds)
    yield found

    outcomeBytes(program) shouldBe Some(bytes("[A").toList)
  }

  /** `ReadOutcome.Bytes` wraps an array, so equality has to be on the contents. */
  private def outcomeBytes(program: IO[Option[TerminalInputHandler.ReadOutcome]]): Option[List[Byte]] =
    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out")).map {
      case TerminalInputHandler.ReadOutcome.Bytes(value) => value.toList
      case other                                         => fail(s"expected bytes, got $other")
    }
