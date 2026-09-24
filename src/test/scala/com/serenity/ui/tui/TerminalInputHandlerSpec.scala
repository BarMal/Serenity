package com.serenity.ui.tui

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import java.util.concurrent.LinkedBlockingQueue

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import com.serenity.input.{InProcessClipboard, InputRouter, SystemClipboard}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.testkit.VirtualTime.runVirtual
import org.jline.terminal.Size
import org.jline.terminal.impl.DumbTerminal
import org.jline.utils.NonBlockingReader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers #1108's JLine wiring shell over `TerminalInputDecoder`: real key/mouse/paste byte sequences, fed through
  * [[FakeTerminalReader]] in place of a real terminal's input, decoded to the same events the real translator stack
  * (`InputRouter` + a `Translator[Event]`) would produce for the equivalent `KeyStrokeInfo` -- the same one
  * `SwingInputHandlerSpec` exercises for the AWT path -- plus EOF-to-graceful-shutdown.
  *
  * #1314/#1358: earlier versions of this spec read through a real `DumbTerminal`, in several cases backed by a live
  * `PipedInputStream`/`PipedOutputStream` pair written to after the handler was already running. See
  * [[FakeTerminalReader]]'s doc comment for why that no longer happens.
  *
  * Input that ends in EOF never leaves the reader blocked, so those scenarios run on virtual time: the
  * ESC-disambiguation deadline can then only pass once every fiber, the reader's included, has nothing left to do, and
  * a starved reader can no longer lose a sequence it was already given to that deadline.
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

  /** A `Terminal` used only for its `.writer()` (`TerminalInputHandler.create` writes mode-toggle escape sequences to
    * it on setup/teardown) -- the handler never calls `.reader()` once `readerOverride` is supplied, so this always
    * wraps an immediately-EOF empty stream rather than any real or live input.
    */
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

  /** Like [[structuralTerminal]], but keeps a handle on the backing `ByteArrayOutputStream` so a test can inspect the
    * exact mode-toggle escape sequences [[TerminalInputHandler.create]]/`.shutdown` write to it (#1532).
    */
  private def terminalWithCapturedOutput(): (DumbTerminal, ByteArrayOutputStream) =
    val out = new ByteArrayOutputStream()
    val terminal =
      new DumbTerminal(
        "test",
        "xterm-256color",
        new ByteArrayInputStream(Array.emptyByteArray),
        out,
        StandardCharsets.UTF_8
      )
    terminal.setSize(new Size(80, 24))
    (terminal, out)

  private def handlerFor(input: Array[Byte]): IO[(TerminalInputHandler, SystemClipboard[IO])] =
    val reader = new FakeTerminalReader
    reader.feed(input)
    reader.feedEof()
    for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler   <- TerminalInputHandler.create(structuralTerminal(), router, clipboard, readerOverride = Some(reader))
    yield (handler, clipboard)

  private def eventsFrom(input: Array[Byte], count: Int): List[Event] =
    runVirtual(handlerFor(input).flatMap((handler, _) => handler.eventStream.take(count.toLong).compile.toList))

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
      // the same un-claimed movement slot and collapse to one -- exactly what a slow consumer sees in production.
      // On virtual time the sleep only ends once the read loop has nothing left to do, so it is a barrier, not a guess.
      runVirtual(
        for
          (handler, _) <- handlerFor(input)
          _            <- IO.sleep(200.millis)
          events       <- handler.eventStream.take(1).compile.toList
        yield events
      )
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

    val (events, pasted) = runVirtual(program)
    events shouldBe List(com.serenity.keystroke.events.Paste)
    pasted shouldBe Some(pasteText)
  }

  "a kitty-protocol bare Ctrl press, release, press" should "double-tap-emit InputKey.Ctrl via the shared detector" in {
    val input  = csi("57442u") ++ csi("57442;1:3u") ++ csi("57442u")
    val reader = new FakeTerminalReader
    reader.feed(input)
    reader.feedEof()
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler   <- TerminalInputHandler.create(structuralTerminal(), router, clipboard, readerOverride = Some(reader))
      strokes   <- handler.keyStrokeInfoStream.take(1).compile.toList
    yield strokes

    runVirtual(program) shouldBe List(KeyStrokeInfo(InputKey.Ctrl, None, Set.empty))
  }

  "a kitty-protocol Ctrl press with no release before the second press" should "not fire the double-tap" in {
    val input  = csi("57442u") ++ csi("57442u") ++ bytes("a")
    val reader = new FakeTerminalReader
    reader.feed(input)
    reader.feedEof()
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler   <- TerminalInputHandler.create(structuralTerminal(), router, clipboard, readerOverride = Some(reader))
      strokes   <- handler.keyStrokeInfoStream.take(1).compile.toList
    yield strokes

    runVirtual(program) shouldBe List(KeyStrokeInfo(InputKey.Character, Some('a'), Set.empty))
  }

  "EOF on stdin" should "translate to the same graceful-shutdown Quit event Ctrl+Q produces, and complete the stream" in {
    val program = handlerFor(Array.emptyByteArray).flatMap((handler, _) => handler.eventStream.compile.toList)
    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out")) shouldBe List(com.serenity.keystroke.events.Quit)
  }

  it should "terminate the raw keyStrokeInfoStream with an EOF KeyStrokeInfo" in {
    val reader = new FakeTerminalReader
    reader.feedEof()
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler   <- TerminalInputHandler.create(structuralTerminal(), router, clipboard, readerOverride = Some(reader))
      strokes   <- handler.keyStrokeInfoStream.compile.toList
    yield strokes

    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out")) shouldBe
      List(KeyStrokeInfo(InputKey.EOF, None, Set.empty))
  }

  // ===Terminal focus reporting (CSI ?1004h/l, #1171): CSI I/CSI O decode to a side-channel focus callback, not the
  // ordinary key/event streams -- fed through the fake reader so bytes only arrive after the callback is registered,
  // avoiding a race against the read loop's own start.===

  private def fakeTerminalWithReader(): (DumbTerminal, FakeTerminalReader) =
    (structuralTerminal(), new FakeTerminalReader)

  /** The stream only ends once the read loop has handled everything before EOF, so by then the callback has fired. */
  private def focusCallbackResultFor(inputAfterRegistration: Array[Byte]): Option[Boolean] =
    val (terminal, reader) = fakeTerminalWithReader()
    val reported           = new java.util.concurrent.atomic.AtomicReference[Option[Boolean]](None)
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler   <- TerminalInputHandler.create(terminal, router, clipboard, readerOverride = Some(reader))
      _         <- IO(handler.registerFocusCallback(focused => reported.set(Some(focused))))
      _         <- IO { reader.feed(inputAfterRegistration); reader.feedEof() }
      _         <- handler.eventStream.compile.drain
    yield reported.get()
    runVirtual(program)

  "a terminal focus-in escape sequence (CSI I)" should "invoke the registered focus callback with true" in {
    focusCallbackResultFor(csi("I")) shouldBe Some(true)
  }

  "a terminal focus-out escape sequence (CSI O)" should "invoke the registered focus callback with false" in {
    focusCallbackResultFor(csi("O")) shouldBe Some(false)
  }

  "a focus-out sequence" should "not appear on the ordinary event stream" in {
    val (terminal, reader) = fakeTerminalWithReader()
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler   <- TerminalInputHandler.create(terminal, router, clipboard, readerOverride = Some(reader))
      _         <- IO { reader.feed(csi("O") ++ bytes("a")); reader.feedEof() }
      events    <- handler.eventStream.take(1).compile.toList
    yield events

    runVirtual(program) shouldBe List(
      translator.translate(KeyStrokeInfo(InputKey.Character, Some('a'), Set.empty))
    )
  }

  "shutdown" should "terminate the event stream without waiting for EOF" in {
    // A `FakeTerminalReader` that is never fed never reaches EOF on its own -- unlike the other specs' immediately-EOF empty
    // input, which would otherwise race `shutdown`'s cancellation against the read loop's own natural EOF-driven
    // completion. This is what a real, still-open terminal's stdin looks like between keystrokes: the read loop is
    // genuinely blocked in `reader.read()`, so only cancellation can end it.
    val reader = new FakeTerminalReader
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler <- TerminalInputHandler.create(
        structuralTerminal(),
        router,
        clipboard,
        readerOverride = Some(reader)
      )
      _      <- IO.interruptible(reader.awaitWaitingRead())
      _      <- handler.shutdown
      events <- handler.eventStream.compile.toList
    yield events

    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out")) shouldBe Nil
  }

  /** A lone `ESC` has to be told from the first byte of an escape sequence, and the only thing that distinguishes them
    * is whether more of the sequence follows. Only the input stream knows which, and it is now asked: the read after an
    * `ESC` is a timed one, so what the queue carries is the stream's own answer rather than a guess from a timer.
    *
    * It used to be a guess. The consumer raced the remaining bytes against a 50ms timer, and on a loaded machine the
    * timer could win while `[A` was still on its way -- turning Up into Escape and a literal `[A` typed into the
    * document. CI caught a first attempt at this that only checked the queue before waiting: that still lost when the
    * reader fiber itself was starved past the deadline, which on a two-core runner it is.
    *
    * The read loop's job is what this holds: a sequence the terminal has already sent decodes as one sequence -- the
    * loop assembles the present bytes rather than splitting them -- because the timed read finds `[A` before the
    * deadline. It runs at the production default (50ms), on virtual time so that "before the deadline" means before the
    * reader runs out of work rather than before a loaded scheduler gets round to it. `FakeTerminalReader` removes the
    * scheduling artifact a real JLine background pump thread could add on top (#1314) -- the bytes are simply in the
    * queue the moment `feed` returns.
    */
  "an escape sequence the terminal has already sent" should "decode as one sequence within the disambiguation deadline" in {
    val (terminal, reader) = fakeTerminalWithReader()
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler <- TerminalInputHandler.create(
        terminal,
        router,
        clipboard,
        escDeadline = 50.millis,
        readerOverride = Some(reader)
      )
      _      <- IO { reader.feed(csi("A")); reader.feedEof() }
      events <- handler.eventStream.take(1).compile.toList
    yield events

    runVirtual(program) shouldBe List(
      translator.translate(KeyStrokeInfo(InputKey.ArrowUp, None, Set.empty))
    )
  }

  /** And the deadline still does its own job: an `ESC` that nothing follows is a bare Escape. */
  "a lone ESC with nothing following" should "still resolve to Escape once the deadline passes" in {
    val (terminal, reader) = fakeTerminalWithReader()
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler   <- TerminalInputHandler.create(terminal, router, clipboard, readerOverride = Some(reader))
      _         <- IO(reader.feed(Array(esc)))
      events    <- handler.eventStream.take(1).compile.toList
    yield events

    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out")) shouldBe List(
      translator.translate(KeyStrokeInfo(InputKey.Escape, None, Set.empty))
    )
  }

  /** A sequence split across two writes, as a slow pty can deliver it, is still one sequence -- provided the rest
    * arrives before the deadline, which a deadline as long as the whole test's budget makes certain. The second write
    * waits until the reader has consumed the `ESC` and gone idle, so the sequence really does arrive in two parts.
    */
  "an escape sequence split across two writes" should "decode as one sequence" in {
    val (terminal, reader) = fakeTerminalWithReader()
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler <- TerminalInputHandler.create(
        terminal,
        router,
        clipboard,
        escDeadline = StreamTimeout,
        readerOverride = Some(reader)
      )
      _      <- IO(reader.feed(Array(esc)))
      _      <- IO.interruptible(reader.awaitWaitingRead())
      _      <- IO(reader.feed(bytes("[A")))
      events <- handler.eventStream.take(1).compile.toList
    yield events

    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out")) shouldBe List(
      translator.translate(KeyStrokeInfo(InputKey.ArrowUp, None, Set.empty))
    )
  }

  /** Emulates JLine's reader over a non-tty MSYS pipe (git bash on Windows without winpty, #-nontty regression): the
    * plain untimed `read()` returns the next queued char in order and blocks when none is available, but the *timed*
    * `read(timeout)` misfires -- it reports `READ_EXPIRED` immediately even though the next byte of an in-flight escape
    * sequence is already available. The ESC-disambiguation deadline must therefore not be delegated to `read(timeout)`;
    * it has to be driven by a terminal-independent clock. `feed` makes the chars available; `close` signals EOF.
    */
  final private class NonTtyPipeReader extends NonBlockingReader:
    private val chars            = new LinkedBlockingQueue[Int]()
    private val EofSentinel: Int = NonBlockingReader.EOF

    def feed(input: Array[Byte]): Unit = input.foreach(b => chars.put(b & 0xff))

    /** Feed a raw int exactly as JLine's reader would return it -- used to simulate the platform-specific Backspace
      * value 0xFFFF that git bash on Windows delivers, which cannot be represented as a byte.
      */
    def feedRawInt(value: Int): Unit = chars.put(value)
    def feedEof(): Unit              = chars.put(EofSentinel)

    override def read(timeout: Long, isPeek: Boolean): Int =
      if timeout > 0 then NonBlockingReader.READ_EXPIRED // the misfire: timed read never sees the available byte
      else
        val next = chars.take() // untimed read blocks until a byte (or EOF) is genuinely available
        if next == EofSentinel then
          chars.put(EofSentinel) // leave EOF latched for any subsequent read
          NonBlockingReader.EOF
        else next

    override def readBuffered(b: Array[Char], off: Int, len: Int, timeout: Long): Int =
      throw new UnsupportedOperationException("not used by the handler's read loop")

    override def shutdown(): Unit = ()

  private def nonTtyPipeEvents(feed: NonTtyPipeReader => Unit, count: Int): IO[List[Event]] =
    val reader = new NonTtyPipeReader
    for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler <- TerminalInputHandler.create(
        structuralTerminal(),
        router,
        clipboard,
        readerOverride = Some(reader)
      )
      _      <- IO(feed(reader))
      events <- handler.eventStream.take(count.toLong).compile.toList
    yield events

  /** For input that ends in EOF, so the reader never blocks and the scenario can run on virtual time. */
  private def eventsFromNonTtyPipe(feed: NonTtyPipeReader => Unit, count: Int): List[Event] =
    runVirtual(nonTtyPipeEvents(feed, count))

  private def thenEof(input: Array[Byte]): NonTtyPipeReader => Unit =
    reader =>
      reader.feed(input)
      reader.feedEof()

  // === #-nontty: the ESC-disambiguation deadline must not ride on JLine's timed read, which misfires on a non-tty
  // MSYS pipe (git bash without winpty). All three feed the whole sequence up front, so the byte after ESC is always
  // available -- a correct decoder keeps the sequence whole; the broken timed-read path splits ESC off as a bare key. ===

  "a kitty-protocol Backspace (ESC [ 127 u) over a non-tty pipe" should "decode as one Backspace, not Escape plus literal characters" in {
    eventsFromNonTtyPipe(thenEof(csi("127u")), 1) shouldBe List(
      translator.translate(KeyStrokeInfo(InputKey.Backspace, None, Set.empty))
    )
  }

  "an arrow key (ESC [ A) over a non-tty pipe" should "decode as one ArrowUp, not Escape plus a literal [A" in {
    eventsFromNonTtyPipe(thenEof(csi("A")), 1) shouldBe List(
      translator.translate(KeyStrokeInfo(InputKey.ArrowUp, None, Set.empty))
    )
  }

  "a genuinely-standalone ESC over a non-tty pipe" should "still resolve to Escape once the deadline passes" in {
    // No EOF here: only the deadline itself may resolve this ESC, so the scenario runs on the real clock.
    nonTtyPipeEvents(_.feed(Array(esc)), 1).unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out")) shouldBe List(
      translator.translate(KeyStrokeInfo(InputKey.Escape, None, Set.empty))
    )
  }

  // === JLine/Windows-console Backspace quirk (#-win-backspace): on Windows under git bash, JLine's reader returns the
  // int 65535 (0xFFFF, Unicode noncharacter U+FFFF) when the user presses Backspace -- confirmed via runtime capture.
  // Without the fix, 0xFFFF encodes to UTF-8 bytes EF BF BF and decodes to Character(U+FFFF) instead of Backspace. ===

  "the JLine/Windows Backspace value (0xFFFF) over a non-tty pipe" should "decode to Backspace, not a U+FFFF character" in {
    eventsFromNonTtyPipe(
      r =>
        r.feedRawInt(0xffff); r.feedEof()
      ,
      1
    ) shouldBe List(
      translator.translate(KeyStrokeInfo(InputKey.Backspace, None, Set.empty))
    )
  }

  // === Mouse-tracking capability detection (#1532): mintty over an MSYS pipe (Git Bash on Windows without winpty) is
  // a real, documented case (see `rawReadLoop`'s READ_EXPIRED doc above) where the terminal can never honor the SGR
  // mouse-tracking sequences `enableModes` sends -- they should not be sent into that environment at all. Bracketed
  // paste is unaffected (the issue's own findings implicate mouse tracking specifically, not paste), so it stays on
  // regardless. `mouseTrackingSupported` is `create`'s injectable capability signal, so both branches are exercised
  // here without needing a real console. ===

  "a mouse-tracking-capable terminal" should "have SGR mouse tracking and bracketed paste enabled on creation" in {
    val (terminal, out) = terminalWithCapturedOutput()
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler <- TerminalInputHandler.create(
        terminal,
        router,
        clipboard,
        readerOverride = Some(new FakeTerminalReader),
        mouseTrackingSupported = true
      )
    yield handler

    val handler = program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out"))
    handler.mouseTrackingSupported shouldBe true
    out.toString(StandardCharsets.UTF_8) shouldBe "\u001b[?1002h\u001b[?1003h\u001b[?1006h\u001b[?2004h"
  }

  "a terminal reported as not mouse-tracking-capable" should "enable bracketed paste only, sending no SGR mouse-tracking sequences" in {
    val (terminal, out) = terminalWithCapturedOutput()
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler <- TerminalInputHandler.create(
        terminal,
        router,
        clipboard,
        readerOverride = Some(new FakeTerminalReader),
        mouseTrackingSupported = false
      )
    yield handler

    val handler = program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out"))
    handler.mouseTrackingSupported shouldBe false
    out.toString(StandardCharsets.UTF_8) shouldBe "\u001b[?2004h"
  }

  it should "send no SGR mouse-tracking disable sequences on shutdown either, since none were ever enabled" in {
    val (terminal, out) = terminalWithCapturedOutput()
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler <- TerminalInputHandler.create(
        terminal,
        router,
        clipboard,
        readerOverride = Some(new FakeTerminalReader),
        mouseTrackingSupported = false
      )
      _ <- handler.shutdown
    yield ()

    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out"))
    out.toString(StandardCharsets.UTF_8) shouldBe "\u001b[?2004h\u001b[?2004l"
  }

  "TerminalInputHandler.defaultMouseTrackingSupported" should "be driven by System.console(), the JDK-level signal that is null for exactly the non-tty MSYS-pipe stdin this issue is about" in {
    TerminalInputHandler.defaultMouseTrackingSupported shouldBe (System.console() != null)
  }
