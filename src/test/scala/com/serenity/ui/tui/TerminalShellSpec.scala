package com.serenity.ui.tui

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PipedInputStream, PipedOutputStream}
import java.nio.charset.StandardCharsets

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.jline.terminal.Terminal
import org.jline.terminal.impl.DumbTerminal
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers #1107's `TerminalShell`: entering/leaving raw mode and the alternate screen around a `Resource[IO, _]`
  * acquire/release, resize propagation via `checkResize`/`registerResizeCallback`, and terminal-state restoration on
  * every exit path the issue calls out -- clean quit, an escaping `IO.raiseError`, and `SIGINT`.
  *
  * Built over a real JLine `DumbTerminal` wired to in-memory streams with the "xterm-256color" type -- a real
  * `Terminal` instance (not a hand-rolled mock) whose `Attributes`/signal handling run entirely in memory, and whose
  * capability strings (`smcup`/`rmcup`/`civis`/`cnorm`) are real, so the raw-mode and alternate-screen escapes this
  * spec asserts on are the actual bytes a real terminal would receive.
  */
class TerminalShellSpec extends AnyFlatSpec with Matchers:

  private val esc          = 0x1b.toChar.toString
  private val enterCaMode  = s"$esc[?1049h"
  private val exitCaMode   = s"$esc[?1049l"
  private val cursorHidden = s"$esc[?25l"
  private val cursorShown  = s"$esc[?12l$esc[?25h"

  final private class Harness(val terminal: Terminal, private val out: ByteArrayOutputStream):
    def written: String = out.toString(StandardCharsets.UTF_8)

  private def dumbTerminal(): Harness = dumbTerminal(Array.emptyByteArray)

  private def dumbTerminal(input: Array[Byte]): Harness =
    val in  = new ByteArrayInputStream(input)
    val out = new ByteArrayOutputStream()
    val terminal =
      new DumbTerminal("test", "xterm-256color", in, out, StandardCharsets.UTF_8)
    terminal.setSize(new org.jline.terminal.Size(80, 24))
    new Harness(terminal, out)

  /** A terminal whose input arrives reactively rather than being pre-loaded: needed to test the *second* negotiation
    * phase (`XTQMODKEYS`/`XTQFMTKEYS` confirmation) in isolation, since [[dumbTerminal]]'s static, pre-loaded input
    * would otherwise be drained by the first phase's kitty-response scan (`awaitKittyResponse` consumes every byte it
    * reads regardless of match, and a canned reply sitting in the stream before the kitty query is even written would
    * be gone -- misread as random bytes -- long before the modifyOtherKeys confirmation query is sent).
    */
  final private class LiveHarness(
      val terminal: Terminal,
      private val pipeOut: PipedOutputStream,
      private val out: ByteArrayOutputStream
  ):
    def written: String = out.toString(StandardCharsets.UTF_8)

    def send(bytes: Array[Byte]): Unit =
      pipeOut.write(bytes)
      pipeOut.flush()

    /** Closes the write end from the same (short-lived) thread that sent it, so JLine's `NonBlockingReader` background
      * thread sees a clean EOF on its next read instead of `PipedInputStream`'s "Write end dead" `IOException` once
      * that sending thread has terminated -- `send`'s own thread going away otherwise races the reader thread that
      * keeps polling the pipe for more input after negotiation's confirmation window closes.
      */
    def closeSendEnd(): Unit = pipeOut.close()

  private def liveTerminal(): LiveHarness =
    val pipeIn  = new PipedInputStream()
    val pipeOut = new PipedOutputStream(pipeIn)
    val out     = new ByteArrayOutputStream()
    val terminal =
      new DumbTerminal("test", "xterm-256color", pipeIn, out, StandardCharsets.UTF_8)
    terminal.setSize(new org.jline.terminal.Size(80, 24))
    new LiveHarness(terminal, pipeOut, out)

  "acquiring the shell" should "enter raw mode, the alternate screen, and hide the cursor" in {
    val harness  = dumbTerminal()
    val original = harness.terminal.getAttributes

    TerminalShell.forTerminal(harness.terminal).use(_ => IO.unit).unsafeRunSync()

    harness.terminal.getAttributes should not be original // restored to an equal-valued copy, not the same instance
    val written = harness.written
    written should include(enterCaMode)
    written should include(cursorHidden)
  }

  // ===Terminal focus reporting (CSI ?1004h/l, #1171): enabled on acquire, restored (disabled) on release, same as
  // every other terminal mode this shell owns.===

  private val focusReportingEnable  = s"$esc[?1004h"
  private val focusReportingDisable = s"$esc[?1004l"

  "acquiring the shell" should "enable terminal focus reporting" in {
    val harness = dumbTerminal()

    TerminalShell.forTerminal(harness.terminal).use(_ => IO.unit).unsafeRunSync()

    harness.written should include(focusReportingEnable)
  }

  "releasing the shell on clean quit" should "disable terminal focus reporting" in {
    val harness = dumbTerminal()

    TerminalShell.forTerminal(harness.terminal).use(_ => IO.unit).unsafeRunSync()

    harness.written should include(focusReportingDisable)
  }

  "releasing the shell after an escaping IO.raiseError" should "still disable terminal focus reporting" in {
    val harness = dumbTerminal()
    val boom    = new RuntimeException("boom")

    TerminalShell.forTerminal(harness.terminal).use(_ => IO.raiseError[Unit](boom)).attempt.unsafeRunSync()

    harness.written should include(focusReportingDisable)
  }

  "releasing the shell on clean quit" should "restore raw-mode attributes and exit the alternate screen" in {
    val harness  = dumbTerminal()
    val original = attributesSnapshot(harness.terminal)

    TerminalShell.forTerminal(harness.terminal).use(_ => IO.unit).unsafeRunSync()

    attributesSnapshot(harness.terminal) shouldBe original
    val written = harness.written
    written should include(exitCaMode)
    written should include(cursorShown)
    // The alternate-screen exit must come after the raw-mode entry it's undoing.
    written.indexOf(exitCaMode) should be > written.indexOf(enterCaMode)
  }

  "releasing the shell after an escaping IO.raiseError" should "still restore terminal state" in {
    val harness  = dumbTerminal()
    val original = attributesSnapshot(harness.terminal)
    val boom     = new RuntimeException("boom")

    val result = TerminalShell.forTerminal(harness.terminal).use(_ => IO.raiseError[Unit](boom)).attempt.unsafeRunSync()

    result shouldBe Left(boom)
    attributesSnapshot(harness.terminal) shouldBe original
    harness.written should include(exitCaMode)
    harness.written should include(cursorShown)
  }

  "releasing the shell after fiber cancellation" should "still restore terminal state" in {
    val harness  = dumbTerminal()
    val original = attributesSnapshot(harness.terminal)

    val program = TerminalShell.forTerminal(harness.terminal).use(_ => IO.never)
    program.start.flatMap(fiber => IO.sleep(50.millis) >> fiber.cancel).unsafeRunSync()

    attributesSnapshot(harness.terminal) shouldBe original
    harness.written should include(exitCaMode)
    harness.written should include(cursorShown)
  }

  "releasing the shell after SIGINT" should "still restore terminal state, and awaitExternalQuit should complete" in {
    val harness  = dumbTerminal()
    val original = attributesSnapshot(harness.terminal)

    // raise() must run after awaitExternalQuit has registered, so race the raise onto a second fiber.
    val racedProgram = TerminalShell.forTerminal(harness.terminal).use { shell =>
      IO.race(
        shell.awaitExternalQuit.timeout(1.second),
        IO.sleep(20.millis) >> IO(harness.terminal.raise(Terminal.Signal.INT))
      )
    }
    racedProgram.unsafeRunSync()

    attributesSnapshot(harness.terminal) shouldBe original
    harness.written should include(exitCaMode)
  }

  "checkResize" should "report nothing until SIGWINCH fires, then the new size once" in {
    val harness = dumbTerminal()

    val (before, afterResize, afterDrain) = TerminalShell
      .forTerminal(harness.terminal)
      .use { shell =>
        for
          before <- shell.checkResize
          _ <- IO {
            harness.terminal.setSize(new org.jline.terminal.Size(120, 40))
            harness.terminal.raise(Terminal.Signal.WINCH)
          }
          afterResize <- shell.checkResize
          afterDrain  <- shell.checkResize
        yield (before, afterResize, afterDrain)
      }
      .unsafeRunSync()

    before shouldBe None
    afterResize shouldBe Some(com.serenity.ui.layout.ViewportSize(120, 40))
    afterDrain shouldBe None
  }

  "registerResizeCallback" should "be invoked when SIGWINCH fires" in {
    val harness = dumbTerminal()

    val invoked = TerminalShell
      .forTerminal(harness.terminal)
      .use { shell =>
        for
          flag  <- cats.effect.Ref.of[IO, Boolean](false)
          _     <- IO(shell.registerResizeCallback(() => flag.set(true).unsafeRunAndForget()))
          _     <- IO(harness.terminal.raise(Terminal.Signal.WINCH))
          _     <- IO.sleep(20.millis)
          value <- flag.get
        yield value
      }
      .unsafeRunSync()

    invoked shouldBe true
  }

  private def attributesSnapshot(terminal: Terminal): String =
    terminal.getAttributes.toString

  // ===#1109's CSI-u negotiation ladder: query kitty support, push its enhancement flags if it answered, otherwise
  // fall back to xterm's modifyOtherKeys/formatOtherKeys=1; pop/disable unconditionally on every exit path.===

  private val kittyQuery             = s"$esc[?u"
  private val kittyPushFlags         = s"$esc[>3u"
  private val kittyPop               = s"$esc[<u"
  private val modifyOtherKeysEnable  = s"$esc[>4;2m"
  private val modifyOtherKeysDisable = s"$esc[>4;0m"
  private val formatOtherKeysEnable  = s"$esc[>4;1f"
  private val formatOtherKeysDisable = s"$esc[>4;0f"
  private val xtqModKeysQuery        = s"$esc[?4m"
  private val xtqFmtKeysQuery        = s"$esc[?4g"

  // A terminal that confirms modifyOtherKeys must echo back both XTMODKEYS and XTFMTKEYS replies carrying exactly
  // the values TerminalShell requested (mode 2, formatOtherKeys=1) -- see #1109/#1200's "confirmed tier" follow-up.
  private val confirmingReply = s"$esc[>4;2m$esc[>4;1f"

  "acquiring the shell against a terminal that answers the kitty query" should
    "push kitty's enhancement flags and report the Kitty tier" in {
      val harness = dumbTerminal(bytes(s"$esc[?1u"))

      val tier =
        TerminalShell.forTerminal(harness.terminal).use(shell => IO(shell.keyboardProtocolTier)).unsafeRunSync()

      tier shouldBe TerminalShell.KeyboardProtocolTier.Kitty
      val written = harness.written
      written should include(kittyQuery)
      written should include(kittyPushFlags)
      written should not include modifyOtherKeysEnable
    }

  /** Sends `reply` into `harness` once its written output shows the `XTQFMTKEYS` query -- i.e. once the kitty phase has
    * already given up and the modifyOtherKeys confirmation phase has actually started -- on a background daemon thread,
    * so the reply lands in the confirmation phase's read window rather than being drained by the kitty phase's earlier
    * scan (see [[liveTerminal]]'s doc).
    */
  private def replyAfterModifyOtherKeysQuery(harness: LiveHarness, reply: Array[Byte]): Unit =
    @annotation.tailrec
    def awaitQueryWritten(): Unit =
      if !harness.written.contains(xtqFmtKeysQuery) then
        Thread.sleep(1)
        awaitQueryWritten()

    val sender = new Thread(() =>
      awaitQueryWritten()
      harness.send(reply)
      harness.closeSendEnd()
    )
    sender.setDaemon(true)
    sender.start()

  "acquiring the shell against a terminal that skips kitty but confirms modifyOtherKeys" should
    "push modifyOtherKeys/formatOtherKeys, query to confirm, and report the ModifyOtherKeys tier" in {
      val harness = liveTerminal()
      replyAfterModifyOtherKeysQuery(harness, bytes(confirmingReply))

      val tier =
        TerminalShell.forTerminal(harness.terminal).use(shell => IO(shell.keyboardProtocolTier)).unsafeRunSync()

      tier shouldBe TerminalShell.KeyboardProtocolTier.ModifyOtherKeys
      val written = harness.written
      written should include(kittyQuery)
      written should include(modifyOtherKeysEnable)
      written should include(formatOtherKeysEnable)
      written should include(xtqModKeysQuery)
      written should include(xtqFmtKeysQuery)
      written should not include kittyPushFlags
      // The confirmation queries are written strictly before the shell releases (and disables) modifyOtherKeys --
      // unlike the Legacy-tier path below, which reverts immediately when confirmation fails.
      written.indexOf(xtqFmtKeysQuery) should be < written.indexOf(modifyOtherKeysDisable)
    }

  "acquiring the shell against a terminal that answers neither the kitty nor the modifyOtherKeys queries" should
    "fall back to the Legacy tier and revert the modifyOtherKeys/formatOtherKeys enable sequences" in {
      val harness = dumbTerminal()

      val tier =
        TerminalShell.forTerminal(harness.terminal).use(shell => IO(shell.keyboardProtocolTier)).unsafeRunSync()

      tier shouldBe TerminalShell.KeyboardProtocolTier.Legacy
      val written = harness.written
      written should include(kittyQuery)
      written should include(modifyOtherKeysEnable)
      written should include(formatOtherKeysEnable)
      written should include(xtqModKeysQuery)
      written should include(xtqFmtKeysQuery)
      written should include(modifyOtherKeysDisable)
      written should include(formatOtherKeysDisable)
      written should not include kittyPushFlags
    }

  "acquiring the shell against a terminal that confirms modifyOtherKeys with the wrong values" should
    "fall back to the Legacy tier just like no reply at all" in {
      val wrongValues = s"$esc[>4;0m$esc[>4;0f"
      val harness     = liveTerminal()
      replyAfterModifyOtherKeysQuery(harness, bytes(wrongValues))

      val tier =
        TerminalShell.forTerminal(harness.terminal).use(shell => IO(shell.keyboardProtocolTier)).unsafeRunSync()

      tier shouldBe TerminalShell.KeyboardProtocolTier.Legacy
    }

  "releasing a Kitty-tier shell on clean quit" should "pop the kitty enhancement flags" in {
    val harness = dumbTerminal(bytes(s"$esc[?1u"))

    TerminalShell.forTerminal(harness.terminal).use(_ => IO.unit).unsafeRunSync()

    harness.written should include(kittyPop)
  }

  // The release half of the quit chord is reported (kitty "report event types") while the protocol is still active,
  // but the input read loop is already gone by then, so it lands unread in the buffer. Left there, the shell the user
  // returns to prints it literally (`[113;5:3u`). restore() must drain it -- see TerminalShell.drainPendingInput.
  "releasing a Kitty-tier shell on clean quit" should
    "drain a buffered keyboard-protocol release event so the shell doesn't print it" in {
      val releaseEvent = s"$esc[113;5:3u"
      val harness      = dumbTerminal(bytes(s"$esc[?1u" + releaseEvent))

      TerminalShell.forTerminal(harness.terminal).use(_ => IO.unit).unsafeRunSync()

      // Negotiation consumes the kitty response; restore() must consume the trailing release event, leaving nothing
      // meaningful for the returning shell -- the next read is a sentinel (EOF/expired), not the ESC that opens it.
      val leftover = harness.terminal.reader().read(100L)
      leftover should (equal(org.jline.utils.NonBlockingReader.EOF) or
        equal(org.jline.utils.NonBlockingReader.READ_EXPIRED))
    }

  "releasing a confirmed ModifyOtherKeys-tier shell on clean quit" should
    "disable modifyOtherKeys and formatOtherKeys" in {
      val harness = liveTerminal()
      replyAfterModifyOtherKeysQuery(harness, bytes(confirmingReply))

      val tier =
        TerminalShell.forTerminal(harness.terminal).use(shell => IO(shell.keyboardProtocolTier)).unsafeRunSync()

      tier shouldBe TerminalShell.KeyboardProtocolTier.ModifyOtherKeys
      val written = harness.written
      written should include(modifyOtherKeysDisable)
      written should include(formatOtherKeysDisable)
    }

  "releasing a confirmed ModifyOtherKeys-tier shell after an escaping IO.raiseError" should
    "still disable the negotiated keyboard protocol" in {
      val harness = liveTerminal()
      replyAfterModifyOtherKeysQuery(harness, bytes(confirmingReply))
      val boom = new RuntimeException("boom")

      TerminalShell.forTerminal(harness.terminal).use(_ => IO.raiseError[Unit](boom)).attempt.unsafeRunSync()

      val written = harness.written
      written should include(modifyOtherKeysDisable)
      written should include(formatOtherKeysDisable)
    }

  "releasing a confirmed ModifyOtherKeys-tier shell after fiber cancellation" should
    "still disable the negotiated keyboard protocol" in {
      val harness = liveTerminal()
      replyAfterModifyOtherKeysQuery(harness, bytes(confirmingReply))

      val program = TerminalShell.forTerminal(harness.terminal).use(_ => IO.never)
      program.start.flatMap(fiber => IO.sleep(50.millis) >> fiber.cancel).unsafeRunSync()

      val written = harness.written
      written should include(modifyOtherKeysDisable)
      written should include(formatOtherKeysDisable)
    }

  "releasing a Legacy-tier shell (no confirmation reply) on clean quit" should
    "have already reverted modifyOtherKeys/formatOtherKeys during negotiation, and not disable again on release" in {
      val harness = dumbTerminal()

      TerminalShell.forTerminal(harness.terminal).use(_ => IO.unit).unsafeRunSync()

      val written = harness.written
      // Exactly one disable pair: written once, at negotiation time, when confirmation failed -- not a second time
      // on release, since KeyboardProtocolTier.Legacy's disableKeyboardProtocol branch is a no-op.
      countOccurrences(written, modifyOtherKeysDisable) shouldBe 1
      countOccurrences(written, formatOtherKeysDisable) shouldBe 1
    }

  private def countOccurrences(haystack: String, needle: String): Int =
    @annotation.tailrec
    def loop(from: Int, count: Int): Int =
      val idx = haystack.indexOf(needle, from)
      if idx < 0 then count else loop(idx + needle.length, count + 1)
    loop(0, 0)

  private def bytes(s: String): Array[Byte] = s.getBytes(StandardCharsets.UTF_8)
