package com.serenity.ui.tui

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
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

  "acquiring the shell against a terminal that never answers the kitty query" should
    "fall back to modifyOtherKeys and report the ModifyOtherKeys tier" in {
      val harness = dumbTerminal()

      val tier =
        TerminalShell.forTerminal(harness.terminal).use(shell => IO(shell.keyboardProtocolTier)).unsafeRunSync()

      tier shouldBe TerminalShell.KeyboardProtocolTier.ModifyOtherKeys
      val written = harness.written
      written should include(kittyQuery)
      written should include(modifyOtherKeysEnable)
      written should include(formatOtherKeysEnable)
      written should not include kittyPushFlags
    }

  "releasing a Kitty-tier shell on clean quit" should "pop the kitty enhancement flags" in {
    val harness = dumbTerminal(bytes(s"$esc[?1u"))

    TerminalShell.forTerminal(harness.terminal).use(_ => IO.unit).unsafeRunSync()

    harness.written should include(kittyPop)
  }

  "releasing a ModifyOtherKeys-tier shell on clean quit" should "disable modifyOtherKeys and formatOtherKeys" in {
    val harness = dumbTerminal()

    TerminalShell.forTerminal(harness.terminal).use(_ => IO.unit).unsafeRunSync()

    val written = harness.written
    written should include(modifyOtherKeysDisable)
    written should include(formatOtherKeysDisable)
  }

  "releasing a ModifyOtherKeys-tier shell after an escaping IO.raiseError" should
    "still disable the negotiated keyboard protocol" in {
      val harness = dumbTerminal()
      val boom    = new RuntimeException("boom")

      TerminalShell.forTerminal(harness.terminal).use(_ => IO.raiseError[Unit](boom)).attempt.unsafeRunSync()

      val written = harness.written
      written should include(modifyOtherKeysDisable)
      written should include(formatOtherKeysDisable)
    }

  "releasing a ModifyOtherKeys-tier shell after fiber cancellation" should
    "still disable the negotiated keyboard protocol" in {
      val harness = dumbTerminal()

      val program = TerminalShell.forTerminal(harness.terminal).use(_ => IO.never)
      program.start.flatMap(fiber => IO.sleep(50.millis) >> fiber.cancel).unsafeRunSync()

      val written = harness.written
      written should include(modifyOtherKeysDisable)
      written should include(formatOtherKeysDisable)
    }

  private def bytes(s: String): Array[Byte] = s.getBytes(StandardCharsets.UTF_8)
