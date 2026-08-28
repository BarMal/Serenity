package com.serenity.ui.tui

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PipedInputStream, PipedOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import org.jline.terminal.Terminal
import org.jline.terminal.impl.DumbTerminal
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

/** End-to-end coverage for issue #1112: `serenity --tui somefile.md` running a full edit/save session against a real
  * JLine terminal -- a `DumbTerminal` over in-memory streams, the same harness #1107/#1108's specs use, not a
  * hand-rolled mock -- proving the exact capability bundle `Main`'s TUI branch wires together via [[TuiRuntime]]: typed
  * input decoded through `TerminalInputHandler`, edits applied through the unmodified `AppRuntime`/ `StateManager`
  * core, the result written back to disk on Ctrl+S, and Ctrl+Q (also EOF alone, covered separately) driving a clean
  * quit that restores the terminal -- exit the alternate screen, show the cursor again.
  */
class TuiRuntimeSpec extends AnyFlatSpec with Matchers with Eventually:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default
  given LoggerFactory[IO]         = Slf4jFactory.create[IO]
  given Logger[IO]                = LoggerFactory[IO].getLogger(using LoggerName("TuiRuntimeSpec"))

  implicit override val patienceConfig: PatienceConfig = PatienceConfig(timeout = Span(15, Seconds))

  private val esc         = 0x1b.toChar.toString
  private val exitCaMode  = s"$esc[?1049l"
  private val cursorShown = s"$esc[?12l$esc[?25h"

  private def ctrl(c: Char): Byte = (c.toUpper - 'A' + 1).toByte

  final private class StaticHarness(val terminal: Terminal, private val out: ByteArrayOutputStream):
    def written: String = out.toString(StandardCharsets.UTF_8)

  private def staticInputTerminal(input: Array[Byte]): StaticHarness =
    val in       = new ByteArrayInputStream(input)
    val out      = new ByteArrayOutputStream()
    val terminal = new DumbTerminal("test", "xterm-256color", in, out, StandardCharsets.UTF_8)
    terminal.setSize(new org.jline.terminal.Size(80, 24))
    new StaticHarness(terminal, out)

  final private class LiveHarness(
      val terminal: Terminal,
      private val pipeOut: PipedOutputStream,
      private val out: ByteArrayOutputStream
  ):
    def written: String = out.toString(StandardCharsets.UTF_8)

    def send(bytes: Array[Byte]): Unit =
      pipeOut.write(bytes)
      pipeOut.flush()

  private def liveInputTerminal(): LiveHarness =
    val pipeIn   = new PipedInputStream()
    val pipeOut  = new PipedOutputStream(pipeIn)
    val out      = new ByteArrayOutputStream()
    val terminal = new DumbTerminal("test", "xterm-256color", pipeIn, out, StandardCharsets.UTF_8)
    terminal.setSize(new org.jline.terminal.Size(80, 24))
    new LiveHarness(terminal, pipeOut, out)

  "TuiRuntime.run" should "run a full edit/save session in a terminal and restore it on Ctrl+Q quit" in {
    val file = Files.createTempFile("tui-runtime-spec", ".md")
    Files.writeString(file, "")
    val sessionRoot = Files.createTempDirectory("tui-runtime-spec-session")
    val harness     = liveInputTerminal()

    val program = TuiRuntime.run(
      shell = TerminalShell.forTerminal(harness.terminal),
      appConfig = AppConfig.default,
      openPath = Some(file),
      configPersistencePath = None,
      hasDisplay = false,
      sessionRootOverride = Some(sessionRoot)
    )

    val fiber = program.start.unsafeRunSync()

    harness.send(Array('h'.toByte, 'i'.toByte))
    harness.send(Array(ctrl('s')))

    eventually {
      Files.readString(file) shouldBe "hi"
    }

    harness.send(Array(ctrl('q')))

    fiber.joinWithNever.unsafeRunTimed(15.seconds) shouldBe defined
    harness.written should include(exitCaMode)
    harness.written should include(cursorShown)
  }

  it should "quit and restore the terminal on EOF alone, with no explicit Ctrl+Q" in {
    val file = Files.createTempFile("tui-runtime-eof-spec", ".md")
    Files.writeString(file, "hello")
    val sessionRoot = Files.createTempDirectory("tui-runtime-eof-spec-session")
    val harness     = staticInputTerminal(Array.emptyByteArray)

    val program = TuiRuntime.run(
      shell = TerminalShell.forTerminal(harness.terminal),
      appConfig = AppConfig.default,
      openPath = Some(file),
      configPersistencePath = None,
      hasDisplay = false,
      sessionRootOverride = Some(sessionRoot)
    )

    program.unsafeRunTimed(15.seconds) shouldBe defined
    harness.written should include(exitCaMode)
    harness.written should include(cursorShown)
  }

  it should "restore the terminal even when the runtime fails" in {
    val sessionRoot = Files.createTempDirectory("tui-runtime-failure-spec-session")
    val harness     = staticInputTerminal(Array.emptyByteArray)

    val program = TuiRuntime.run(
      shell = TerminalShell.forTerminal(harness.terminal),
      appConfig = AppConfig.default,
      // A path inside a directory that does not exist: StateManager.openFile fails opening it, which should still
      // leave the terminal properly restored via TerminalShell's Resource release.
      openPath = Some(java.nio.file.Path.of("/nonexistent-tui-runtime-failure-dir/does-not-exist.md")),
      configPersistencePath = None,
      hasDisplay = false,
      sessionRootOverride = Some(sessionRoot)
    )

    program.attempt.unsafeRunTimed(15.seconds) shouldBe defined
    harness.written should include(exitCaMode)
    harness.written should include(cursorShown)
  }
