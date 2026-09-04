package com.serenity.ui.tui

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PipedInputStream, PipedOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import com.serenity.state.models.{Buffer, BufferId, CursorPosition, Viewport}
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

  it should "keep editing working across a terminal focus-out/focus-in round trip (CSI O / CSI I, #1171)" in {
    val file = Files.createTempFile("tui-runtime-focus-spec", ".md")
    Files.writeString(file, "")
    val sessionRoot = Files.createTempDirectory("tui-runtime-focus-spec-session")
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

    // Losing then regaining focus (terminal focus reporting, CSI O / CSI I) should be silently absorbed -- parking
    // and resuming the idle cursor tick internally -- rather than being decoded as ordinary keystrokes or otherwise
    // disrupting the input stream.
    harness.send(esc.getBytes(StandardCharsets.UTF_8) ++ "[O".getBytes(StandardCharsets.UTF_8))
    harness.send(esc.getBytes(StandardCharsets.UTF_8) ++ "[I".getBytes(StandardCharsets.UTF_8))
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

  // -- The screen the real runtime paints -------------------------------------------------------------------------
  //
  // The deterministic behaviour suite (TuiHarnessSpec and friends) drives the same wiring without AppRuntime's own
  // loops, which is what makes it fast and free of timing. These two anchor it to the real thing: `TuiRuntime.run`
  // itself, its render loop, its input loop, with the bytes it wrote replayed through the same mock terminal the
  // behaviour suite asserts through -- so if the two ever disagreed about what reaches the screen, this would say so.

  private def screenOf(harness: LiveHarness): TerminalEmulator =
    TerminalEmulator.blank(80, 24).consume(harness.written)

  it should "paint the opened file, the gutter and the status bar onto the real terminal" in {
    val file = Files.createTempFile("tui-runtime-screen-spec", ".md")
    Files.writeString(file, "alpha\nbeta")
    val sessionRoot = Files.createTempDirectory("tui-runtime-screen-spec-session")
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

    eventually {
      val screen = screenOf(harness)
      screen.inAlternateScreen shouldBe true
      screen.rowText(1).stripTrailing shouldBe " 1 alpha"
      screen.rowText(2).stripTrailing shouldBe " 2 beta"
      screen.rowText(23) should include("Line 1, Col 1")
    }

    harness.send(Array(ctrl('q')))
    fiber.joinWithNever.unsafeRunTimed(15.seconds) shouldBe defined
  }

  it should "repaint typed characters and move the terminal's own caret with them" in {
    val file = Files.createTempFile("tui-runtime-typing-spec", ".md")
    Files.writeString(file, "")
    val sessionRoot = Files.createTempDirectory("tui-runtime-typing-spec-session")
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

    harness.send("typed".getBytes(StandardCharsets.UTF_8))

    eventually {
      val screen = screenOf(harness)
      screen.rowText(1).stripTrailing shouldBe " 1 typed"
      // The gutter is three cells wide, so the caret sits just past the five characters typed after it.
      screen.cursor.col shouldBe 8
      screen.cursor.row shouldBe 1
      screen.cursor.visible shouldBe true
    }

    // Quitting with unsaved changes would raise the "unsaved changes" prompt instead of exiting, so save first.
    harness.send(Array(ctrl('s')))
    eventually {
      Files.readString(file) shouldBe "typed"
    }

    harness.send(Array(ctrl('q')))
    fiber.joinWithNever.unsafeRunTimed(15.seconds) shouldBe defined

    // Restoring the terminal leaves the alternate screen and shows the cursor again.
    val finalScreen = screenOf(harness)
    finalScreen.inAlternateScreen shouldBe false
    finalScreen.cursor.visible shouldBe true
  }

  "TuiRuntime.markdownPreviewSourceWindow" should "return an empty window for an empty buffer" in {
    val buffer = Buffer.fromString(BufferId(1), "")

    TuiRuntime.markdownPreviewSourceWindow(buffer, heightPx = 800) shouldBe
      com.serenity.markdown.MarkdownDocumentPreview.PreviewWindow(0, 0, "")
  }

  it should "follow the editor viewport's top line when the cursor is inside the visible window" in {
    val lines = (0 until 100).map(i => s"line $i").mkString("\n")
    val baseBuffer = Buffer
      .fromString(BufferId(1), lines)
      .copy(viewport = Viewport.default.copy(topLine = 20))
    // A cursor within the window the viewport already implies: firstSourceLine should follow the viewport's top
    // line rather than the cursor recentering it.
    val buffer = baseBuffer.copy(editing = baseBuffer.editing.copy(cursors = List(CursorPosition(25, 0))))

    val window = TuiRuntime.markdownPreviewSourceWindow(buffer, heightPx = 320)

    window.firstSourceLine shouldBe 20
    window.source should startWith("line 20")
  }

  it should "recenter on the cursor line when it has scrolled past the current window" in {
    val lines = (0 until 200).map(i => s"line $i").mkString("\n")
    val baseBuffer = Buffer
      .fromString(BufferId(1), lines)
      .copy(viewport = Viewport.default.copy(topLine = 0))
    val buffer = baseBuffer.copy(editing = baseBuffer.editing.copy(cursors = List(CursorPosition(150, 0))))

    val window = TuiRuntime.markdownPreviewSourceWindow(buffer, heightPx = 320)

    window.firstSourceLine should (be > 0 and be <= 150)
    window.source should include("line 150")
  }
