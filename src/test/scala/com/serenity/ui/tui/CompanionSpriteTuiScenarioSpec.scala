package com.serenity.ui.tui

import java.io.{PipedInputStream, PipedOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import org.jline.terminal.impl.DumbTerminal
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

/** End-to-end TUI coverage for the companion sprite pane: a real `TuiRuntime.run` against a `DumbTerminal` (the same
  * harness `TuiRuntimeSpec` drives), with the emitted ANSI replayed through [[TerminalEmulator]] -- the mock terminal
  * every TUI behaviour spec asserts through -- so the assertion is against the resulting on-screen cell grid, not
  * against raw escape bytes or the half-block algorithm in isolation (see `HalfBlockImageRendererSpec` for that unit
  * coverage).
  */
class CompanionSpriteTuiScenarioSpec extends AnyFlatSpec with Matchers with Eventually:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default
  given LoggerFactory[IO]         = Slf4jFactory.create[IO]
  given Logger[IO]                = LoggerFactory[IO].getLogger(using LoggerName("CompanionSpriteTuiScenarioSpec"))

  implicit override val patienceConfig: PatienceConfig = PatienceConfig(timeout = Span(15, Seconds))

  "the companion sprite pane" should "paint real half-block glyphs onto the terminal when enabled" in {
    val file        = Files.createTempFile("companion-sprite-tui-spec", ".md")
    val sessionRoot = Files.createTempDirectory("companion-sprite-tui-spec-session")
    val out         = new java.io.ByteArrayOutputStream()
    val pipeIn      = new PipedInputStream()
    val pipeOut     = new PipedOutputStream(pipeIn)
    val terminal    = new DumbTerminal("test", "xterm-256color", pipeIn, out, StandardCharsets.UTF_8)
    terminal.setSize(new org.jline.terminal.Size(160, 40))

    val config = AppConfig.default.withCompanionSpriteConfig(
      AppConfig.default.companionSpriteConfig.copy(enabled = true)
    )

    val program = TuiRuntime.run(
      shell = TerminalShell.forTerminal(terminal),
      appConfig = config,
      openPath = Some(file),
      configPersistencePath = None,
      hasDisplay = false,
      sessionRootOverride = Some(sessionRoot)
    )

    val fiber = program.start.unsafeRunSync()

    eventually {
      val screen = TerminalEmulator.blank(160, 40).consume(out.toString(StandardCharsets.UTF_8))
      val glyphCells = for
        row <- 0 until 40
        col <- 0 until 160
        if screen.cellAt(col, row).text == HalfBlockImageRenderer.UpperHalfBlock.toString
      yield (col, row)
      glyphCells should not be empty
    }

    pipeOut.write(Array(17: Byte)) // Ctrl+Q
    pipeOut.flush()
    fiber.joinWithNever.unsafeRunTimed(15.seconds) shouldBe defined
  }

  it should "not paint any half-block glyphs when disabled" in {
    val file        = Files.createTempFile("companion-sprite-tui-disabled-spec", ".md")
    val sessionRoot = Files.createTempDirectory("companion-sprite-tui-disabled-spec-session")
    val out         = new java.io.ByteArrayOutputStream()
    val pipeIn      = new PipedInputStream()
    val pipeOut     = new PipedOutputStream(pipeIn)
    val terminal    = new DumbTerminal("test", "xterm-256color", pipeIn, out, StandardCharsets.UTF_8)
    terminal.setSize(new org.jline.terminal.Size(160, 40))

    val program = TuiRuntime.run(
      shell = TerminalShell.forTerminal(terminal),
      appConfig = AppConfig.default,
      openPath = Some(file),
      configPersistencePath = None,
      hasDisplay = false,
      sessionRootOverride = Some(sessionRoot)
    )

    val fiber = program.start.unsafeRunSync()

    eventually {
      out.toString(StandardCharsets.UTF_8) should include("Line 1, Col 1")
    }

    val screen = TerminalEmulator.blank(160, 40).consume(out.toString(StandardCharsets.UTF_8))
    val glyphCells = for
      row <- 0 until 40
      col <- 0 until 160
      if screen.cellAt(col, row).text == HalfBlockImageRenderer.UpperHalfBlock.toString
    yield (col, row)
    glyphCells shouldBe empty

    pipeOut.write(Array(17: Byte))
    pipeOut.flush()
    fiber.joinWithNever.unsafeRunTimed(15.seconds) shouldBe defined
  }
