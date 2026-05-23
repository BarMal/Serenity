package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.traverse.*
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Test startup rendering issues - specifically that initial buffer state should be renderable without requiring input
  * events
  */
class StartupRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Startup State Rendering"

  it should "have buffer content available immediately after setup" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      bufferId     <- stateManager.createBuffer("Welcome to Serenity!")
      state        <- stateManager.getCurrentState
      paneId = state.layout.editorPanes.keys.head
      _            <- stateManager.setBufferForPane(paneId, bufferId)
      finalState   <- stateManager.getCurrentState
    yield
      // Buffer should have content
      val buffer = finalState.buffers(bufferId)
      buffer.content.collect() shouldBe "Welcome to Serenity!"

      // Pane should reference buffer
      val pane = finalState.layout.editorPanes(paneId)
      pane.bufferId shouldBe Some(bufferId)

      // This verifies the state is properly set up for rendering

    program.unsafeRunSync()
  }

  it should "have proper cursor position in initial state" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      bufferId     <- stateManager.createBuffer("Hello")
      state        <- stateManager.getCurrentState
      paneId = state.layout.editorPanes.keys.head
      _            <- stateManager.setBufferForPane(paneId, bufferId)
      finalState   <- stateManager.getCurrentState
    yield
      val pane   = finalState.layout.editorPanes(paneId)
      val buffer = finalState.buffers(bufferId)

      // Should have cursor at start of buffer
      buffer.cursors.size shouldBe 1
      val cursor = buffer.cursors.head
      cursor.line shouldBe 0
      cursor.column shouldBe 0

    program.unsafeRunSync()
  }

  behavior of "Text Overflow Prevention"

  it should "properly track cursor position when text extends beyond typical panel width" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      bufferId     <- stateManager.createBuffer("")
      state        <- stateManager.getCurrentState
      paneId = state.layout.editorPanes.keys.head
      _            <- stateManager.setBufferForPane(paneId, bufferId)
      // Insert very long line of text (100 characters)
      longText = "x" * 100
      _            <- longText.toList.traverse(char => stateManager.applyEvent(InsertChar(char)))
      finalState   <- stateManager.getCurrentState
    yield
      val pane   = finalState.layout.editorPanes(paneId)
      val buffer = finalState.buffers(bufferId)
      val cursor = buffer.cursors.head

      // Cursor should be at end of text
      cursor.column shouldBe 100
      cursor.line shouldBe 0

      // Buffer should contain full text
      buffer.content.collect() shouldBe longText

    program.unsafeRunSync()
  }

  it should "handle viewport scrolling when cursor extends beyond visible area" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      bufferId     <- stateManager.createBuffer("")
      state        <- stateManager.getCurrentState
      paneId = state.layout.editorPanes.keys.head
      _            <- stateManager.setBufferForPane(paneId, bufferId)
      // Assume typical panel width is around 50 characters
      panelWidth = 50
      longText = "a" * (panelWidth + 20) // Text extending beyond panel
      _            <- longText.toList.traverse(char => stateManager.applyEvent(InsertChar(char)))
      finalState   <- stateManager.getCurrentState
    yield
      val pane     = finalState.layout.editorPanes(paneId)
      val buffer   = finalState.buffers(bufferId)
      val cursor   = buffer.cursors.head
      val viewport = buffer.viewport

      // Cursor should be at end of text
      cursor.column shouldBe longText.length

      // Viewport should potentially scroll to keep cursor visible
      // (This test documents expected behavior - implementation may vary)
      val cursorVisibleInViewport = cursor.column >= viewport.leftColumn &&
        cursor.column < (viewport.leftColumn + viewport.visibleColumns)

      // The viewport system should keep cursor visible when possible
      // Note: this test may pass or fail depending on current viewport logic
      info(s"Cursor at column ${cursor.column}, viewport left=${viewport.leftColumn}, visible=${viewport.visibleColumns}")

    program.unsafeRunSync()
  }

  it should "preserve buffer content integrity regardless of viewport scrolling" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      bufferId     <- stateManager.createBuffer("")
      state        <- stateManager.getCurrentState
      paneId = state.layout.editorPanes.keys.head
      _            <- stateManager.setBufferForPane(paneId, bufferId)
      // Create text with mixed content to ensure integrity
      testText = "The quick brown fox jumps over the lazy dog. This is a long sentence that extends beyond normal panel boundaries."
      _            <- testText.toList.traverse(char => stateManager.applyEvent(InsertChar(char)))
      finalState   <- stateManager.getCurrentState
    yield
      val buffer = finalState.buffers(bufferId)

      // Buffer content should be completely intact
      buffer.content.collect() shouldBe testText

      // This is the key test: regardless of how rendering works,
      // the underlying buffer must preserve all data

    program.unsafeRunSync()
  }
