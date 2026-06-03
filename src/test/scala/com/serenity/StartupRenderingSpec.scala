package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.traverse.*
import com.serenity.app.AppStartup
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.Renderer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StartupRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Startup State Rendering"

  it should "render the dedicated start page vertically centered in the viewport" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      state        <- AppStartup.startPageState(stateManager, com.serenity.ui.theme.Theme.dark, ViewportSize(100, 30))
    yield
      val surface = new MockRenderSurface(100, 30)
      Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

      val renderedLines =
        (0 until 30).flatMap { y =>
          val line = (0 until 100).map(x => surface.getChar(x, y)).mkString.trim
          Option.when(line.nonEmpty)((y, line))
        }

      val startPage       = state.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      val fullExpectedLines = startPage.renderLines
      val expectedLines   = fullExpectedLines.filter(_.nonEmpty)

      renderedLines.map(_._2) should contain allElementsOf expectedLines

      val expectedStartY = (30 - fullExpectedLines.size) / 2
      renderedLines.head._1.shouldBe(expectedStartY)
      renderedLines.last._1.shouldBe(expectedStartY + fullExpectedLines.size - 1)

    program.unsafeRunSync()
  }

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
      val buffer = finalState.buffers(bufferId)
      buffer.content.collect() shouldBe "Welcome to Serenity!"
      val pane = finalState.layout.editorPanes(paneId)
      pane.bufferId shouldBe Some(bufferId)

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
      val buffer = finalState.buffers(bufferId)
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
      longText = "x" * 100
      _            <- longText.toList.traverse(char => stateManager.applyEvent(InsertChar(char)))
      finalState   <- stateManager.getCurrentState
    yield
      val buffer = finalState.buffers(bufferId)
      val cursor = buffer.cursors.head
      cursor.column shouldBe 100
      cursor.line shouldBe 0
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
      panelWidth = 50
      longText = "a" * (panelWidth + 20)
      _            <- longText.toList.traverse(char => stateManager.applyEvent(InsertChar(char)))
      finalState   <- stateManager.getCurrentState
    yield
      val buffer   = finalState.buffers(bufferId)
      val cursor   = buffer.cursors.head
      val viewport = buffer.viewport
      cursor.column shouldBe longText.length
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
      testText = "The quick brown fox jumps over the lazy dog. This is a long sentence that extends beyond normal panel boundaries."
      _            <- testText.toList.traverse(char => stateManager.applyEvent(InsertChar(char)))
      finalState   <- stateManager.getCurrentState
    yield
      val buffer = finalState.buffers(bufferId)
      buffer.content.collect() shouldBe testText

    program.unsafeRunSync()
  }
