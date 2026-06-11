package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

class StartupCommandsSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  behavior of "Startup Commands"

  it should "open file workflow when third option (Open file) is selected" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec")
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      _ <- AppStartup.initializeState(stateManager, theme, viewportSize)
      _ <- stateManager.applyEvent(MoveDown)
      _ <- stateManager.applyEvent(MoveDown)

      stateAfterNav <- stateManager.getCurrentState
      startPage = stateAfterNav.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _         = startPage.selectedIndex shouldBe 2

      _          <- stateManager.applyEvent(Enter)
      finalState <- stateManager.getCurrentState
    yield
      // Startup page remains as the back-destination while the file modal is open
      finalState.startPageSurface should not be None

      // File workflow modal is open alongside the startup page
      val hasFileWorkflow = finalState.uiSurfaces.exists { surface =>
        surface.content match
          case SurfaceContent.ModalWorkflow(modal) =>
            modal match
              case Modal.FileWorkflow(_) => true
              case _                     => false
          case _ => false
      }
      hasFileWorkflow shouldBe true

    program.unsafeRunSync()
  }

  it should "return to startup page when Escape is pressed in the file workflow" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec")
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      _          <- AppStartup.initializeState(stateManager, theme, viewportSize)
      _          <- stateManager.applyEvent(MoveDown)
      _          <- stateManager.applyEvent(MoveDown)
      _          <- stateManager.applyEvent(Enter)
      _          <- stateManager.applyEvent(Escape)
      finalState <- stateManager.getCurrentState
    yield
      finalState.startPageSurface should not be None
      val hasFileWorkflow = finalState.uiSurfaces.exists { surface =>
        surface.content match
          case SurfaceContent.ModalWorkflow(modal) =>
            modal match
              case Modal.FileWorkflow(_) => true
              case _                     => false
          case _ => false
      }
      hasFileWorkflow shouldBe false
      finalState.focus match
        case Focus.Surface(_) => succeed
        case other            => fail(s"Expected startup page focus, got $other")

    program.unsafeRunSync()
  }

  it should "create a default editor session when no saved session exists" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec")
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      _ <- AppStartup.initializeState(stateManager, theme, viewportSize)
      _ <- stateManager.applyEvent(MoveDown)

      stateAfterNav <- stateManager.getCurrentState
      startPage = stateAfterNav.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _         = startPage.selectedIndex shouldBe 1

      _          <- stateManager.applyEvent(Enter)
      finalState <- stateManager.getCurrentState
    yield
      finalState.startPageSurface shouldBe None
      finalState.layout.editorPanes should not be empty
      finalState.buffers should not be empty
      finalState.focus match
        case Focus.EditorPane(_) => succeed
        case other               => fail(s"Expected editor pane focus, got $other")

    program.unsafeRunSync()
  }

  it should "include the new buffer in bufferOrder after starting a new session" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec")
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      _          <- AppStartup.initializeState(stateManager, theme, viewportSize)
      _          <- stateManager.applyEvent(Enter) // Select first option: New Session
      finalState <- stateManager.getCurrentState
    yield
      finalState.bufferOrder should not be empty
      finalState.buffers.keys.toList.foreach(bufferId => finalState.bufferOrder should contain(bufferId))

    program.unsafeRunSync()
  }
