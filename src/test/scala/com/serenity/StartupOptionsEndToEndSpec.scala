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

class StartupOptionsEndToEndSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  behavior of "Startup Options End-to-End"

  it should "handle all three startup options correctly" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      // Test Option 1: New Session
      stateManager1 <- createStateManagerIO("StartupOptionsEndToEndSpec")
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)
      _ <- AppStartup.initializeState(stateManager1, theme, viewportSize)
      // Option 1 is selected by default, just press Enter
      _               <- stateManager1.applyEvent(Enter)
      newSessionState <- stateManager1.getCurrentState

      _ =
        newSessionState.startPageSurface shouldBe None
        newSessionState.layout.editorPanes should not be empty
        newSessionState.buffers should not be empty
        newSessionState.focus should matchPattern { case Focus.EditorPane(_) => }

      // Test Option 2: Restore Session
      stateManager2       <- createStateManagerIO("StartupOptionsEndToEndSpec")
      _                   <- AppStartup.initializeState(stateManager2, theme, viewportSize)
      _                   <- stateManager2.applyEvent(MoveDown) // Move to option 2
      _                   <- stateManager2.applyEvent(Enter)
      restoreSessionState <- stateManager2.getCurrentState

      _ =
        restoreSessionState.startPageSurface shouldBe None
        restoreSessionState.layout.editorPanes should not be empty
        restoreSessionState.buffers should not be empty
        restoreSessionState.focus should matchPattern { case Focus.EditorPane(_) => }

      // Test Option 3: Open File
      stateManager3 <- createStateManagerIO("StartupOptionsEndToEndSpec")
      _             <- AppStartup.initializeState(stateManager3, theme, viewportSize)
      _             <- stateManager3.applyEvent(MoveDown) // Move to option 2
      _             <- stateManager3.applyEvent(MoveDown) // Move to option 3
      _             <- stateManager3.applyEvent(Enter)
      openFileState <- stateManager3.getCurrentState

      _ =
        // Startup page remains as the back-destination while the file modal is open
        openFileState.startPageSurface should not be None

        // Should have file workflow modal open
        val hasFileWorkflow = openFileState.uiSurfaces.exists { surface =>
          surface.content match
            case SurfaceContent.ModalWorkflow(modal) =>
              modal match
                case Modal.FileWorkflow(_) => true
                case _                     => false
            case _ => false
        }
        hasFileWorkflow shouldBe true

        openFileState.focus should matchPattern { case Focus.Surface(_) => }
    yield succeed

    program.unsafeRunSync()
  }

  it should "handle navigation between all options correctly" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupOptionsEndToEndSpec")
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      _ <- AppStartup.initializeState(stateManager, theme, viewportSize)

      // Test full navigation cycle
      // Start at option 0
      state0 <- stateManager.getCurrentState
      startPage0 = state0.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _          = startPage0.selectedIndex shouldBe 0

      // Move down to option 1
      _      <- stateManager.applyEvent(MoveDown)
      state1 <- stateManager.getCurrentState
      startPage1 = state1.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _          = startPage1.selectedIndex shouldBe 1

      // Move down to option 2
      _      <- stateManager.applyEvent(MoveDown)
      state2 <- stateManager.getCurrentState
      startPage2 = state2.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _          = startPage2.selectedIndex shouldBe 2

      // Move down again (should wrap to option 0)
      _      <- stateManager.applyEvent(MoveDown)
      state3 <- stateManager.getCurrentState
      startPage3 = state3.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _          = startPage3.selectedIndex shouldBe 0

      // Move up (should wrap to option 2)
      _      <- stateManager.applyEvent(MoveUp)
      state4 <- stateManager.getCurrentState
      startPage4 = state4.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _          = startPage4.selectedIndex shouldBe 2
    yield succeed

    program.unsafeRunSync()
  }
