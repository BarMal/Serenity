package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.TerminalSize
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StartupCommandsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  behavior of "Startup Commands"

  it should "open file workflow when third option (Open file) is selected" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      theme = Theme.default
      terminalSize = TerminalSize(80, 24)
      
      // Initialize startup state
      initialState <- AppStartup.initializeState(stateManager, theme, terminalSize)
      
      // Navigate to third option (Open file)
      _ <- stateManager.applyEvent(MoveDown) // Move to second option
      _ <- stateManager.applyEvent(MoveDown) // Move to third option  
      
      // Verify we're on the third option
      stateAfterNav <- stateManager.getCurrentState
      startPage = stateAfterNav.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _ = startPage.selectedIndex shouldBe 2
      
      // Select the third option (Open file)
      _ <- stateManager.applyEvent(Enter)
      finalState <- stateManager.getCurrentState
      
      // Debug output
      _ = println(s"Final state focus: ${finalState.focus}")
      _ = println(s"Final state surfaces: ${finalState.uiSurfaces.size}")
      _ = println(s"Surface types: ${finalState.uiSurfaces.map(_.content.getClass.getSimpleName)}")
      _ = finalState.uiSurfaces.foreach { surface =>
        println(s"Surface ${surface.id}: ${surface.content}")
      }
      
    yield 
      // Should have dismissed startup page
      finalState.startPageSurface shouldBe None
      
      // Should have opened file workflow modal
      finalState.uiSurfaces should not be empty
      val hasFileWorkflow = finalState.uiSurfaces.exists { surface =>
        surface.content match
          case SurfaceContent.ModalWorkflow(modal) =>
            modal match
              case Modal.FileWorkflow(_) => true
              case _ => false
          case _ => false
      }
      hasFileWorkflow shouldBe true
    
    program.unsafeRunSync()
  }

  it should "create a default editor session when no saved session exists" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      theme = Theme.default
      terminalSize = TerminalSize(80, 24)
      
      // Initialize startup state
      initialState <- AppStartup.initializeState(stateManager, theme, terminalSize)
      
      // Navigate to second option (Restore session)
      _ <- stateManager.applyEvent(MoveDown) // Move to second option
      
      // Verify we're on the second option
      stateAfterNav <- stateManager.getCurrentState
      startPage = stateAfterNav.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _ = startPage.selectedIndex shouldBe 1
      
      // Select the second option (Restore session)  
      _ <- stateManager.applyEvent(Enter)
      finalState <- stateManager.getCurrentState
      
      // Debug output
      _ = println(s"After restore session - Focus: ${finalState.focus}")
      _ = println(s"After restore session - Surfaces: ${finalState.uiSurfaces.size}")
      
    yield
      // Should have dismissed startup page
      finalState.startPageSurface shouldBe None
      
      // Should have created a default editor session
      finalState.layout.editorPanes should not be empty
      finalState.buffers should not be empty
      finalState.focus match
        case Focus.EditorPane(_) => succeed
        case other => fail(s"Expected editor pane focus, got $other")
    
    program.unsafeRunSync()
  }
