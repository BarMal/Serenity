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

class StartupPageIntegrationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  behavior of "Startup Page Integration"

  it should "allow navigation through startup options and execute selection" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      theme = Theme.default
      terminalSize = TerminalSize(80, 24)
      
      // Initialize startup state
      initialState <- AppStartup.initializeState(stateManager, theme, terminalSize)
      
      // Verify we start with startup page focused
      _ = initialState.focus shouldBe Focus.Surface(SurfaceId("surface-0"))
      _ = initialState.startPageSurface should be (defined)
      startPage = initialState.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _ = startPage.selectedIndex shouldBe 0
      
      // Navigate down one option
      _ <- stateManager.applyEvent(MoveDown)
      state1 <- stateManager.getCurrentState
      startPage1 = state1.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _ = startPage1.selectedIndex shouldBe 1
      
      // Navigate down again
      _ <- stateManager.applyEvent(MoveDown)
      state2 <- stateManager.getCurrentState
      startPage2 = state2.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _ = startPage2.selectedIndex shouldBe 2
      
      // Navigate down once more (should wrap to first option)
      _ <- stateManager.applyEvent(MoveDown)
      state3 <- stateManager.getCurrentState
      startPage3 = state3.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _ = startPage3.selectedIndex shouldBe 0
      
      // Navigate up (should wrap to last option)
      _ <- stateManager.applyEvent(MoveUp)
      state4 <- stateManager.getCurrentState
      startPage4 = state4.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _ = startPage4.selectedIndex shouldBe 2
      
      // Reset to first option and select it (new session)
      _ <- stateManager.applyEvent(MoveDown)
      _ <- stateManager.applyEvent(Enter)
      finalState <- stateManager.getCurrentState
      
      // After selecting "new session", startup page should be dismissed and we should have editor
      _ = finalState.startPageSurface shouldBe None
      _ = finalState.layout.editorPanes.size shouldBe 1
      _ = finalState.buffers.size shouldBe 1
      
    yield ()
    
    program.unsafeRunSync()
  }

  it should "dismiss startup page on Escape" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      theme = Theme.default
      terminalSize = TerminalSize(80, 24)
      
      // Initialize startup state  
      initialState <- AppStartup.initializeState(stateManager, theme, terminalSize)
      _ = initialState.startPageSurface should be (defined)
      
      // Press escape to dismiss
      _ <- stateManager.applyEvent(Escape)
      finalState <- stateManager.getCurrentState
      
      // Startup page should be dismissed
      _ = finalState.startPageSurface shouldBe None
      
    yield ()
    
    program.unsafeRunSync()
  }