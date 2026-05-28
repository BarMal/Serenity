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

class ActualStartupFlowSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  behavior of "Actual Startup Flow (mimicking Main.scala)"

  it should "start with startup page focused when following Main.scala flow" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      // This mimics what Main.scala does
      logger <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      
      // 1. Create StateManager (this creates AppState.initial with editor pane)
      stateManager <- StateManager.apply(logger)
      
      // 2. Check what StateManager starts with
      stateAfterCreation <- stateManager.getCurrentState
      _ = println(s"State after StateManager creation:")
      _ = println(s"  Focus: ${stateAfterCreation.focus}")
      _ = println(s"  Editor panes: ${stateAfterCreation.layout.editorPanes.size}")
      _ = println(s"  Buffers: ${stateAfterCreation.buffers.size}")
      _ = println(s"  UI Surfaces: ${stateAfterCreation.uiSurfaces.size}")
      
      // 3. Initialize startup state (this should replace with startup page)
      theme = Theme.default
      terminalSize = TerminalSize(80, 24)
      initialState <- AppStartup.initializeState(stateManager, theme, terminalSize)
      
      // 4. Check final state
      _ = println(s"State after AppStartup.initializeState:")
      _ = println(s"  Focus: ${initialState.focus}")
      _ = println(s"  Editor panes: ${initialState.layout.editorPanes.size}")
      _ = println(s"  Buffers: ${initialState.buffers.size}")
      _ = println(s"  UI Surfaces: ${initialState.uiSurfaces.size}")
      _ = println(s"  Startup surface present: ${initialState.startPageSurface.isDefined}")
      
      // 5. Test that navigation works
      _ = println(s"Testing navigation on startup page...")
      _ <- stateManager.applyEvent(MoveDown)
      stateAfterNav <- stateManager.getCurrentState
      startPage = stateAfterNav.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _ = println(s"  Selected index after MoveDown: ${startPage.selectedIndex}")
      
    yield
      // Verify final state is correct
      initialState.focus shouldBe Focus.Surface(SurfaceId("surface-0"))
      initialState.startPageSurface should be (defined)
      initialState.layout.editorPanes shouldBe empty
      initialState.buffers shouldBe empty
      
      // Verify navigation worked
      startPage.selectedIndex shouldBe 1
    
    program.unsafeRunSync()
  }