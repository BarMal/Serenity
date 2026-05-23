package com.serenity

import com.serenity.state.manager.StateManager
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}
import cats.effect.IO
import cats.effect.unsafe.implicits.global

class MainStartupSpec extends AnyFlatSpec with Matchers:

  behavior of "Main Application Startup"

  it should "simulate the Main.scala startup sequence with exactly 1 pane" in {
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    // Given: Simulate Main.scala startup sequence
    val logger = LoggerFactory[IO].getLogger(using LoggerName("Main"))
    
    // When: Follow the same steps as Main.scala (after fix)
    val result = for {
      // This follows the exact sequence in Main.scala run method
      themeManager <- IO.pure(AppThemeManager.create)
      defaultTheme <- themeManager.initializeWithTheme()
      stateManager <- StateManager.apply(logger)
      // Note: No longer creating extra buffer and pane here
      _            <- stateManager.updateState(_.copy(theme = defaultTheme))
      finalState   <- stateManager.getCurrentState
    } yield (stateManager, finalState)
    
    val (stateManager, finalState) = result.unsafeRunSync()
    
    // Then: Should have exactly 1 pane and 1 buffer
    finalState.layout.editorPanes.should(have).size(1)
    finalState.buffers.should(have).size(1)
    
    // And the pane should have a buffer
    val paneId = finalState.layout.editorPanes.keys.head
    val pane = finalState.layout.editorPanes(paneId)
    pane.bufferId.shouldBe(defined)
    
    val bufferId = pane.bufferId.get
    finalState.buffers.should(contain).key(bufferId)
    
    // And focus should be correct
    finalState.focus.shouldBe(com.serenity.state.models.Focus.EditorPane(paneId))
    finalState.layout.activeEditorPaneId.shouldBe(Some(paneId))
    
    println(s"Final startup state: ${finalState.layout.editorPanes.size} panes, ${finalState.buffers.size} buffers")
  }