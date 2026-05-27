package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.ui.layout.TerminalSize
import com.serenity.ui.renderer.RenderController
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class ResizeResponsivenessSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  "Immediate resize detection" should "trigger fast mode on resize for responsive rendering" in {
    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      
      // Simulate what happens in the main loop when input event triggers resize check
      fastModeTriggered <- IO.ref(false)
      onResized = fastModeTriggered.set(true)
      
      // This simulates checkResize being called during input event processing
      newSize = TerminalSize(100, 30)
      _ <- RenderController.handleResize(Some(newSize), stateManager, onResized)
      
      triggered <- fastModeTriggered.get
      finalState <- stateManager.getCurrentState
    yield
      // Fast mode should be triggered immediately when resize is detected
      triggered shouldBe true
      
      // State should be updated with new terminal size
      finalState.terminalSize shouldBe Some(newSize)

    program.unsafeRunSync()
  }

  it should "not trigger fast mode when no resize occurs" in {
    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      
      fastModeTriggered <- IO.ref(false)
      onResized = fastModeTriggered.set(true)
      
      // No resize detected
      _ <- RenderController.handleResize(None, stateManager, onResized)
      
      triggered <- fastModeTriggered.get
      finalState <- stateManager.getCurrentState
    yield
      // Fast mode should not be triggered when no resize occurs
      triggered shouldBe false

    program.unsafeRunSync()
  }