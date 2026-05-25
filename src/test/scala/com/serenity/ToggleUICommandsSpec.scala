package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.CommandRegistry
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** TDD tests for toggleable UI commands functionality.
  *
  * Requirements:
  *   1. Command registry contains toggle commands for line numbers and gutter
  *   2. Commands can be found by search terms ("line", "gutter", "toggle")
  *   3. Executing toggle commands properly updates AppConfig
  *   4. Commands work correctly with current state (toggle on/off appropriately)
  *   5. Commands integrate properly with existing command runner system
  */
class ToggleUICommandsSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  behavior of "Toggle Line Numbers Command"

  it should "be found in command registry by search terms" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      registry = CommandRegistry.withToggleUIStateful(stateManager)
      
      // When: Search for line number related terms
      lineResults = registry.searchCommands("line")
      numberResults = registry.searchCommands("numbers")
      toggleResults = registry.searchCommands("toggle")
      
    yield
      // Then: Toggle line numbers command should be found
      lineResults.map(_.name) should contain("toggle-line-numbers")
      numberResults.map(_.name) should contain("toggle-line-numbers")
      toggleResults.map(_.name) should contain("toggle-line-numbers")
      
    program.unsafeRunSync()
  }

  it should "toggle line numbers from enabled to disabled" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      registry = CommandRegistry.withToggleUIStateful(stateManager)
      
      // Given: Line numbers are enabled by default
      initialState <- stateManager.getCurrentState
      _ = initialState.config.showLineNumbers shouldBe true
      
      // When: Execute toggle line numbers command
      toggleCommand = registry.findCommand("toggle-line-numbers").get
      _ <- toggleCommand.execute(initialState) // This should update the state through stateManager
      
      // Then: Line numbers should be disabled
      finalState <- stateManager.getCurrentState
      
    yield
      finalState.config.showLineNumbers shouldBe false
      
    program.unsafeRunSync()
  }

  it should "toggle line numbers from disabled to enabled" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      registry = CommandRegistry.withToggleUIStateful(stateManager)
      
      // Given: Line numbers are disabled
      _ <- stateManager.updateState(s => s.copy(config = s.config.copy(showLineNumbers = false)))
      initialState <- stateManager.getCurrentState
      _ = initialState.config.showLineNumbers shouldBe false
      
      // When: Execute toggle line numbers command
      toggleCommand = registry.findCommand("toggle-line-numbers").get
      _ <- toggleCommand.execute(initialState)
      
      // Then: Line numbers should be enabled
      finalState <- stateManager.getCurrentState
      
    yield
      finalState.config.showLineNumbers shouldBe true
      
    program.unsafeRunSync()
  }

  behavior of "Toggle Gutter Command"

  it should "be found in command registry by search terms" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      registry = CommandRegistry.withToggleUIStateful(stateManager)
      
      // When: Search for gutter related terms
      gutterResults = registry.searchCommands("gutter")
      statusResults = registry.searchCommands("status")
      toggleResults = registry.searchCommands("toggle")
      
    yield
      // Then: Toggle gutter command should be found
      gutterResults.map(_.name) should contain("toggle-gutter")
      statusResults.map(_.name) should contain("toggle-gutter") 
      toggleResults.map(_.name) should contain("toggle-gutter")
      
    program.unsafeRunSync()
  }

  it should "toggle gutter from enabled to disabled" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      registry = CommandRegistry.withToggleUIStateful(stateManager)
      
      // Given: Gutter is enabled by default
      initialState <- stateManager.getCurrentState
      _ = initialState.config.showGutter shouldBe true
      
      // When: Execute toggle gutter command
      toggleCommand = registry.findCommand("toggle-gutter").get
      _ <- toggleCommand.execute(initialState)
      
      // Then: Gutter should be disabled
      finalState <- stateManager.getCurrentState
      
    yield
      finalState.config.showGutter shouldBe false
      
    program.unsafeRunSync()
  }

  it should "toggle gutter from disabled to enabled" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      registry = CommandRegistry.withToggleUIStateful(stateManager)
      
      // Given: Gutter is disabled
      _ <- stateManager.updateState(s => s.copy(config = s.config.copy(showGutter = false)))
      initialState <- stateManager.getCurrentState
      _ = initialState.config.showGutter shouldBe false
      
      // When: Execute toggle gutter command  
      toggleCommand = registry.findCommand("toggle-gutter").get
      _ <- toggleCommand.execute(initialState)
      
      // Then: Gutter should be enabled
      finalState <- stateManager.getCurrentState
      
    yield
      finalState.config.showGutter shouldBe true
      
    program.unsafeRunSync()
  }

  behavior of "Combined Toggle UI Command Integration"

  it should "allow toggling both line numbers and gutter independently" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      registry = CommandRegistry.withToggleUIStateful(stateManager)
      
      // Given: Both enabled by default
      initialState <- stateManager.getCurrentState
      _ = initialState.config.showLineNumbers shouldBe true
      _ = initialState.config.showGutter shouldBe true
      
      // When: Toggle line numbers off, leave gutter on
      lineToggleCommand = registry.findCommand("toggle-line-numbers").get
      _ <- lineToggleCommand.execute(initialState)
      
      // Then: Line numbers disabled, gutter still enabled
      midState <- stateManager.getCurrentState
      _ = midState.config.showLineNumbers shouldBe false
      _ = midState.config.showGutter shouldBe true
      
      // When: Toggle gutter off too
      gutterToggleCommand = registry.findCommand("toggle-gutter").get
      _ <- gutterToggleCommand.execute(midState)
      
      // Then: Both disabled
      finalState <- stateManager.getCurrentState
      
    yield
      finalState.config.showLineNumbers shouldBe false
      finalState.config.showGutter shouldBe false
      
    program.unsafeRunSync()
  }

  it should "have descriptive command names and descriptions" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      registry = CommandRegistry.withToggleUIStateful(stateManager)
      
      // When: Find commands
      lineCommand = registry.findCommand("toggle-line-numbers").get
      gutterCommand = registry.findCommand("toggle-gutter").get
      
    yield
      // Then: Commands should have clear names and descriptions
      lineCommand.name shouldBe "toggle-line-numbers"
      lineCommand.description should include("line numbers")
      lineCommand.description.toLowerCase should include("toggle")
      
      gutterCommand.name shouldBe "toggle-gutter"
      gutterCommand.description should include("gutter")
      gutterCommand.description.toLowerCase should include("toggle")
      
    program.unsafeRunSync()
  }