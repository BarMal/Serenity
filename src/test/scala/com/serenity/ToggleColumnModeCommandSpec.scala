package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.keystroke.events.{Enter, InsertChar, ToggleCommandRunner}
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.SurfaceContent
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Column-based document layout (issue #1338, Phase 1): the global `columnModeEnabled` toggle command, mirroring
  * `ToggleUICommandsSpec`'s "Toggle Typewriter Scrolling Command" coverage. Split into its own file (rather than grown
  * onto `ToggleUICommandsSpec`) to keep that file under the architecture ratchet's file-length target.
  */
class ToggleColumnModeCommandSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def createStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("ToggleColumnModeCommandSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def executeCommandThroughRunner(
    stateManager: StateManager,
    searchTerm: String,
    expectedCommandName: String
  ): Unit =
    val beforeOpen = stateManager.getCurrentState.unsafeRunSync()
    if beforeOpen.commandRunnerSurface
          .flatMap {
            _.content match
              case SurfaceContent.CommandPalette(runner) => Some(runner.isActive)
              case _                                     => None
          }
          .getOrElse(false) == false
    then stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    searchTerm.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.getCurrentState.unsafeRunSync().commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => runner.selectedCommand.map(_.name)
        case _                                     => None
    } shouldBe Some(expectedCommandName)
    stateManager.applyEvent(Enter).unsafeRunSync()

  behavior of "Toggle Column Mode Command"

  it should "be found in command registry by search terms" in {
    val registry = CommandRegistry.withToggleUI

    val columnResults = registry.searchCommands("column")
    val command       = registry.findCommand("toggle-column-mode").get

    columnResults.map(_.name) should contain("toggle-column-mode")
    command.intent shouldBe CommandIntent.Settings(
      SettingsIntent.TextDisplay(TextDisplayIntent.ToggleColumnMode)
    )
  }

  it should "toggle column mode from disabled to enabled" in {
    val stateManager = createStateManager()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .config
      .surfaceConfig
      .columnModeEnabled shouldBe false

    executeCommandThroughRunner(stateManager, "toggle-column-mode", "toggle-column-mode")

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.persisted.config.surfaceConfig.columnModeEnabled shouldBe true
  }

  it should "toggle column mode from enabled to disabled" in {
    val stateManager = createStateManager()

    stateManager
      .updateState(s => s.copy(persisted = s.persisted.copy(config = s.persisted.config.withColumnMode(true))))
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "toggle-column-mode", "toggle-column-mode")

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.persisted.config.surfaceConfig.columnModeEnabled shouldBe false
  }
