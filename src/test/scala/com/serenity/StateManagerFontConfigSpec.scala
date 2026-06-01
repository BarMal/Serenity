package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{Enter, InsertChar, ToggleCommandRunner}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.ui.fonts.FontLoader.FontConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateManagerFontConfigSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(onFontConfigChanged: FontConfig => IO[Unit]): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerFontConfigSpec"))
    StateManager.apply(logger, onFontConfigChanged = onFontConfigChanged).unsafeRunSync()

  private def executeCommandThroughRunner(
    stateManager: StateManager,
    searchTerm: String,
    expectedCommandName: String
  ): Unit =
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    searchTerm.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    stateManager.getCurrentState.unsafeRunSync().commandRunnerSurface.flatMap {
      _.content match
        case com.serenity.state.models.SurfaceContent.CommandPalette(runner) => runner.selectedCommand.map(_.name)
        case _                                                               => None
    } shouldBe Some(expectedCommandName)

    stateManager.applyEvent(Enter).unsafeRunSync()

  "StateManager" should "invoke the runtime font callback when increasing font size" in {
    var observed: List[FontConfig] = Nil
    val stateManager = createStateManager(config => IO { observed = observed :+ config })

    executeCommandThroughRunner(stateManager, "increase-font-size", "increase-font-size")

    observed should not be empty
    observed.last.fontSize shouldBe 13.0f
  }

  it should "invoke the runtime font callback when toggling ligatures" in {
    var observed: List[FontConfig] = Nil
    val stateManager = createStateManager(config => IO { observed = observed :+ config })

    executeCommandThroughRunner(stateManager, "toggle-ligatures", "toggle-ligatures")

    observed should not be empty
    observed.last.enableLigatures shouldBe false
  }
