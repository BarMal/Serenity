package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerUiScenarioSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  "Command runner UI scenario" should "search, expose matching click targets, and close and reopen" in {
    val driver = UiScenarioDriver.create("command-runner").unsafeRunSync()
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    "toggle-line".foreach(char => driver.dispatch(InsertChar(char)).unsafeRunSync())
    val searched  = driver.renderFrame("searched").unsafeRunSync()
    val surfaceId = searched.evidence.surfaceRects.keys.headOption.getOrElse(fail("Expected command runner"))
    val target =
      searched.evidence.itemRects.getOrElse(surfaceId, Nil).headOption.getOrElse(fail("Expected item target"))

    driver.dispatch(MouseMove(target.x + 1, target.y)).unsafeRunSync()
    val hovered = driver.state.unsafeRunSync().commandRunnerSurface.getOrElse(fail("Expected command runner"))
    val selectedIndex = hovered.content match
      case com.serenity.state.models.SurfaceContent.CommandPalette(runner) => runner.selectedIndex
      case _                                                               => fail("Expected command palette")
    selectedIndex shouldBe 0
    val beforeClick = driver.state.unsafeRunSync().config.showLineNumbers
    driver.dispatch(MouseClick(target.x + 1, target.y)).unsafeRunSync()
    driver.state.unsafeRunSync().config.showLineNumbers shouldBe !beforeClick
    searched.evidence.layoutViolations shouldBe empty
    driver.dispatch(Escape).unsafeRunSync()
    driver.advanceToSettled().unsafeRunSync() shouldBe true
    driver.renderFrame("closed").unsafeRunSync().evidence.surfaceRects shouldBe empty
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    driver.renderFrame("reopened").unsafeRunSync().evidence.surfaceRects should not be empty
  }
