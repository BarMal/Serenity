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
    "line".foreach(char => driver.dispatch(InsertChar(char)).unsafeRunSync())
    val searched = driver.renderFrame("searched").unsafeRunSync()
    val surfaceId = searched.evidence.surfaceRects.keys.headOption.getOrElse(fail("Expected command runner"))

    searched.evidence.itemRects.getOrElse(surfaceId, Nil) should not be empty
    searched.evidence.layoutViolations shouldBe empty
    driver.dispatch(Escape).unsafeRunSync()
    driver.advanceToSettled().unsafeRunSync() shouldBe true
    driver.renderFrame("closed").unsafeRunSync().evidence.surfaceRects shouldBe empty
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    driver.renderFrame("reopened").unsafeRunSync().evidence.surfaceRects should not be empty
  }
