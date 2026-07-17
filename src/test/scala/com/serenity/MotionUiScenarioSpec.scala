package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, MotionPreset}
import com.serenity.keystroke.events.ToggleCommandRunner
import com.serenity.rope.Balance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MotionUiScenarioSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  "Motion UI scenario" should "render an interrupted surface transition through its settled frame" in {
    val config = AppConfig.default.withMotionPreset(MotionPreset.Smooth)
    val driver = UiScenarioDriver.create("motion", initialConfig = config).unsafeRunSync()
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    val opening = driver.renderFrame("opening").unsafeRunSync()
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    val settled    = driver.advanceToSettled().unsafeRunSync()
    val finalFrame = driver.renderFrame("settled").unsafeRunSync()

    opening.evidence.layoutViolations shouldBe empty
    settled shouldBe true
    finalFrame.evidence.animationComplete shouldBe true
  }
