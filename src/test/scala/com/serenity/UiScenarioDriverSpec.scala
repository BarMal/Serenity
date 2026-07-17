package com.serenity

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UiScenarioDriverSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  "UiScenarioDriver" should "render deterministic light and dark frames at 1x and 2x device scale" in {
    for
      theme <- List("light", "dark")
      scale <- List(1.0, 2.0)
    do
      val environment = UiScenarioEnvironment(deviceScale = scale, themeName = theme)
      val driver      = UiScenarioDriver.create(s"variant-$theme-$scale", environment).unsafeRunSync()
      val first       = driver.renderFrame("first").unsafeRunSync()
      val second      = driver.renderFrame("second").unsafeRunSync()

      first.image.getWidth shouldBe (environment.viewport.width * environment.cellMetrics.charWidth * scale).toInt
      first.image.getHeight shouldBe (environment.viewport.height * environment.cellMetrics.lineHeight * scale).toInt
      first.image.getRGB(0, 0) shouldBe second.image.getRGB(0, 0)
      driver.state.unsafeRunSync().theme shouldBe (if theme == "light" then Theme.light else Theme.dark)
  }

  it should "emit semantic diagnostics and a PNG for a controlled red/green regression" in {
    val artifacts = Files.createTempDirectory("ui-scenario-artifacts")
    val driver = UiScenarioDriver
      .create("controlled-regression", artifactDirectory = Some(artifacts))
      .unsafeRunSync()

    val failure = intercept[AssertionError] {
      driver.verifyFrame("red")(_ => Left("controlled Markdown collapse")).unsafeRunSync()
    }
    failure.getMessage should include("controlled Markdown collapse")
    failure.getMessage should include("focus=")
    Files.exists(artifacts.resolve("red.png")) shouldBe true

    noException should be thrownBy driver.verifyFrame("green")(_ => Right(())).unsafeRunSync()
  }
