package com.serenity

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.config.{MarkdownViewMode, MotionPreset}
import com.serenity.keystroke.events.ToggleCommandRunner
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

  it should "load the isolated scenario configuration fixture" in {
    val driver = UiScenarioDriver.create("isolated-config", isolatedConfig = true).unsafeRunSync()
    val config = driver.state.unsafeRunSync().config

    config.motionPreset shouldBe MotionPreset.Reduced
    config.markdownViewMode shouldBe MarkdownViewMode.InlineLens
    config.commandRunnerItemGapRows shouldBe 0
  }

  it should "capture startup, command runner, and settings references in both bundled themes" in {
    val artifacts = Files.createTempDirectory("semantic-ui-references")

    List("dark", "light").foreach { themeName =>
      val environment = UiScenarioEnvironment(themeName = themeName)
      val driver      = UiScenarioDriver.create(s"semantic-$themeName", environment, Some(artifacts)).unsafeRunSync()
      val theme       = if themeName == "light" then Theme.light else Theme.dark

      AppStartup.initializeState(driver.stateManager, theme, environment.viewport).unsafeRunSync()
      val startup = driver.renderFrame(s"$themeName-startup").unsafeRunSync()
      startup.evidence.drawnText.map(_.text).mkString(" ") should include("Welcome to Serenity")

      val runnerDriver = UiScenarioDriver.create(s"semantic-$themeName-runner", environment, Some(artifacts)).unsafeRunSync()
      runnerDriver.dispatch(ToggleCommandRunner).unsafeRunSync()
      val runner = runnerDriver.renderFrame(s"$themeName-command-runner").unsafeRunSync()
      runner.evidence.surfaceRects should not be empty

      val settingsDriver = UiScenarioDriver.create(s"semantic-$themeName-settings", environment, Some(artifacts)).unsafeRunSync()
      settingsDriver.stateManager
        .executeCommand(Command.typed("scenario-settings", "Scenario settings", CommandIntent.OpenSettings, CommandCategory.Settings))
        .unsafeRunSync()
      val settings = settingsDriver.renderFrame(s"$themeName-settings").unsafeRunSync()
      settings.evidence.surfaceRects should not be empty
      settings.evidence.drawnText.map(_.text).mkString(" ") should include("Settings")

      Files.exists(artifacts.resolve(s"$themeName-startup.png")) shouldBe true
      Files.exists(artifacts.resolve(s"$themeName-command-runner.png")) shouldBe true
      Files.exists(artifacts.resolve(s"$themeName-settings.png")) shouldBe true
    }
  }
