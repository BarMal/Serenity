package com.serenity

import java.nio.file.Files
import java.security.MessageDigest

import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.config.{MarkdownViewMode, MotionPreset}
import com.serenity.keystroke.events.ToggleCommandRunner
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.ui.layout.ViewportSize
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

  it should "match durable visual references for narrow startup, prose, code, command runner, and settings workflows" in {
    val references = scala.collection.mutable.ListBuffer.empty[(String, ScenarioFrame)]
    List("dark", "light").foreach { themeName =>
      val environment = UiScenarioEnvironment(themeName = themeName)
      val theme       = if themeName == "light" then Theme.light else Theme.dark

      val narrowEnvironment = environment.copy(viewport = ViewportSize(48, 16))
      val startupDriver     = UiScenarioDriver.create(s"semantic-$themeName-startup", narrowEnvironment).unsafeRunSync()
      AppStartup.initializeState(startupDriver.stateManager, theme, narrowEnvironment.viewport).unsafeRunSync()
      val startup = startupDriver.renderFrame("startup").unsafeRunSync()
      startup.evidence.drawnText.map(_.text).mkString(" ") should include("Welcome to Serenity")
      startup.evidence.layoutViolations shouldBe empty
      references += s"$themeName-narrow-startup" -> startup

      val runnerDriver = UiScenarioDriver.create(s"semantic-$themeName-runner", environment).unsafeRunSync()
      runnerDriver.dispatch(ToggleCommandRunner).unsafeRunSync()
      val runner = runnerDriver.renderFrame("command-runner").unsafeRunSync()
      runner.evidence.surfaceRects should not be empty
      runner.evidence.drawnText.map(_.text).mkString(" ") should include("Open Settings")
      runner.evidence.layoutViolations shouldBe empty
      references += s"$themeName-command-runner" -> runner

      val settingsDriver = UiScenarioDriver.create(s"semantic-$themeName-settings", environment).unsafeRunSync()
      settingsDriver.stateManager
        .executeCommand(Command.typed("scenario-settings", "Scenario settings", CommandIntent.OpenSettings, CommandCategory.Settings))
        .unsafeRunSync()
      val settings = settingsDriver.renderFrame("settings").unsafeRunSync()
      settings.evidence.surfaceRects should not be empty
      settings.evidence.drawnText.map(_.text).mkString(" ") should include("Settings")
      settings.evidence.layoutViolations shouldBe empty
      references += s"$themeName-settings" -> settings

      val proseDriver = UiScenarioDriver.create(s"semantic-$themeName-prose", environment).unsafeRunSync()
      setDocument(proseDriver, "# Serenity notes\n\nQuiet prose keeps attention on the document.", LanguageId.Markdown)
      val prose = proseDriver.renderFrame("prose").unsafeRunSync()
      prose.evidence.drawnText.map(_.text).mkString(" ") should include("Serenity notes")
      prose.evidence.renderedContentRows should not be empty
      references += s"$themeName-prose" -> prose

      val codeDriver = UiScenarioDriver.create(s"semantic-$themeName-code", environment).unsafeRunSync()
      setDocument(codeDriver, "object Serenity:\n  val language = \"quiet\"", LanguageId.Scala)
      val code = codeDriver.renderFrame("code").unsafeRunSync()
      code.evidence.drawnText.map(_.text).mkString(" ") should include("object Serenity:")
      code.evidence.renderedContentRows should not be empty
      references += s"$themeName-code" -> code
    }

    references.map { case (name, frame) => name -> imageFingerprint(frame) }.toList shouldBe List(
      "dark-narrow-startup" -> "8fae1b3f70396aabbd9c89781d6213787f2cf9cfe6931e4c3cb70a04784b7fb9",
      "dark-command-runner" -> "8deeb4f89512b090b6ca14ac04b2156f49bc890f14881605ea07fcea899efc05",
      "dark-settings" -> "835fc293e40f62d21c6624b35a9f5318384c78ac4e7270a7bd2aa19d8d6a4e8b",
      "dark-prose" -> "6477b62668cf547e27e27b1ab71005bef91322c88b40d2defecb41ec8caaa8d3",
      "dark-code" -> "acd8cffe248beaf62b7c345162ae9c188a1184bd8dccec02ea470884a05766dc",
      "light-narrow-startup" -> "7ad39057c8790c0fc9ed598d6e004a59496fe9a04f85e08a0899ac307f269ab2",
      "light-command-runner" -> "c8b04468b51db51e0b1988b383d73b19e725d01808da77c7895eeb8aad758673",
      "light-settings" -> "2753529369db706d34467e6542c143d1f2d6e3a006da6bf715005919969c541e",
      "light-prose" -> "2f06aba7daa6b199897f86adb719cadcc51f5c8f8f1d2c25471a7634e411c1ee",
      "light-code" -> "a283db1fca24befef6d2f778ba339f592725192f4b3816ce85e045c329c65dcd"
    )
  }

  private def setDocument(driver: UiScenarioDriver, content: String, language: LanguageId): Unit =
    val bufferId = driver.state.unsafeRunSync().focusedBufferId.getOrElse(fail("Expected a focused scenario buffer"))
    driver.stateManager.updateBuffer(bufferId, content).unsafeRunSync()
    driver.updateState { state =>
      state.copy(buffers = state.buffers.updated(bufferId, state.buffers(bufferId).copy(language = Some(language))))
    }.unsafeRunSync()

  private def imageFingerprint(frame: ScenarioFrame): String =
    val digest = MessageDigest.getInstance("SHA-256")
    for
      y <- 0 until frame.image.getHeight
      x <- 0 until frame.image.getWidth
    do
      val pixel = frame.image.getRGB(x, y)
      digest.update((pixel >>> 24).toByte)
      digest.update((pixel >>> 16).toByte)
      digest.update((pixel >>> 8).toByte)
      digest.update(pixel.toByte)
    digest.digest.map("%02x".format(_)).mkString
