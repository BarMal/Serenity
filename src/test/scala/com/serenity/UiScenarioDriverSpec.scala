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
      startup.evidence.styleCalls should contain(ScenarioStyleCall("enable", theme.focusStyle))
      references += s"$themeName-narrow-startup" -> startup

      val runnerDriver = UiScenarioDriver.create(s"semantic-$themeName-runner", environment).unsafeRunSync()
      runnerDriver.dispatch(ToggleCommandRunner).unsafeRunSync()
      val runner = runnerDriver.renderFrame("command-runner").unsafeRunSync()
      runner.evidence.surfaceRects should not be empty
      runner.evidence.drawnText.map(_.text).mkString(" ") should include("Open Settings")
      runner.evidence.layoutViolations shouldBe empty
      runner.evidence.styleCalls should contain(ScenarioStyleCall("enable", theme.focusStyle))
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
      "dark-narrow-startup" -> "30aaac3585dd338c4afa1c2035d0fb1c581bd9d829a7dfae50244c20de58fd4e",
      "dark-command-runner" -> "8deeb4f89512b090b6ca14ac04b2156f49bc890f14881605ea07fcea899efc05",
      "dark-settings" -> "1558add743329961bed6b4544bce3fd1dd955f83ba5885723b38f103e1b43dba",
      "dark-prose" -> "6477b62668cf547e27e27b1ab71005bef91322c88b40d2defecb41ec8caaa8d3",
      "dark-code" -> "acd8cffe248beaf62b7c345162ae9c188a1184bd8dccec02ea470884a05766dc",
      "light-narrow-startup" -> "61238c78eb2b902196ec94486a8b3ba2f1f6508ab1a90cc7ee81204273eda0c6",
      "light-command-runner" -> "c8b04468b51db51e0b1988b383d73b19e725d01808da77c7895eeb8aad758673",
      "light-settings" -> "30ebf5c35e2b548ff11a14e80c96c4e43274bcf7dc216ff513b232cadb86c679",
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
