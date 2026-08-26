package com.serenity

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.config.{AppConfig, MarkdownViewMode, MaterialPreset, MotionPreset}
import com.serenity.keystroke.events.ToggleCommandRunner
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.SurfaceMaterials
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

  it should "match stable semantic and region references for narrow startup, prose, code, command runner, and settings workflows" in
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
      assertVisualReference(
        startup,
        narrowEnvironment,
        expectedForegrounds = Set(theme.foreground, theme.selection.foreground),
        expectedBackgrounds = Set(theme.background, theme.selection.background),
        expectedFocusStyle = true
      )

      val crystalConfig = AppConfig.default.withMaterialPreset(MaterialPreset.Crystal)
      val runnerDriver = UiScenarioDriver
        .create(s"semantic-$themeName-runner", environment, initialConfig = crystalConfig)
        .unsafeRunSync()
      runnerDriver.dispatch(ToggleCommandRunner).unsafeRunSync()
      val runner = runnerDriver.renderFrame("command-runner").unsafeRunSync()
      runner.evidence.surfaceRects should not be empty
      runner.evidence.drawnText.map(_.text).mkString(" ") should include("Open Settings")
      runner.evidence.layoutViolations shouldBe empty
      runner.evidence.styleCalls should contain(ScenarioStyleCall("enable", theme.focusStyle))
      assertVisualReference(
        runner,
        environment,
        expectedForegrounds = Set(theme.panel.foreground, theme.selection.foreground),
        expectedBackgrounds = Set(theme.panel.background, theme.selection.background),
        expectedFocusStyle = true,
        expectedSurfaceCount = 1,
        expectedBorderColor = Some(theme.focus),
        expectedSheen = SurfaceMaterials.glassSheenBackground(crystalConfig, theme)
      )

      val settingsDriver = UiScenarioDriver.create(s"semantic-$themeName-settings", environment).unsafeRunSync()
      settingsDriver.stateManager
        .executeCommand(
          Command.typed("scenario-settings", "Scenario settings", CommandIntent.OpenSettings, CommandCategory.Settings)
        )
        .unsafeRunSync()
      val settings = settingsDriver.renderFrame("settings").unsafeRunSync()
      settings.evidence.surfaceRects should not be empty
      settings.evidence.drawnText.map(_.text).mkString(" ") should include("Settings")
      settings.evidence.layoutViolations shouldBe empty
      assertVisualReference(
        settings,
        environment,
        expectedForegrounds = Set(theme.panel.foreground),
        expectedBackgrounds = Set(theme.panel.background),
        expectedSurfaceCount = 1,
        expectedBorderColor = Some(theme.focus)
      )

      val proseDriver = UiScenarioDriver.create(s"semantic-$themeName-prose", environment).unsafeRunSync()
      setDocument(proseDriver, "# Serenity notes\n\nQuiet prose keeps attention on the document.", LanguageId.Markdown)
      val prose = proseDriver.renderFrame("prose").unsafeRunSync()
      prose.evidence.drawnText.map(_.text).mkString(" ") should include("Serenity notes")
      prose.evidence.renderedContentRows should not be empty
      assertVisualReference(
        prose,
        environment,
        expectedForegrounds = Set(theme.foreground),
        expectedBackgrounds = Set(theme.background)
      )

      val codeDriver = UiScenarioDriver.create(s"semantic-$themeName-code", environment).unsafeRunSync()
      setDocument(codeDriver, "object Serenity:\n  val language = \"quiet\"", LanguageId.Scala)
      val code = codeDriver.renderFrame("code").unsafeRunSync()
      code.evidence.drawnText.map(_.text).mkString(" ") should include("object Serenity:")
      code.evidence.renderedContentRows should not be empty
      assertVisualReference(
        code,
        environment,
        expectedForegrounds = Set(theme.foreground),
        expectedBackgrounds = Set(theme.background)
      )
    }

  private def setDocument(driver: UiScenarioDriver, content: String, language: LanguageId): Unit =
    val bufferId = driver.state.unsafeRunSync().focusedBufferId.getOrElse(fail("Expected a focused scenario buffer"))
    driver.stateManager.updateBuffer(bufferId, content).unsafeRunSync()
    driver
      .updateState { state =>
        val buffer = state.buffers(bufferId)
        state.copy(buffers =
          state.buffers.updated(bufferId, buffer.copy(document = buffer.document.copy(language = Some(language))))
        )
      }
      .unsafeRunSync()

  private def assertVisualReference(
    frame: ScenarioFrame,
    environment: UiScenarioEnvironment,
    expectedForegrounds: Set[java.awt.Color],
    expectedBackgrounds: Set[java.awt.Color],
    expectedFocusStyle: Boolean = false,
    expectedSurfaceCount: Int = 0,
    expectedBorderColor: Option[java.awt.Color] = None,
    expectedSheen: Option[java.awt.Color] = None
  ): Unit =
    frame.image.getWidth shouldBe environment.viewport.width * environment.cellMetrics.charWidth
    frame.image.getHeight shouldBe environment.viewport.height * environment.cellMetrics.lineHeight
    frame.evidence.drawnText should not be empty
    frame.evidence.paintedRegions.map(_.foreground).toSet should contain allElementsOf expectedForegrounds
    frame.evidence.paintedRegions.map(_.background).toSet should contain allElementsOf expectedBackgrounds
    frame.evidence.surfaceRects.size should be >= expectedSurfaceCount
    if expectedFocusStyle then
      frame.evidence.styleCalls should contain(ScenarioStyleCall("enable", com.serenity.ui.theme.TextStyle.bold))
    expectedBorderColor.foreach { color =>
      frame.evidence.borders.map(_.color) should contain(color)
      frame.evidence.surfaceRects.values.foreach { surfaceRect =>
        frame.evidence.borders should contain(ScenarioBorder(surfaceRect, color))
      }
    }
    expectedSheen.foreach(color => frame.evidence.paintedRegions.map(_.background) should contain(color))
    frame.evidence.drawnText.foreach { drawnText =>
      drawnText.bounds.x should be >= 0
      drawnText.bounds.x should be < environment.viewport.width
      drawnText.bounds.y should be >= 0
      drawnText.bounds.y should be < environment.viewport.height
    }
