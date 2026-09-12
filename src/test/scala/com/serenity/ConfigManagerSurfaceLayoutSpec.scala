package com.serenity

import java.nio.file.Files

import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Surface layout and display settings: command runner spacing, render FPS, text area insets, cursor info bar alpha,
  * word wrap, pane headers, focused text body, contextual toolbar, and viewport sizing.
  */
class ConfigManagerSurfaceLayoutSpec extends AnyFlatSpec with Matchers with OptionValues:

  "ConfigManager" should "load and write command runner visible rows" in {
    val configFile = Files.createTempFile("serenity-command-rows-config", ".conf")
    Files.writeString(
      configFile,
      """command_runner.visible_rows = 9
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.surfaceConfig.commandRunnerVisibleRows shouldBe Some(9)
    ConfigManager.configToString(config) should include("command_runner.visible_rows = 9")
  }

  it should "load and write command runner item and cursor gaps independently" in {
    val configFile = Files.createTempFile("serenity-command-spacing-config", ".conf")
    Files.writeString(
      configFile,
      """command_runner.item_gap_rows = 1
        |command_runner.cursor_gap_rows = 3
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.surfaceConfig.commandRunnerItemGapRows shouldBe Some(1)
    config.surfaceConfig.commandRunnerCursorGapRows shouldBe Some(3)
    ConfigManager.configToString(config) should include("command_runner.item_gap_rows = 1")
    ConfigManager.configToString(config) should include("command_runner.cursor_gap_rows = 3")
  }

  it should "preserve decimal floating-surface spacing values" in {
    val configFile = Files.createTempFile("serenity-command-decimal-spacing-config", ".conf")
    Files.writeString(
      configFile,
      """command_runner.item_gap_rows = 0.25
        |command_runner.cursor_gap_rows = 0.5
        |ui.element_gap = 0.75
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.surfaceConfig.commandRunnerItemGapRows shouldBe Some(0.25)
    config.surfaceConfig.commandRunnerCursorGapRows shouldBe Some(0.5)
    config.uiElementGap shouldBe 0.75
    ConfigManager.configToString(config) should include("command_runner.item_gap_rows = 0.25")
    ConfigManager.configToString(config) should include("command_runner.cursor_gap_rows = 0.5")
    ConfigManager.configToString(config) should include("ui.element_gap = 0.75")
  }

  // issue #1046: item gap rows collapses onto the one "Interface Density" control -- with no explicit
  // `command_runner.item_gap_rows` override in the config file, the effective spacing now follows density instead
  // of a flat 0.0 regardless of density.
  it should "derive command runner item gap rows from interface density when no override is configured" in {
    val configFile = Files.createTempFile("serenity-command-density-spacing-config", ".conf")
    Files.writeString(configFile, "interface.density = spacious\n")

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.surfaceConfig.commandRunnerItemGapRows shouldBe None
    config.effectiveCommandRunnerItemGapRows shouldBe
      InterfaceDensityMetrics.forDensity(InterfaceDensity.Spacious).itemGapRows
    ConfigManager.configToString(config) should include("command_runner.item_gap_rows = auto")
  }

  it should "load and write render FPS targets" in {
    val configFile = Files.createTempFile("serenity-render-fps-config", ".conf")
    Files.writeString(
      configFile,
      """render.fps = 120
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.surfaceConfig.renderFpsTarget shouldBe RenderFpsTarget.Fps120
    ConfigManager.configToString(config) should include("render.fps = 120")
  }

  it should "load uncapped render FPS targets" in {
    val configFile = Files.createTempFile("serenity-render-fps-uncapped-config", ".conf")
    Files.writeString(
      configFile,
      """render.fps = uncapped
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.surfaceConfig.renderFpsTarget shouldBe RenderFpsTarget.Uncapped
    ConfigManager.configToString(config) should include("render.fps = uncapped")
  }

  it should "load and write text area inset percentages" in {
    val configFile = Files.createTempFile("serenity-text-area-config", ".conf")
    Files.writeString(
      configFile,
      """text_area.left.percent = 12.5
        |text_area.right.percent = 20
        |text_area.top.percent = 7.5
        |text_area.bottom.percent = 10
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.surfaceConfig.textAreaInsets.left shouldBe 0.125 +- 0.0001
    config.surfaceConfig.textAreaInsets.right shouldBe 0.20 +- 0.0001
    config.surfaceConfig.textAreaInsets.top shouldBe 0.075 +- 0.0001
    config.surfaceConfig.textAreaInsets.bottom shouldBe 0.10 +- 0.0001
    ConfigManager.configToString(config) should include("text_area.left.percent = 12.5")
    ConfigManager.configToString(config) should include("text_area.right.percent = 20.0")
    ConfigManager.configToString(config) should include("text_area.top.percent = 7.5")
    ConfigManager.configToString(config) should include("text_area.bottom.percent = 10.0")
  }

  it should "load and write the renderer frame-state cache capacity" in {
    val configFile = Files.createTempFile("serenity-render-frame-state-cache-capacity-config", ".conf")
    Files.writeString(
      configFile,
      """render.frame_state_cache_capacity = 128
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.surfaceConfig.rendererFrameStateCacheCapacity shouldBe 128
    ConfigManager.configToString(config) should include("render.frame_state_cache_capacity = 128")
  }

  it should "load and write the cursor info bar background alpha override" in {
    val configFile = Files.createTempFile("serenity-cursor-info-bar-alpha-config", ".conf")
    Files.writeString(
      configFile,
      """display.cursor_info_bar_background_alpha = 0.5
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.surfaceConfig.cursorInfoBarBackgroundAlpha shouldBe Some(0.5)
    ConfigManager.configToString(config) should include("display.cursor_info_bar_background_alpha = 0.5")
  }

  it should "default the cursor info bar background alpha to unset (theme default) when not configured" in {
    ConfigManager.configToString(AppConfig.default) should include(
      "display.cursor_info_bar_background_alpha = auto"
    )
    AppConfig.default.surfaceConfig.cursorInfoBarBackgroundAlpha shouldBe None
  }

  it should "reject a cursor info bar background alpha outside 0.0-1.0" in {
    val configFile = Files.createTempFile("serenity-cursor-info-bar-alpha-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """display.cursor_info_bar_background_alpha = 1.5
        |""".stripMargin
    )

    val result = ConfigManager.loadConfigResult(Some(configFile.toString))

    result.config.surfaceConfig.cursorInfoBarBackgroundAlpha shouldBe None
    result.report.invalidEntries.map(_.key) should contain("display.cursor_info_bar_background_alpha")
  }

  it should "load and write word wrap display mode" in {
    val configFile = Files.createTempFile("serenity-word-wrap-config", ".conf")
    Files.writeString(
      configFile,
      """display.word_wrap = false
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.surfaceConfig.wordWrapEnabled shouldBe false
    ConfigManager.configToString(config) should include("display.word_wrap = false")
  }

  it should "load and write pane header display mode" in {
    val configFile = Files.createTempFile("serenity-pane-header-config", ".conf")
    Files.writeString(configFile, "display.pane_headers = false\n")

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.surfaceConfig.showPaneHeaders shouldBe false
    ConfigManager.configToString(config) should include("display.pane_headers = false")
  }

  it should "load and write focused text body display mode" in {
    val configFile = Files.createTempFile("serenity-focused-body-config", ".conf")
    Files.writeString(
      configFile,
      """display.focused_text_body = true
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.surfaceConfig.focusedTextBodyEnabled shouldBe true
    ConfigManager.configToString(config) should include("display.focused_text_body = true")
  }

  it should "load and write contextual toolbar display mode" in {
    val configFile = Files.createTempFile("serenity-contextual-toolbar-display-config", ".conf")
    Files.writeString(
      configFile,
      """display.contextual_toolbar_mode = text-only
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.surfaceConfig.contextualToolbarDisplayMode shouldBe ToolbarDisplayMode.TextOnly
    ConfigManager.configToString(config) should include("display.contextual_toolbar_mode = text-only")
  }

  it should "report invalid surface display config values through the surface schema" in {
    val configFile = Files.createTempFile("serenity-surface-display-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """command_runner.visible_rows = 0
        |render.fps = turbo
        |display.word_wrap = maybe
        |display.contextual_toolbar_mode = pictures
        |""".stripMargin
    )

    val result = ConfigManager.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain("command_runner.visible_rows")
    result.report.invalidEntries.map(_.key) should contain("render.fps")
    result.report.invalidEntries.map(_.key) should contain("display.word_wrap")
    result.report.invalidEntries.map(_.key) should contain("display.contextual_toolbar_mode")
  }

  it should "report invalid surface layout config values through the surface schema" in {
    val configFile = Files.createTempFile("serenity-surface-layout-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """text_area.left.percent = 60
        |viewport.width.percent = 0
        |viewport.height.max = 0
        |""".stripMargin
    )

    val result = ConfigManager.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain("text_area.left.percent")
    result.report.invalidEntries.map(_.key) should contain("viewport.width.percent")
    result.report.invalidEntries.map(_.key) should contain("viewport.height.max")
  }

  it should "load and write viewport sizing policy" in {
    val configFile = Files.createTempFile("serenity-viewport-config", ".conf")
    Files.writeString(
      configFile,
      """viewport.width.percent = 80
        |viewport.width.max =
        |viewport.height.percent = 100
        |viewport.height.max = 50
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.surfaceConfig.viewportSizing.width.percent shouldBe 0.8
    config.surfaceConfig.viewportSizing.width.maxCells shouldBe None
    config.surfaceConfig.viewportSizing.height.percent shouldBe 1.0
    config.surfaceConfig.viewportSizing.height.maxCells shouldBe Some(50)
    ConfigManager.configToString(config) should include("viewport.width.percent = 80.0")
    ConfigManager.configToString(config) should include("viewport.width.max = ")
    ConfigManager.configToString(config) should include("viewport.height.percent = 100.0")
    ConfigManager.configToString(config) should include("viewport.height.max = 50")
  }
