package com.serenity

import java.awt.Color
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.animation.{AnimationConfig, TransitionKind}
import com.serenity.config.*
import com.serenity.keystroke.{InputKey, Modifier}
import com.serenity.lsp.config.{LanguageId, LspServerOverride}
import com.serenity.ui.fonts.FontLoader.TextScaleMode
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConfigManagerSpec extends AnyFlatSpec with Matchers with OptionValues:

  "ConfigManager" should "load configured hotkey overrides from a config file" in {
    val configFile = Files.createTempFile("serenity-config", ".conf")
    Files.writeString(
      configFile,
      """hotkey.command_palette = ctrl+k
        |hotkey.file_search = ctrl+alt+f
        |hotkey.previous_tab = ctrl+shift+tab
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.hotkeyConfig
      .bindingsFor(HotkeyAction.ToggleCommandRunner)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Character,
      character = Some('k'),
      modifiers = Set(Modifier.Ctrl)
    )
    config.hotkeyConfig
      .bindingsFor(HotkeyAction.FileSearch)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Character,
      character = Some('f'),
      modifiers = Set(Modifier.Ctrl, Modifier.Alt)
    )
    config.hotkeyConfig
      .bindingsFor(HotkeyAction.PreviousTab)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Tab,
      character = None,
      modifiers = Set(Modifier.Ctrl, Modifier.Shift)
    )
  }

  it should "load and write meta hotkey overrides using command-key aliases" in {
    val configFile = Files.createTempFile("serenity-config", ".conf")
    Files.writeString(
      configFile,
      """hotkey.command_palette = cmd+p
        |hotkey.file_search = command+shift+f
        |keymap.command_runner.submit = meta+enter
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.hotkeyConfig
      .bindingsFor(HotkeyAction.ToggleCommandRunner)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Character,
      character = Some('p'),
      modifiers = Set(Modifier.Meta)
    )
    config.hotkeyConfig
      .bindingsFor(HotkeyAction.FileSearch)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Character,
      character = Some('f'),
      modifiers = Set(Modifier.Meta, Modifier.Shift)
    )
    config.focusedKeymapConfig.commandRunner
      .bindingsFor(CommandRunnerKeyAction.Submit)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Enter,
      character = None,
      modifiers = Set(Modifier.Meta)
    )

    val written = ConfigManager.configToString(config)
    written should include("hotkey.command_palette = meta+p")
    written should include("hotkey.file_search = meta+shift+f")
    written should include("keymap.command_runner.submit = meta+enter")
  }

  it should "load configuration through the effectful blocking-safe API" in {
    val configFile = Files.createTempFile("serenity-config-io", ".conf")
    Files.writeString(
      configFile,
      """syntax.highlighting = true
        |font.size = 18.0
        |""".stripMargin
    )

    val config = ConfigManager.loadConfigIO(Some(configFile.toString)).unsafeRunSync()

    config.syntaxHighlightingEnabled shouldBe true
    config.fontConfig.fontSize shouldBe 18.0f
  }

  it should "load configuration with a structured migration report" in {
    val configFile = Files.createTempFile("serenity-config-result", ".conf")
    Files.writeString(
      configFile,
      """font_size = 18.0
        |unknown.setting = yes
        |syntax.highlighting = maybe
        |""".stripMargin
    )

    val result = ConfigManager.loadConfigResult(Some(configFile.toString))

    result.config.fontConfig.codeFontSize shouldBe 18.0f
    result.config.fontConfig.textFontSize shouldBe 18.0f
    result.report.deprecatedEntries.map(_.key) should contain("font_size")
    result.report.deprecatedEntries.map(_.replacement) should contain("font.code.size and font.text.size")
    result.report.unknownKeys should contain("unknown.setting")
    result.report.invalidEntries.map(_.key) should contain("syntax.highlighting")
    result.report.hasWarnings shouldBe true
  }

  it should "report invalid language-tool config values through the language-tools schema" in {
    val configFile = Files.createTempFile("serenity-language-tools-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """syntax.highlighting = maybe
        |spellcheck.enabled = perhaps
        |""".stripMargin
    )

    val result = ConfigManager.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain("syntax.highlighting")
    result.report.invalidEntries.map(_.key) should contain("spellcheck.enabled")
  }

  it should "return default config result with an empty report when the config file is missing" in {
    val missingConfig = Files.createTempDirectory("serenity-missing-config-result").resolve("missing.conf")

    val result = ConfigManager.loadConfigResult(Some(missingConfig.toString))

    result.config shouldBe AppConfig.default
    result.report.hasWarnings shouldBe false
  }

  it should "return defaults through the effectful API when the config file is missing" in {
    val missingConfig = Files.createTempDirectory("serenity-missing-config").resolve("missing.conf")

    ConfigManager.loadConfigIO(Some(missingConfig.toString)).unsafeRunSync() shouldBe AppConfig.default
  }

  it should "parse richer key trigger names for local keymap overrides" in {
    val configFile = Files.createTempFile("serenity-config", ".conf")
    Files.writeString(
      configFile,
      """keymap.editor.page_down = ctrl+pagedown
        |keymap.editor.extend_selection_right = shift+right
        |keymap.command_runner.submit = ctrl+enter
        |keymap.modal.dismiss = ctrl+escape
        |keymap.panel.activate = ctrl+enter
        |keymap.peek.accept = ctrl+enter
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.focusedKeymapConfig.editor
      .bindingsFor(EditorKeyAction.PageDown)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.PageDown,
      character = None,
      modifiers = Set(Modifier.Ctrl)
    )
    config.focusedKeymapConfig.editor
      .bindingsFor(EditorKeyAction.ExtendSelectionRight)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.ArrowRight,
      character = None,
      modifiers = Set(Modifier.Shift)
    )
    config.focusedKeymapConfig.commandRunner
      .bindingsFor(CommandRunnerKeyAction.Submit)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Enter,
      character = None,
      modifiers = Set(Modifier.Ctrl)
    )
    config.focusedKeymapConfig.modal
      .bindingsFor(ModalKeyAction.Dismiss)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Escape,
      character = None,
      modifiers = Set(Modifier.Ctrl)
    )
    config.focusedKeymapConfig.panel
      .bindingsFor(PanelKeyAction.Activate)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Enter,
      character = None,
      modifiers = Set(Modifier.Ctrl)
    )
    config.focusedKeymapConfig.peek
      .bindingsFor(PeekKeyAction.Accept)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Enter,
      character = None,
      modifiers = Set(Modifier.Ctrl)
    )
  }

  it should "load and write font configuration including UI font family" in {
    val configFile = Files.createTempFile("serenity-font-config", ".conf")
    Files.writeString(
      configFile,
      """font.code.family = Monospaced
        |font.text.family = Serif
        |font.ui.family = Dialog
        |font.code.size = 15.0
        |font.text.size = 16.0
        |font.ui.size = 13.0
        |font.scale.mode = manual
        |font.text_scale = 1.5
        |font.code.ligatures = false
        |font.text.ligatures = true
        |font.ui.ligatures = true
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.fontConfig.codeFontFamily shouldBe "Monospaced"
    config.fontConfig.textFontFamily shouldBe "Serif"
    config.fontConfig.uiFontFamily shouldBe "Dialog"
    config.fontConfig.codeFontSize shouldBe 15.0f
    config.fontConfig.textFontSize shouldBe 16.0f
    config.fontConfig.uiFontSize shouldBe 13.0f
    config.fontConfig.textScaleMode shouldBe TextScaleMode.Manual
    config.fontConfig.textScaleMultiplier shouldBe 1.5
    config.fontConfig.codeLigatures shouldBe false
    config.fontConfig.textLigatures shouldBe true
    config.fontConfig.uiLigatures shouldBe true

    ConfigManager.configToString(config) should include("font.ui.family = Dialog")
    ConfigManager.configToString(config) should include("font.code.size = 15.0")
    ConfigManager.configToString(config) should include("font.text.size = 16.0")
    ConfigManager.configToString(config) should include("font.scale.mode = manual")
    ConfigManager.configToString(config) should include("font.text_scale = 1.5")
    ConfigManager.configToString(config) should include("font.ui.ligatures = true")
    ConfigManager.configToString(config) should include("config.version = 1")
  }

  it should "write default editor selection-extension keymap bindings" in {
    val written = ConfigManager.configToString(AppConfig.default)

    written should include("keymap.editor.extend_selection_left = shift+left")
    written should include("keymap.editor.extend_selection_right = shift+right")
    written should include("keymap.editor.extend_selection_up = shift+up")
    written should include("keymap.editor.extend_selection_down = shift+down")
  }

  it should "fall back to default editor key bindings when writing sparse keymap config" in {
    val config  = AppConfig.default.withFocusedKeymapConfig(FocusedKeymapConfig(editor = EditorKeymapConfig(Map.empty)))
    val written = ConfigManager.configToString(config)

    written should include("keymap.editor.page_down = pagedown")
    written should include("keymap.editor.extend_selection_right = shift+right")
  }

  it should "load legacy shared font size and ligature keys for code and prose fonts" in {
    val configFile = Files.createTempFile("serenity-legacy-font-config", ".conf")
    Files.writeString(
      configFile,
      """font.size = 17.0
        |font.ligatures = false
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.fontConfig.codeFontSize shouldBe 17.0f
    config.fontConfig.textFontSize shouldBe 17.0f
    config.fontConfig.codeLigatures shouldBe false
    config.fontConfig.textLigatures shouldBe false
    config.fontConfig.uiLigatures shouldBe false
  }

  it should "load and write active and inactive cursor colour overrides" in {
    val configFile = Files.createTempFile("serenity-cursor-config", ".conf")
    Files.writeString(
      configFile,
      """cursor.active.color = #3366CC
        |cursor.inactive.color = #CC663380
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.cursorColors.active shouldBe Some(new Color(0x33, 0x66, 0xcc))
    config.cursorColors.inactive shouldBe Some(new Color(0xcc, 0x66, 0x33, 0x80))
    ConfigManager.configToString(config) should include("cursor.active.color = #3366CC")
    ConfigManager.configToString(config) should include("cursor.inactive.color = #CC663380")
  }

  it should "load and write cursor mode" in {
    val configFile = Files.createTempFile("serenity-cursor-mode-config", ".conf")
    Files.writeString(
      configFile,
      """cursor.mode = breathe
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.cursorMode shouldBe CursorMode.Breathe
    ConfigManager.configToString(config) should include("cursor.mode = breathe")
  }

  it should "load and write cursor information bar mode" in {
    val configFile = Files.createTempFile("serenity-cursor-info-config", ".conf")
    Files.writeString(
      configFile,
      """cursor.info_bar = detailed
        |cursor.info_bar.placement = pinned-bottom
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.cursorInfoBarMode shouldBe CursorInfoBarMode.Detailed
    config.cursorInfoBarPlacement shouldBe CursorInfoBarPlacement.PinnedBottom
    ConfigManager.configToString(config) should include("cursor.info_bar = detailed")
    ConfigManager.configToString(config) should include("cursor.info_bar.placement = pinned-bottom")
  }

  it should "ignore invalid cursor colour overrides" in {
    val configFile = Files.createTempFile("serenity-cursor-config", ".conf")
    Files.writeString(
      configFile,
      """cursor.active.color = not-a-colour
        |cursor.inactive.color = #xyz
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.cursorColors.active shouldBe None
    config.cursorColors.inactive shouldBe None
  }

  it should "report invalid cursor config values through the cursor schema" in {
    val configFile = Files.createTempFile("serenity-cursor-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """cursor.mode = unknown
        |cursor.active.color = not-a-colour
        |cursor.info_bar = sideways
        |cursor.info_bar.placement = upside-down
        |""".stripMargin
    )

    val result = ConfigManager.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain("cursor.mode")
    result.report.invalidEntries.map(_.key) should contain("cursor.active.color")
    result.report.invalidEntries.map(_.key) should contain("cursor.info_bar")
    result.report.invalidEntries.map(_.key) should contain("cursor.info_bar.placement")
  }

  it should "load and write preferred window size" in {
    val configFile = Files.createTempFile("serenity-window-size-config", ".conf")
    Files.writeString(
      configFile,
      """window.preferred.width = 1400
        |window.preferred.height = 900
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.preferredWindowSize shouldBe Some(PreferredWindowSize(1400, 900))
    ConfigManager.configToString(config) should include("window.preferred.width = 1400")
    ConfigManager.configToString(config) should include("window.preferred.height = 900")
  }

  it should "load and write window chrome mode" in {
    val configFile = Files.createTempFile("serenity-window-chrome-config", ".conf")
    Files.writeString(
      configFile,
      """window.chrome = auto
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.windowChromeMode shouldBe WindowChromeMode.Auto
    ConfigManager.configToString(config) should include(
      "# Window chrome: auto uses themed chrome on Linux; native preserves OS snap/window animations; native-themed uses Windows system chrome colours; custom is themed and applies after restart"
    )
    ConfigManager.configToString(config) should include("window.chrome = auto")
  }

  it should "report invalid window config values through the window schema" in {
    val configFile = Files.createTempFile("serenity-window-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """window.chrome = themed-ish
        |window.preferred.width = very-wide
        |""".stripMargin
    )

    val result = ConfigManager.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain("window.chrome")
    result.report.invalidEntries.map(_.key) should contain("window.preferred.width")
  }

  it should "load and write interface density mode" in {
    val configFile = Files.createTempFile("serenity-density-config", ".conf")
    Files.writeString(
      configFile,
      """interface.density = spacious
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.interfaceDensity shouldBe InterfaceDensity.Spacious
    ConfigManager.configToString(config) should include("interface.density = spacious")
  }

  it should "report invalid interface config values through the interface schema" in {
    val configFile = Files.createTempFile("serenity-interface-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """interface.density = roomy
        |ui.element_gap = wide
        |ui.outline_thickness =
        |""".stripMargin
    )

    val result = ConfigManager.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain("interface.density")
    result.report.invalidEntries.map(_.key) should contain("ui.element_gap")
    result.report.invalidEntries.map(_.key) should contain("ui.outline_thickness")
  }

  it should "load and write command runner visible rows" in {
    val configFile = Files.createTempFile("serenity-command-rows-config", ".conf")
    Files.writeString(
      configFile,
      """command_runner.visible_rows = 9
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.commandRunnerVisibleRows shouldBe Some(9)
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

    config.commandRunnerItemGapRows shouldBe 1
    config.commandRunnerCursorGapRows shouldBe Some(3)
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

    config.commandRunnerItemGapRows shouldBe 0.25
    config.commandRunnerCursorGapRows shouldBe Some(0.5)
    config.uiElementGap shouldBe 0.75
    ConfigManager.configToString(config) should include("command_runner.item_gap_rows = 0.25")
    ConfigManager.configToString(config) should include("command_runner.cursor_gap_rows = 0.5")
    ConfigManager.configToString(config) should include("ui.element_gap = 0.75")
  }

  it should "load and write render FPS targets" in {
    val configFile = Files.createTempFile("serenity-render-fps-config", ".conf")
    Files.writeString(
      configFile,
      """render.fps = 120
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.renderFpsTarget shouldBe RenderFpsTarget.Fps120
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

    config.renderFpsTarget shouldBe RenderFpsTarget.Uncapped
    ConfigManager.configToString(config) should include("render.fps = uncapped")
  }

  it should "load and write LSP language server overrides" in {
    val configFile = Files.createTempFile("serenity-lsp-config", ".conf")
    Files.writeString(
      configFile,
      """lsp.scala.enabled = false
        |lsp.python.command = pylsp
        |lsp.python.args = --stdio,--log-file,/tmp/pylsp.log
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.lspUserConfig.servers.value(LanguageId.Scala.id) shouldBe LspServerOverride(
      command = None,
      args = None,
      enabled = Some(false)
    )
    config.lspUserConfig.servers.value(LanguageId.Python.id) shouldBe LspServerOverride(
      command = Some("pylsp"),
      args = Some(List("--stdio", "--log-file", "/tmp/pylsp.log")),
      enabled = None
    )

    val written = ConfigManager.configToString(config)
    written should include("lsp.scala.enabled = false")
    written should include("lsp.python.command = pylsp")
    written should include("lsp.python.args = --stdio,--log-file,/tmp/pylsp.log")
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

    config.textAreaInsets.left shouldBe 0.125 +- 0.0001
    config.textAreaInsets.right shouldBe 0.20 +- 0.0001
    config.textAreaInsets.top shouldBe 0.075 +- 0.0001
    config.textAreaInsets.bottom shouldBe 0.10 +- 0.0001
    ConfigManager.configToString(config) should include("text_area.left.percent = 12.5")
    ConfigManager.configToString(config) should include("text_area.right.percent = 20.0")
    ConfigManager.configToString(config) should include("text_area.top.percent = 7.5")
    ConfigManager.configToString(config) should include("text_area.bottom.percent = 10.0")
  }

  it should "load and write UI element gaps" in {
    val configFile = Files.createTempFile("serenity-ui-gap-config", ".conf")
    Files.writeString(
      configFile,
      """ui.element_gap = 3
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.uiElementGap shouldBe 3
    ConfigManager.configToString(config) should include("ui.element_gap = 3")
  }

  it should "load and write UI corner radius" in {
    val configFile = Files.createTempFile("serenity-ui-corner-radius-config", ".conf")
    Files.writeString(
      configFile,
      """ui.corner_radius = 14
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.uiCornerRadiusPx shouldBe 14
    ConfigManager.configToString(config) should include("ui.corner_radius = 14")
  }

  it should "load and write UI outline thickness" in {
    val configFile = Files.createTempFile("serenity-ui-outline-thickness-config", ".conf")
    Files.writeString(
      configFile,
      """ui.outline_thickness = 4
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.uiOutlineThicknessPx shouldBe 4
    ConfigManager.configToString(config) should include("ui.outline_thickness = 4")
  }

  it should "store interface density and chrome metrics inside the interface sub-config" in {
    val config = AppConfig.default
      .withInterfaceDensity(InterfaceDensity.Spacious)
      .withUiElementGap(3)
      .withUiCornerRadiusPx(14)
      .withUiOutlineThicknessPx(4)

    config.interfaceConfig shouldBe InterfaceConfig(
      density = InterfaceDensity.Spacious,
      elementGap = 3,
      cornerRadiusPx = 14,
      outlineThicknessPx = 4
    )
  }

  it should "load and write word wrap display mode" in {
    val configFile = Files.createTempFile("serenity-word-wrap-config", ".conf")
    Files.writeString(
      configFile,
      """display.word_wrap = false
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.wordWrapEnabled shouldBe false
    ConfigManager.configToString(config) should include("display.word_wrap = false")
  }

  it should "load and write focused text body display mode" in {
    val configFile = Files.createTempFile("serenity-focused-body-config", ".conf")
    Files.writeString(
      configFile,
      """display.focused_text_body = true
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.focusedTextBodyEnabled shouldBe true
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

    config.contextualToolbarDisplayMode shouldBe ToolbarDisplayMode.TextOnly
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

  it should "report invalid surface motion config values through the surface schema" in {
    val configFile = Files.createTempFile("serenity-surface-motion-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """ui.material = neon
        |ui.motion = turbo
        |ui.motion.speed_scale = 5
        |ui.motion.command_runner_reveal = sideways
        |""".stripMargin
    )

    val result = ConfigManager.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain("ui.material")
    result.report.invalidEntries.map(_.key) should contain("ui.motion")
    result.report.invalidEntries.map(_.key) should contain("ui.motion.speed_scale")
    result.report.invalidEntries.map(_.key) should contain("ui.motion.command_runner_reveal")
  }

  it should "round-trip the authoritative motion hierarchy" in {
    val configured = AppConfig.default.withMotionConfiguration(
      MotionConfig(
        MotionAccessibility.Reduced,
        MotionPreset.Expressive,
        Map(
          MotionFamily.CommandSurfaces -> MotionFamilyConfig(
            enabled = true,
            transitionKind = TransitionKind.TypedText,
            animation = AnimationConfig.subtle,
            speedScale = 0.5
          ),
          MotionFamily.PinnedPanels -> MotionFamilyConfig(
            enabled = false,
            transitionKind = TransitionKind.Disabled,
            animation = None,
            speedScale = 0.0
          )
        )
      )
    )
    val configFile = Files.createTempFile("serenity-motion-hierarchy", ".conf")
    val serialized = ConfigManager.configToString(configured)
    Files.writeString(configFile, serialized)

    val loaded = ConfigManager.loadConfig(Some(configFile.toString))

    serialized should not include "ui.motion.speed_scale ="
    serialized should not include "ui.motion.editor_text.speed_scale ="
    serialized should not include "ui.motion.command_runner.speed_scale ="
    serialized should not include "ui.motion.ui.speed_scale ="
    serialized should not include "ui.motion.cursor.speed_scale ="
    serialized should not include "ui.motion.command_runner ="
    serialized should not include "ui.motion.command_runner_reveal ="
    serialized should not include "ui.motion.ui ="
    serialized should not include "ui.motion.editor_text ="
    serialized should not include "ui.motion.panel_open ="
    serialized should not include "ui.motion.panel_close ="
    serialized should include("ui.motion.family.command_surfaces.speed_scale = 0.5")
    serialized should include("ui.motion.family.pinned_panels.close_transition = off")
    loaded.surfaceConfig.motionConfiguration shouldBe configured.surfaceConfig.motionConfiguration.map(configuration =>
      configuration.withFallback(MotionConfig.fromLegacy(configured.surfaceConfig, configuration.baseline))
    )
    loaded.surfaceConfig.effectiveMotionBaseline shouldBe MotionPreset.Expressive
  }

  it should "preserve distinct legacy panel transitions when migrating to the authoritative hierarchy" in {
    val legacy = AppConfig.default
      .withPanelOpenTransitionKind(Some(TransitionKind.DirectionalSweep))
      .withPanelCloseTransitionKind(Some(TransitionKind.Disabled))
    val migrated   = legacy.withMotionConfiguration(MotionConfig.fromLegacy(legacy.surfaceConfig))
    val configFile = Files.createTempFile("serenity-panel-transition-migration", ".conf")
    Files.writeString(configFile, ConfigManager.configToString(migrated))

    val loaded = ConfigManager.loadConfig(Some(configFile.toString))

    loaded.effectivePanelOpenTransitionKind shouldBe TransitionKind.DirectionalSweep
    loaded.effectivePanelCloseTransitionKind shouldBe TransitionKind.Disabled
  }

  it should "round-trip custom family animation timing" in {
    val customAnimation = AnimationConfig(
      steps = 7,
      totalDuration = scala.concurrent.duration.Duration.fromNanos(320_000_000)
    )
    val configured = AppConfig.default.withMotionConfiguration(
      MotionConfig(
        MotionAccessibility.Standard,
        MotionPreset.Smooth,
        Map(
          MotionFamily.CommandSurfaces -> MotionFamilyConfig(
            enabled = true,
            transitionKind = TransitionKind.TypedText,
            animation = Some(customAnimation),
            speedScale = 1.0
          )
        )
      )
    )
    val configFile = Files.createTempFile("serenity-custom-family-animation", ".conf")
    val serialized = ConfigManager.configToString(configured)
    Files.writeString(configFile, serialized)

    val loaded = ConfigManager.loadConfig(Some(configFile.toString))

    serialized should include("ui.motion.family.command_surfaces.animation = custom")
    serialized should include("ui.motion.family.command_surfaces.animation.duration_ms = 320")
    serialized should include("ui.motion.family.command_surfaces.animation.steps = 7")
    loaded.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.CommandSurfaces).animation shouldBe Some(
      customAnimation
    )
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

    config.viewportSizing.width.percent shouldBe 0.8
    config.viewportSizing.width.maxCells shouldBe None
    config.viewportSizing.height.percent shouldBe 1.0
    config.viewportSizing.height.maxCells shouldBe Some(50)
    ConfigManager.configToString(config) should include("viewport.width.percent = 80.0")
    ConfigManager.configToString(config) should include("viewport.width.max = ")
    ConfigManager.configToString(config) should include("viewport.height.percent = 100.0")
    ConfigManager.configToString(config) should include("viewport.height.max = 50")
  }

  it should "load and write material and motion presets" in {
    val configFile = Files.createTempFile("serenity-material-motion-config", ".conf")
    Files.writeString(
      configFile,
      """ui.material = crystal
        |ui.motion = reduced
        |ui.motion.speed_scale = 1.75
        |ui.motion.editor_text.speed_scale = 0.50
        |ui.motion.command_runner.speed_scale = 2.25
        |ui.motion.ui.speed_scale = 1.25
        |ui.motion.cursor.speed_scale = 0.75
        |ui.motion.editor_text = typed
        |ui.motion.panel_open = directional
        |ui.motion.panel_close = off
        |ui.motion.command_runner_reveal = outline
        |ui.motion.command_runner = subtle
        |ui.motion.ui = smooth
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.materialPreset shouldBe MaterialPreset.Crystal
    config.backgroundStyle shouldBe BackgroundStyle.GlassLike
    config.blurRadius shouldBe 0.65f
    config.motionPreset shouldBe MotionPreset.Reduced
    config.elementTransitionSpeedScale shouldBe 1.75
    config.editorTextTransitionSpeedScale shouldBe Some(0.5)
    config.commandRunnerTransitionSpeedScale shouldBe Some(2.25)
    config.uiTransitionSpeedScale shouldBe Some(1.25)
    config.cursorTransitionSpeedScale shouldBe Some(0.75)
    config.editorInsertionTransitionKind shouldBe TransitionKind.TypedText
    config.panelOpenTransitionKind shouldBe Some(TransitionKind.DirectionalSweep)
    config.panelCloseTransitionKind shouldBe Some(TransitionKind.Disabled)
    config.commandRunnerTransitionKind shouldBe Some(TransitionKind.OutlineThenContent)
    config.commandRunnerAnimation shouldBe com.serenity.animation.AnimationConfig.subtle
    config.uiAnimation shouldBe com.serenity.animation.AnimationConfig.smooth
    config.characterAnimation shouldBe None
    val serialized = ConfigManager.configToString(config)
    serialized should include("ui.material = crystal")
    serialized should include("ui.motion = reduced")
    serialized should include("ui.motion.accessibility = standard")
    serialized should include("ui.motion.family.editor_text.transition = typed")
    serialized should include("ui.motion.family.editor_text.speed_scale = 0.5")
    serialized should include("ui.motion.family.command_surfaces.transition = outline")
    serialized should include("ui.motion.family.command_surfaces.animation = subtle")
    serialized should include("ui.motion.family.command_surfaces.speed_scale = 2.25")
    serialized should include("ui.motion.family.pinned_panels.open_transition = directional")
    serialized should include("ui.motion.family.pinned_panels.close_transition = off")
    serialized should include("ui.motion.family.ui_transitions.animation = smooth")
    serialized should include("ui.motion.family.cursor.speed_scale = 0.75")
  }

  it should "load and write the post-processing effect" in {
    val configFile = Files.createTempFile("serenity-post-processing-config", ".conf")
    Files.writeString(configFile, "ui.post_processing = glow\n")

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.postProcessingEffect shouldBe PostProcessingEffect.Glow
    ConfigManager.configToString(config) should include("ui.post_processing = glow")
  }

  it should "round-trip custom character animation duration and steps" in {
    val customAnimation = AnimationConfig(
      steps = 7,
      totalDuration = scala.concurrent.duration.Duration.fromNanos(320_000_000)
    )
    val written = ConfigManager.configToString(AppConfig.default.withCharacterAnimation(customAnimation))

    written should include("character.animation = custom")
    written should include("character.animation.duration_ms = 320")
    written should include("character.animation.steps = 7")

    val configFile = Files.createTempFile("serenity-custom-animation-config", ".conf")
    Files.writeString(configFile, written)

    val loaded = ConfigManager.loadConfig(Some(configFile.toString))

    loaded.motionPreset shouldBe MotionPreset.Custom
    loaded.characterAnimation.value.durationMs shouldBe 320
    loaded.characterAnimation.value.steps shouldBe 7
  }

  it should "load and write the default document mode" in {
    val configFile = Files.createTempFile("serenity-default-document-mode", ".conf")
    Files.writeString(
      configFile,
      """document.default_mode = markdown
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.defaultDocumentMode shouldBe DefaultDocumentMode.Markdown
    ConfigManager.configToString(config) should include("document.default_mode = markdown")
  }

  it should "report invalid document config values through the document schema" in {
    val configFile = Files.createTempFile("serenity-document-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """document.default_mode = wordperfect
        |document.markdown_view = preview-ish
        |""".stripMargin
    )

    val result = ConfigManager.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain("document.default_mode")
    result.report.invalidEntries.map(_.key) should contain("document.markdown_view")
  }

  it should "load and write the markdown view mode" in {
    val configFile = Files.createTempFile("serenity-markdown-view-mode", ".conf")
    Files.writeString(
      configFile,
      """document.markdown_view = inline-lens
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.markdownViewMode shouldBe MarkdownViewMode.InlineLens
    ConfigManager.configToString(config) should include("document.markdown_view = inline-lens")
  }

  it should "load and write spell-check configuration" in {
    val configFile = Files.createTempFile("serenity-spell-config", ".conf")
    Files.writeString(
      configFile,
      """spellcheck.enabled = true
        |spellcheck.languages = en,fr
        |spellcheck.dictionary_paths = C:\Dictionaries\en_US.dic,/usr/share/hunspell/fr.dic
        |spellcheck.words = Serenity,κόσμος,café
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.spellCheck.enabled shouldBe true
    config.spellCheck.languages shouldBe List("en", "fr")
    config.spellCheck.dictionaryPaths shouldBe List("C:\\Dictionaries\\en_US.dic", "/usr/share/hunspell/fr.dic")
    config.spellCheck.additionalWords shouldBe List("serenity", "κόσμος", "café")

    val written = ConfigManager.configToString(config)
    written should include("spellcheck.enabled = true")
    written should include("spellcheck.languages = en,fr")
    written should include("spellcheck.dictionary_paths = C:\\Dictionaries\\en_US.dic,/usr/share/hunspell/fr.dic")
    written should include("spellcheck.words = serenity,κόσμος,café")
  }

  it should "close loaded config files and save using UTF-8" in {
    val configFile = Files.createTempFile("serenity-config-utf8", ".conf")
    Files.writeString(
      configFile,
      """font.text.family = Sérif
        |""".stripMargin,
      StandardCharsets.UTF_8
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))
    config.fontConfig.textFontFamily shouldBe "Sérif"

    Files.delete(configFile)

    ConfigManager.saveConfig(config, configFile) shouldBe true
    Files.readString(configFile, StandardCharsets.UTF_8) should include("font.text.family = Sérif")
  }
