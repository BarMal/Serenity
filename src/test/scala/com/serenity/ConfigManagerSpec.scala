package com.serenity

import java.awt.Color
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.animation.TransitionKind
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
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.textAreaInsets.left shouldBe 0.125 +- 0.0001
    config.textAreaInsets.right shouldBe 0.20 +- 0.0001
    ConfigManager.configToString(config) should include("text_area.left.percent = 12.5")
    ConfigManager.configToString(config) should include("text_area.right.percent = 20.0")
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

  it should "load and write material and motion presets" in {
    val configFile = Files.createTempFile("serenity-material-motion-config", ".conf")
    Files.writeString(
      configFile,
      """ui.material = crystal
        |ui.motion = reduced
        |ui.motion.speed_scale = 1.75
        |ui.motion.editor_text = typed
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.materialPreset shouldBe MaterialPreset.Crystal
    config.backgroundStyle shouldBe BackgroundStyle.GlassLike
    config.blurRadius shouldBe 0.65f
    config.motionPreset shouldBe MotionPreset.Reduced
    config.elementTransitionSpeedScale shouldBe 1.75
    config.editorInsertionTransitionKind shouldBe TransitionKind.TypedText
    config.characterAnimation shouldBe None
    ConfigManager.configToString(config) should include("ui.material = crystal")
    ConfigManager.configToString(config) should include("ui.motion = reduced")
    ConfigManager.configToString(config) should include("ui.motion.speed_scale = 1.75")
    ConfigManager.configToString(config) should include("ui.motion.editor_text = typed")
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

  it should "load and write spell-check configuration" in {
    val configFile = Files.createTempFile("serenity-spell-config", ".conf")
    Files.writeString(
      configFile,
      """spellcheck.enabled = true
        |spellcheck.languages = en,fr
        |spellcheck.words = Serenity,κόσμος,café
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.spellCheck.enabled shouldBe true
    config.spellCheck.languages shouldBe List("en", "fr")
    config.spellCheck.additionalWords shouldBe List("serenity", "κόσμος", "café")

    val written = ConfigManager.configToString(config)
    written should include("spellcheck.enabled = true")
    written should include("spellcheck.languages = en,fr")
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
