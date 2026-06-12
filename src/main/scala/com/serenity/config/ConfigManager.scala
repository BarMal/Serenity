package com.serenity.config

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.io.Source
import scala.util.Using

import cats.effect.IO
import com.serenity.animation.AnimationConfig

/** Manages loading and saving application configuration */
object ConfigManager:

  val defaultConfigPath: Path =
    Paths.get(System.getProperty("user.home"), ".serenity", "config.conf")

  /** Available animation presets */
  object Presets:
    val none   = AppConfig.default.withoutCharacterAnimation
    val quick  = AppConfig.default.withCharacterAnimation(AnimationConfig.quick.get)
    val smooth = AppConfig.default.withCharacterAnimation(AnimationConfig.smooth.get)
    val subtle = AppConfig.default.withCharacterAnimation(AnimationConfig.subtle.get)

  /** Load configuration from file or return default */
  def loadConfig(configPath: Option[String] = None): AppConfig =
    val path = configPath.map(Paths.get(_)).getOrElse(defaultConfigPath)
    if Files.exists(path) then
      try
        Using.resource(Source.fromFile(path.toFile, StandardCharsets.UTF_8.name())) { source =>
          parseConfig(source.mkString)
        }
      catch
        case _: Exception =>
          System.err.println(s"[CONFIG] Failed to load config from $path, using defaults")
          AppConfig.default
    else AppConfig.default

  /** Load configuration from file on the Cats Effect blocking pool. */
  def loadConfigIO(configPath: Option[String] = None): IO[AppConfig] =
    IO.blocking(loadConfig(configPath))

  /** Parse configuration from string */
  private def parseConfig(content: String): AppConfig =
    // Simple key=value parser for configuration
    val lines = content.split("\n").map(_.trim).filter(_.nonEmpty).filter(!_.startsWith("#"))

    lines.foldLeft(AppConfig.default) { (config, line) =>
      line.split("=", 2) match
        case Array(key, value) =>
          key.trim.toLowerCase match
            case "character.animation" | "character_animation" =>
              value.trim.toLowerCase match
                case "none" | "false" | "off" | "disabled" =>
                  config.withoutCharacterAnimation
                case "quick" =>
                  config.withCharacterAnimation(AnimationConfig.quick.get)
                case "smooth" =>
                  config.withCharacterAnimation(AnimationConfig.smooth.get)
                case "subtle" =>
                  config.withCharacterAnimation(AnimationConfig.subtle.get)
                case _ =>
                  config // Unknown value, keep current config
            case "syntax.highlighting" | "syntax_highlighting" =>
              value.trim.toLowerCase match
                case "true" | "on" | "enabled" =>
                  config.withSyntaxHighlighting(true)
                case "false" | "off" | "disabled" =>
                  config.withSyntaxHighlighting(false)
                case _ =>
                  config // Unknown value, keep current config
            case "font.code.family" | "font_code_family" =>
              config.withFontConfig(config.fontConfig.copy(codeFontFamily = value.trim))
            case "font.text.family" | "font_text_family" =>
              config.withFontConfig(config.fontConfig.copy(textFontFamily = value.trim))
            case "font.ui.family" | "font_ui_family" =>
              config.withFontConfig(config.fontConfig.copy(uiFontFamily = value.trim))
            case "font.code.size" | "font_code_size" =>
              value.trim.toFloatOption
                .map(size => config.withFontConfig(config.fontConfig.copy(fontSize = clampFontSize(size))))
                .getOrElse(config)
            case "font.text.size" | "font.prose.size" | "font_text_size" | "font_prose_size" =>
              value.trim.toFloatOption
                .map(size => config.withFontConfig(config.fontConfig.copy(textFontSize = clampFontSize(size))))
                .getOrElse(config)
            case "font.size" | "font_size" =>
              value.trim.toFloatOption
                .map(size =>
                  config.withFontConfig(
                    config.fontConfig.copy(fontSize = clampFontSize(size), textFontSize = clampFontSize(size))
                  )
                )
                .getOrElse(config)
            case "font.ui.size" | "font_ui_size" =>
              value.trim.toFloatOption
                .map(size => config.withFontConfig(config.fontConfig.copy(uiFontSize = clampFontSize(size))))
                .getOrElse(config)
            case "font.code.ligatures" | "font_code_ligatures" =>
              value.trim.toLowerCase match
                case "true" | "on" | "enabled" =>
                  config.withFontConfig(config.fontConfig.copy(enableLigatures = true))
                case "false" | "off" | "disabled" =>
                  config.withFontConfig(config.fontConfig.copy(enableLigatures = false))
                case _ =>
                  config
            case "font.text.ligatures" | "font.prose.ligatures" | "font_text_ligatures" | "font_prose_ligatures" =>
              value.trim.toLowerCase match
                case "true" | "on" | "enabled" =>
                  config.withFontConfig(config.fontConfig.copy(textLigatures = true))
                case "false" | "off" | "disabled" =>
                  config.withFontConfig(config.fontConfig.copy(textLigatures = false))
                case _ =>
                  config
            case "font.ui.ligatures" | "font_ui_ligatures" =>
              value.trim.toLowerCase match
                case "true" | "on" | "enabled" =>
                  config.withFontConfig(config.fontConfig.copy(uiLigatures = true))
                case "false" | "off" | "disabled" =>
                  config.withFontConfig(config.fontConfig.copy(uiLigatures = false))
                case _ =>
                  config
            case "font.ligatures" | "font_ligatures" =>
              value.trim.toLowerCase match
                case "true" | "on" | "enabled" =>
                  config.withFontConfig(config.fontConfig.copy(enableLigatures = true, textLigatures = true))
                case "false" | "off" | "disabled" =>
                  config.withFontConfig(config.fontConfig.copy(enableLigatures = false, textLigatures = false))
                case _ =>
                  config
            case "cursor.active.color" | "cursor_active_color" =>
              parseColor(value.trim)
                .map(color => config.withCursorColors(config.cursorColors.copy(active = Some(color))))
                .getOrElse(config)
            case "cursor.inactive.color" | "cursor_inactive_color" =>
              parseColor(value.trim)
                .map(color => config.withCursorColors(config.cursorColors.copy(inactive = Some(color))))
                .getOrElse(config)
            case "window.preferred.width" | "window_preferred_width" =>
              value.trim.toIntOption
                .map(width =>
                  config.withPreferredWindowSize(
                    config.preferredWindowSize.getOrElse(PreferredWindowSize(width, 768)).copy(width = width)
                  )
                )
                .getOrElse(config)
            case "window.preferred.height" | "window_preferred_height" =>
              value.trim.toIntOption
                .map(height =>
                  config.withPreferredWindowSize(
                    config.preferredWindowSize.getOrElse(PreferredWindowSize(1024, height)).copy(height = height)
                  )
                )
                .getOrElse(config)
            case hotkeyKey if hotkeyKey.startsWith("hotkey.") =>
              HotkeyAction.values
                .find(action => s"hotkey.${action.configKey}" == hotkeyKey)
                .flatMap(action =>
                  HotkeyTrigger
                    .parse(value.trim)
                    .map(trigger => config.withHotkeyConfig(config.hotkeyConfig.withBinding(action, trigger)))
                )
                .getOrElse(config)
            case keymapKey if keymapKey.startsWith("keymap.editor.") =>
              EditorKeyAction.values
                .find(action => s"keymap.editor.${action.configKey}" == keymapKey)
                .map(action => config.withEditorKeyOverride(action, value.trim))
                .getOrElse(config)
            case keymapKey if keymapKey.startsWith("keymap.command_runner.") =>
              CommandRunnerKeyAction.values
                .find(action => s"keymap.command_runner.${action.configKey}" == keymapKey)
                .map(action => config.withCommandRunnerKeyOverride(action, value.trim))
                .getOrElse(config)
            case keymapKey if keymapKey.startsWith("keymap.modal.") =>
              ModalKeyAction.values
                .find(action => s"keymap.modal.${action.configKey}" == keymapKey)
                .map(action => config.withModalKeyOverride(action, value.trim))
                .getOrElse(config)
            case keymapKey if keymapKey.startsWith("keymap.panel.") =>
              PanelKeyAction.values
                .find(action => s"keymap.panel.${action.configKey}" == keymapKey)
                .map(action => config.withPanelKeyOverride(action, value.trim))
                .getOrElse(config)
            case keymapKey if keymapKey.startsWith("keymap.peek.") =>
              PeekKeyAction.values
                .find(action => s"keymap.peek.${action.configKey}" == keymapKey)
                .map(action => config.withPeekKeyOverride(action, value.trim))
                .getOrElse(config)
            case _ =>
              config // Unknown key, ignore
        case _ =>
          config // Invalid line format, ignore
    }

  /** Generate configuration file content from AppConfig */
  def configToString(config: AppConfig): String =
    val animationSetting = config.characterAnimation match
      case None                                             => "none"
      case Some(anim) if anim == AnimationConfig.quick.get  => "quick"
      case Some(anim) if anim == AnimationConfig.smooth.get => "smooth"
      case Some(anim) if anim == AnimationConfig.subtle.get => "subtle"
      case Some(_)                                          => "custom" // For custom configurations

    s"""# Serenity Editor Configuration
       |# Character animation style: none, quick, smooth, subtle
       |character.animation = $animationSetting
       |
       |# Syntax highlighting: true, false
       |syntax.highlighting = ${config.syntaxHighlightingEnabled}
       |
       |# Font configuration
        |font.code.family = ${config.fontConfig.codeFontFamily}
        |font.text.family = ${config.fontConfig.textFontFamily}
        |font.ui.family = ${config.fontConfig.uiFontFamily}
        |font.code.size = ${config.fontConfig.codeFontSize}
        |font.text.size = ${config.fontConfig.textFontSize}
        |font.ui.size = ${config.fontConfig.uiFontSize}
        |font.code.ligatures = ${config.fontConfig.codeLigatures}
        |font.text.ligatures = ${config.fontConfig.textLigatures}
        |font.ui.ligatures = ${config.fontConfig.uiLigatures}
       |
       |# Cursor colour overrides. Leave empty to use the active theme cursor.
       |cursor.active.color = ${config.cursorColors.active.map(formatColor).getOrElse("")}
       |cursor.inactive.color = ${config.cursorColors.inactive.map(formatColor).getOrElse("")}
       |
       |# Preferred desktop window size. Leave empty to use the default.
       |window.preferred.width = ${config.preferredWindowSize.map(_.width).fold("")(_.toString)}
       |window.preferred.height = ${config.preferredWindowSize.map(_.height).fold("")(_.toString)}
       |
       |# Hotkey overrides
       |hotkey.command_palette = ${config.hotkeyConfig.bindingsFor(HotkeyAction.ToggleCommandRunner).head.render}
       |hotkey.file_search = ${config.hotkeyConfig.bindingsFor(HotkeyAction.FileSearch).head.render}
       |
       |# Focused keymap overrides
       |keymap.editor.page_down = ${config.focusedKeymapConfig.editor.bindingsFor(EditorKeyAction.PageDown).head.render}
       |keymap.command_runner.submit = ${config.focusedKeymapConfig.commandRunner.bindingsFor(CommandRunnerKeyAction.Submit).head.render}
       |keymap.modal.dismiss = ${config.focusedKeymapConfig.modal.bindingsFor(ModalKeyAction.Dismiss).head.render}
       |""".stripMargin

  /** Save configuration to file */
  def saveConfig(config: AppConfig, configPath: String): Boolean =
    saveConfig(config, Paths.get(configPath))

  def saveConfig(config: AppConfig, configPath: Path): Boolean =
    try
      Option(configPath.getParent).foreach(parent => Files.createDirectories(parent))
      Files.write(configPath, configToString(config).getBytes(StandardCharsets.UTF_8))
      true
    catch case _: Exception => false

  /** Get configuration preset by name */
  def getPreset(name: String): Option[AppConfig] =
    name.toLowerCase match
      case "none"   => Some(Presets.none)
      case "quick"  => Some(Presets.quick)
      case "smooth" => Some(Presets.smooth)
      case "subtle" => Some(Presets.subtle)
      case _        => None

  /** List available preset names */
  def availablePresets: List[String] = List("none", "quick", "smooth", "subtle")

  private def clampFontSize(size: Float): Float =
    size.max(8.0f).min(48.0f)

  private def parseColor(value: String): Option[java.awt.Color] =
    val hex = value.stripPrefix("#")
    Option
      .when(hex.length == 6 || hex.length == 8)(hex)
      .filter(_.forall(ch => Character.digit(ch, 16) >= 0))
      .flatMap { normalized =>
        scala.util.Try {
          val red   = Integer.parseInt(normalized.substring(0, 2), 16)
          val green = Integer.parseInt(normalized.substring(2, 4), 16)
          val blue  = Integer.parseInt(normalized.substring(4, 6), 16)
          val alpha = if normalized.length == 8 then Integer.parseInt(normalized.substring(6, 8), 16) else 255
          java.awt.Color(red, green, blue, alpha)
        }.toOption
      }

  private def formatColor(color: java.awt.Color): String =
    val rgb = f"#${color.getRed}%02X${color.getGreen}%02X${color.getBlue}%02X"
    if color.getAlpha == 255 then rgb else f"$rgb${color.getAlpha}%02X"

  /** Create a sample configuration file */
  def createSampleConfig(path: String): Boolean =
    try
      val sampleConfig = """# Serenity Editor Configuration
                          |# This is a sample configuration file
                          |
                          |# Character animation style: none, quick, smooth, subtle
                          |# - none: No character animations (best performance)
                          |# - quick: Fast 3-step fade-in over 150ms
                          |# - smooth: Smooth 5-step fade-in over 200ms  
                          |# - subtle: Minimal 2-step fade-in over 100ms
                          |character.animation = none
                          |
                          |# Syntax highlighting: true, false
                          |syntax.highlighting = false
                          |
                          |# Font configuration
                           |font.code.family = Monaspace Neon (Bundled)
                           |font.text.family = SansSerif
                           |font.ui.family = SansSerif
                           |font.code.size = 12.0
                           |font.text.size = 12.0
                           |font.ui.size = 12.0
                           |font.code.ligatures = true
                           |font.text.ligatures = true
                           |font.ui.ligatures = false
                          |
                          |# Cursor colour overrides. Leave empty to use the active theme cursor.
                          |cursor.active.color =
                          |cursor.inactive.color =
                          |
                          |# Preferred desktop window size. Leave empty to use the default.
                          |window.preferred.width =
                          |window.preferred.height =
                          |
                          |# Hotkey overrides
                          |hotkey.command_palette = ctrl+p
                          |hotkey.file_search = ctrl+shift+f
                          |
                          |# Focused keymap overrides
                          |keymap.editor.page_down = pagedown
                          |keymap.command_runner.submit = enter
                          |keymap.modal.dismiss = escape
                          |""".stripMargin

      Files.write(Paths.get(path), sampleConfig.getBytes(StandardCharsets.UTF_8))
      true
    catch case _: Exception => false
