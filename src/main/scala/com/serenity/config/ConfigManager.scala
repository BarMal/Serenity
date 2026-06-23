package com.serenity.config

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.io.Source
import scala.util.Using

import cats.effect.IO
import com.serenity.animation.{AnimationConfig, TransitionKind}
import com.serenity.lsp.config.{LanguageId, LspServerOverride, LspUserConfig}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.TextScaleMode

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
    loadConfigResult(configPath).config

  /** Load configuration from file with migration/deprecation report or return defaults. */
  def loadConfigResult(configPath: Option[String] = None): ConfigLoadResult =
    val path = configPath.map(Paths.get(_)).getOrElse(defaultConfigPath)
    if Files.exists(path) then
      try
        Using.resource(Source.fromFile(path.toFile, StandardCharsets.UTF_8.name())) { source =>
          parseConfigResult(source.mkString)
        }
      catch
        case _: Exception =>
          System.err.println(s"[CONFIG] Failed to load config from $path, using defaults")
          ConfigLoadResult(AppConfig.default, ConfigMigrationReport.empty)
    else ConfigLoadResult(AppConfig.default, ConfigMigrationReport.empty)

  /** Load configuration from file on the Cats Effect blocking pool. */
  def loadConfigIO(configPath: Option[String] = None): IO[AppConfig] =
    IO.blocking(loadConfig(configPath))

  /** Load configuration with migration/deprecation report on the Cats Effect blocking pool. */
  def loadConfigResultIO(configPath: Option[String] = None): IO[ConfigLoadResult] =
    IO.blocking(loadConfigResult(configPath))

  private def parseConfigResult(content: String): ConfigLoadResult =
    ConfigLoadResult(parseConfig(content), inspectConfig(content))

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
            case "font.scale.mode" | "font_scale_mode" =>
              parseTextScaleMode(value.trim)
                .map(mode =>
                  config.withFontConfig(config.fontConfig.copy(textScaleMode = mode).resolveAutoTextScale(1.0))
                )
                .getOrElse(config)
            case "font.text_scale" | "font.text.scale" | "font_text_scale" =>
              parseTextScaleMultiplier(value.trim)
                .map(scale =>
                  config.withFontConfig(
                    config.fontConfig.copy(textScaleMultiplier = scale, textScaleMode = TextScaleMode.Manual)
                  )
                )
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
            case "interface.density" | "interface_density" =>
              value.trim.toLowerCase match
                case "compact" =>
                  config.withInterfaceDensity(InterfaceDensity.Compact)
                case "comfortable" =>
                  config.withInterfaceDensity(InterfaceDensity.Comfortable)
                case "spacious" =>
                  config.withInterfaceDensity(InterfaceDensity.Spacious)
                case _ =>
                  config
            case "ui.element_gap" | "ui.element.gap" | "ui_element_gap" =>
              parseUiElementGap(value.trim).map(config.withUiElementGap).getOrElse(config)
            case "ui.corner_radius" | "ui.corner.radius" | "ui_corner_radius" =>
              parseUiCornerRadiusPx(value.trim).map(config.withUiCornerRadiusPx).getOrElse(config)
            case "display.word_wrap" | "display.word.wrap" | "display_word_wrap" =>
              parseBoolean(value.trim).map(config.withWordWrap).getOrElse(config)
            case "cursor.info_bar" | "cursor.info.bar" | "cursor_info_bar" =>
              value.trim.toLowerCase match
                case "off" | "false" | "disabled" =>
                  config.withCursorInfoBarMode(CursorInfoBarMode.Off)
                case "position" | "minimal" =>
                  config.withCursorInfoBarMode(CursorInfoBarMode.Position)
                case "detailed" | "full" =>
                  config.withCursorInfoBarMode(CursorInfoBarMode.Detailed)
                case _ =>
                  config
            case "cursor.info_bar.placement" | "cursor.info.bar.placement" | "cursor_info_bar_placement" =>
              value.trim.toLowerCase match
                case "floating" | "float" =>
                  config.withCursorInfoBarPlacement(CursorInfoBarPlacement.Floating)
                case "pinned-bottom" | "bottom" | "pinned" =>
                  config.withCursorInfoBarPlacement(CursorInfoBarPlacement.PinnedBottom)
                case _ =>
                  config
            case "ui.material" | "ui_material" | "material.preset" | "material_preset" =>
              parseMaterialPreset(value.trim).map(config.withMaterialPreset).getOrElse(config)
            case "ui.motion" | "ui_motion" | "motion.preset" | "motion_preset" =>
              parseMotionPreset(value.trim).map(config.withMotionPreset).getOrElse(config)
            case "ui.motion.speed_scale" | "motion.speed_scale" | "ui_motion_speed_scale" | "motion_speed_scale" =>
              parseElementTransitionSpeedScale(value.trim)
                .map(config.withElementTransitionSpeedScale)
                .getOrElse(config)
            case "ui.motion.editor_text" | "ui.motion.editor.text" | "ui_motion_editor_text" =>
              parseEditorInsertionTransitionKind(value.trim)
                .map(config.withEditorInsertionTransitionKind)
                .getOrElse(config)
            case "document.default_mode" | "document.default.mode" | "document_default_mode" =>
              parseDefaultDocumentMode(value.trim).map(config.withDefaultDocumentMode).getOrElse(config)
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
            case lspKey if lspKey.startsWith("lsp.") =>
              parseLspConfigEntry(config, lspKey, value.trim)
            case "text_area.left.percent" | "text.area.left.percent" | "text_area_left_percent" =>
              value.trim.toDoubleOption
                .map(percent => config.withTextAreaLeftInset(percent / 100.0))
                .getOrElse(config)
            case "text_area.right.percent" | "text.area.right.percent" | "text_area_right_percent" =>
              value.trim.toDoubleOption
                .map(percent => config.withTextAreaRightInset(percent / 100.0))
                .getOrElse(config)
            case "spellcheck.enabled" | "spellcheck_enabled" =>
              parseBoolean(value.trim)
                .map(enabled => config.withSpellCheck(config.spellCheck.copy(enabled = enabled)))
                .getOrElse(config)
            case "spellcheck.languages" | "spellcheck_languages" =>
              config.withSpellCheck(config.spellCheck.copy(languages = parseCommaList(value.trim)))
            case "spellcheck.words" | "spellcheck_words" =>
              config.withSpellCheck(config.spellCheck.copy(additionalWords = parseCommaList(value.trim)))
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
            case "config.version" =>
              config
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
    val lspSettings = lspConfigToString(config.lspUserConfig)
    def editorBinding(action: EditorKeyAction): String =
      config.focusedKeymapConfig.editor
        .bindingsFor(action)
        .headOption
        .orElse(EditorKeymapConfig.defaultBindings.get(action).flatMap(_.headOption))
        .fold("")(_.render)

    s"""# Serenity Editor Configuration
       |config.version = ${ConfigVersion.Current.value}
       |
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
        |font.scale.mode = ${config.fontConfig.textScaleMode.configKey}
        |font.text_scale = ${config.fontConfig.textScaleMultiplier}
        |font.code.ligatures = ${config.fontConfig.codeLigatures}
        |font.text.ligatures = ${config.fontConfig.textLigatures}
        |font.ui.ligatures = ${config.fontConfig.uiLigatures}
       |
       |# Cursor colour overrides. Leave empty to use the active theme cursor.
       |cursor.active.color = ${config.cursorColors.active.map(formatColor).getOrElse("")}
       |cursor.inactive.color = ${config.cursorColors.inactive.map(formatColor).getOrElse("")}
       |cursor.info_bar = ${config.cursorInfoBarMode.configKey}
       |cursor.info_bar.placement = ${config.cursorInfoBarPlacement.configKey}
       |
       |# Interface density: compact, comfortable, spacious
       |interface.density = ${config.interfaceDensity.configKey}
       |ui.element_gap = ${config.uiElementGap}
       |ui.corner_radius = ${config.uiCornerRadiusPx}
       |display.word_wrap = ${config.wordWrapEnabled}
       |
       |# UI material and motion presets: solid, clear, frosted, crystal, custom / reduced, subtle, smooth, expressive, custom
       |ui.material = ${config.materialPreset.configKey}
       |ui.motion = ${config.motionPreset.configKey}
       |ui.motion.speed_scale = ${config.elementTransitionSpeedScale}
       |ui.motion.editor_text = ${editorInsertionTransitionConfigKey(config.editorInsertionTransitionKind)}
       |
       |# Default mode for new buffers: plain-text, markdown, rich-text
       |document.default_mode = ${config.defaultDocumentMode.configKey}
       |
       |# Preferred desktop window size. Leave empty to use the default.
       |window.preferred.width = ${config.preferredWindowSize.map(_.width).fold("")(_.toString)}
       |window.preferred.height = ${config.preferredWindowSize.map(_.height).fold("")(_.toString)}
       |
       |# Text area horizontal insets as percentages of the central workspace.
       |text_area.left.percent = ${config.textAreaInsets.leftPercent}
       |text_area.right.percent = ${config.textAreaInsets.rightPercent}
       |
       |# LSP server overrides
       |$lspSettings
       |
       |# Spell-checking for prose buffers
       |spellcheck.enabled = ${config.spellCheck.enabled}
       |spellcheck.languages = ${config.spellCheck.normalized.languages.mkString(",")}
       |spellcheck.words = ${config.spellCheck.normalized.additionalWords.mkString(",")}
       |
       |# Hotkey overrides
       |hotkey.command_palette = ${config.hotkeyConfig.bindingsFor(HotkeyAction.ToggleCommandRunner).head.render}
       |hotkey.file_search = ${config.hotkeyConfig.bindingsFor(HotkeyAction.FileSearch).head.render}
       |
       |# Focused keymap overrides
       |keymap.editor.page_down = ${editorBinding(EditorKeyAction.PageDown)}
       |keymap.editor.extend_selection_left = ${editorBinding(EditorKeyAction.ExtendSelectionLeft)}
       |keymap.editor.extend_selection_right = ${editorBinding(EditorKeyAction.ExtendSelectionRight)}
       |keymap.editor.extend_selection_up = ${editorBinding(EditorKeyAction.ExtendSelectionUp)}
       |keymap.editor.extend_selection_down = ${editorBinding(EditorKeyAction.ExtendSelectionDown)}
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

  private def inspectConfig(content: String): ConfigMigrationReport =
    val entries = configEntries(content)
    val deprecatedEntries = entries
      .flatMap((key, _) => deprecatedReplacement(key).map(replacement => DeprecatedConfigEntry(key, replacement)))
      .distinctBy(_.key)
    val unknownKeys = entries
      .map(_._1)
      .filterNot(isKnownConfigKey)
      .distinct
    val invalidEntries = entries.flatMap { case (key, value) => invalidEntry(key, value) }

    ConfigMigrationReport(
      version = ConfigVersion.Current,
      deprecatedEntries = deprecatedEntries,
      unknownKeys = unknownKeys,
      invalidEntries = invalidEntries
    )

  private def configEntries(content: String): List[(String, String)] =
    content
      .split("\n")
      .toList
      .map(_.trim)
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .flatMap { line =>
        line.split("=", 2).toList match
          case key :: value :: Nil => Some(key.trim.toLowerCase -> value.trim)
          case _                   => None
      }

  private def deprecatedReplacement(key: String): Option[String] =
    DeprecatedConfigKeys.get(key)

  private def isKnownConfigKey(key: String): Boolean =
    CurrentConfigKeys.contains(key) ||
      DeprecatedConfigKeys.contains(key) ||
      key.startsWith("lsp.") ||
      key.startsWith("hotkey.") ||
      key.startsWith("keymap.editor.") ||
      key.startsWith("keymap.command_runner.") ||
      key.startsWith("keymap.modal.") ||
      key.startsWith("keymap.panel.") ||
      key.startsWith("keymap.peek.")

  private def invalidEntry(key: String, value: String): Option[InvalidConfigEntry] =
    val normalizedValue = value.trim.toLowerCase
    val invalid =
      key match
        case "character.animation" | "character_animation" =>
          !Set("none", "false", "off", "disabled", "quick", "smooth", "subtle").contains(normalizedValue)
        case "syntax.highlighting" | "syntax_highlighting" | "font.code.ligatures" | "font_code_ligatures" |
            "font.text.ligatures" | "font.prose.ligatures" | "font_text_ligatures" | "font_prose_ligatures" |
            "font.ui.ligatures" | "font_ui_ligatures" | "font.ligatures" | "font_ligatures" | "spellcheck.enabled" |
            "spellcheck_enabled" | "display.word_wrap" | "display.word.wrap" | "display_word_wrap" =>
          parseBoolean(value).isEmpty
        case "font.code.size" | "font_code_size" | "font.text.size" | "font.prose.size" | "font_text_size" |
            "font_prose_size" | "font.size" | "font_size" | "font.ui.size" | "font_ui_size" =>
          value.trim.toFloatOption.isEmpty
        case "font.scale.mode" | "font_scale_mode" =>
          parseTextScaleMode(value).isEmpty
        case "font.text_scale" | "font.text.scale" | "font_text_scale" =>
          parseTextScaleMultiplier(value).isEmpty
        case "cursor.active.color" | "cursor_active_color" | "cursor.inactive.color" | "cursor_inactive_color" =>
          value.nonEmpty && parseColor(value).isEmpty
        case "interface.density" | "interface_density" =>
          !Set("compact", "comfortable", "spacious").contains(normalizedValue)
        case "cursor.info_bar" | "cursor.info.bar" | "cursor_info_bar" =>
          !Set("off", "false", "disabled", "position", "minimal", "detailed", "full").contains(normalizedValue)
        case "cursor.info_bar.placement" | "cursor.info.bar.placement" | "cursor_info_bar_placement" =>
          !Set("floating", "float", "pinned-bottom", "bottom", "pinned").contains(normalizedValue)
        case "ui.material" | "ui_material" | "material.preset" | "material_preset" =>
          parseMaterialPreset(value).isEmpty
        case "ui.motion" | "ui_motion" | "motion.preset" | "motion_preset" =>
          parseMotionPreset(value).isEmpty
        case "ui.motion.speed_scale" | "motion.speed_scale" | "ui_motion_speed_scale" | "motion_speed_scale" =>
          parseElementTransitionSpeedScale(value).isEmpty
        case "ui.motion.editor_text" | "ui.motion.editor.text" | "ui_motion_editor_text" =>
          parseEditorInsertionTransitionKind(value).isEmpty
        case "ui.element_gap" | "ui.element.gap" | "ui_element_gap" =>
          parseUiElementGap(value).isEmpty
        case "ui.corner_radius" | "ui.corner.radius" | "ui_corner_radius" =>
          parseUiCornerRadiusPx(value).isEmpty
        case "window.preferred.width" | "window_preferred_width" | "window.preferred.height" |
            "window_preferred_height" =>
          value.trim.nonEmpty && value.trim.toIntOption.isEmpty
        case "text_area.left.percent" | "text.area.left.percent" | "text_area_left_percent" |
            "text_area.right.percent" | "text.area.right.percent" | "text_area_right_percent" =>
          value.trim.toDoubleOption.isEmpty
        case _ =>
          false

    Option.when(invalid)(InvalidConfigEntry(key, value, "Invalid value for supported config key"))

  private val CurrentConfigKeys: Set[String] =
    Set(
      "config.version",
      "character.animation",
      "syntax.highlighting",
      "font.code.family",
      "font.text.family",
      "font.ui.family",
      "font.code.size",
      "font.text.size",
      "font.prose.size",
      "font.ui.size",
      "font.scale.mode",
      "font.text_scale",
      "font.text.scale",
      "font.code.ligatures",
      "font.text.ligatures",
      "font.prose.ligatures",
      "font.ui.ligatures",
      "cursor.active.color",
      "cursor.inactive.color",
      "cursor.info_bar",
      "cursor.info.bar",
      "cursor.info_bar.placement",
      "cursor.info.bar.placement",
      "interface.density",
      "ui.material",
      "material.preset",
      "ui.motion",
      "motion.preset",
      "ui.motion.speed_scale",
      "motion.speed_scale",
      "ui.motion.editor_text",
      "ui.motion.editor.text",
      "ui.element_gap",
      "ui.element.gap",
      "ui.corner_radius",
      "ui.corner.radius",
      "display.word_wrap",
      "display.word.wrap",
      "document.default_mode",
      "document.default.mode",
      "window.preferred.width",
      "window.preferred.height",
      "text_area.left.percent",
      "text.area.left.percent",
      "text_area.right.percent",
      "text.area.right.percent",
      "spellcheck.enabled",
      "spellcheck.languages",
      "spellcheck.words"
    )

  private val DeprecatedConfigKeys: Map[String, String] =
    Map(
      "character_animation"       -> "character.animation",
      "syntax_highlighting"       -> "syntax.highlighting",
      "font_code_family"          -> "font.code.family",
      "font_text_family"          -> "font.text.family",
      "font_ui_family"            -> "font.ui.family",
      "font_code_size"            -> "font.code.size",
      "font_text_size"            -> "font.text.size",
      "font_prose_size"           -> "font.text.size",
      "font_size"                 -> "font.code.size and font.text.size",
      "font_ui_size"              -> "font.ui.size",
      "font_scale_mode"           -> "font.scale.mode",
      "font_text_scale"           -> "font.text_scale",
      "font_code_ligatures"       -> "font.code.ligatures",
      "font_text_ligatures"       -> "font.text.ligatures",
      "font_prose_ligatures"      -> "font.text.ligatures",
      "font_ligatures"            -> "font.code.ligatures and font.text.ligatures",
      "font_ui_ligatures"         -> "font.ui.ligatures",
      "cursor_active_color"       -> "cursor.active.color",
      "cursor_inactive_color"     -> "cursor.inactive.color",
      "cursor_info_bar"           -> "cursor.info_bar",
      "cursor_info_bar_placement" -> "cursor.info_bar.placement",
      "interface_density"         -> "interface.density",
      "ui_material"               -> "ui.material",
      "material_preset"           -> "material.preset",
      "ui_motion"                 -> "ui.motion",
      "motion_preset"             -> "motion.preset",
      "ui_motion_speed_scale"     -> "ui.motion.speed_scale",
      "ui_motion_editor_text"     -> "ui.motion.editor_text",
      "motion_speed_scale"        -> "motion.speed_scale",
      "ui_element_gap"            -> "ui.element_gap",
      "ui_corner_radius"          -> "ui.corner_radius",
      "display_word_wrap"         -> "display.word_wrap",
      "document_default_mode"     -> "document.default_mode",
      "window_preferred_width"    -> "window.preferred.width",
      "window_preferred_height"   -> "window.preferred.height",
      "text_area_left_percent"    -> "text_area.left.percent",
      "text_area_right_percent"   -> "text_area.right.percent",
      "spellcheck_enabled"        -> "spellcheck.enabled",
      "spellcheck_languages"      -> "spellcheck.languages",
      "spellcheck_words"          -> "spellcheck.words"
    )

  private def clampFontSize(size: Float): Float =
    size.max(8.0f).min(48.0f)

  private def parseTextScaleMode(value: String): Option[TextScaleMode] =
    value.toLowerCase match
      case "auto"                      => Some(TextScaleMode.Auto)
      case "manual" | "custom"         => Some(TextScaleMode.Manual)
      case "off" | "none" | "disabled" => Some(TextScaleMode.Off)
      case _                           => None

  private def parseTextScaleMultiplier(value: String): Option[Double] =
    value.toDoubleOption
      .filter(scale => scale >= FontLoader.FontConfig.MinTextScale && scale <= FontLoader.FontConfig.MaxTextScale)

  private def parseMaterialPreset(value: String): Option[MaterialPreset] =
    value.toLowerCase match
      case "solid" | "opaque"      => Some(MaterialPreset.Solid)
      case "clear" | "transparent" => Some(MaterialPreset.Clear)
      case "frosted" | "soft"      => Some(MaterialPreset.Frosted)
      case "crystal" | "glass"     => Some(MaterialPreset.Crystal)
      case "custom"                => Some(MaterialPreset.Custom)
      case _                       => None

  private def parseMotionPreset(value: String): Option[MotionPreset] =
    value.toLowerCase match
      case "reduced" | "none" | "off" | "disabled" => Some(MotionPreset.Reduced)
      case "subtle"                                => Some(MotionPreset.Subtle)
      case "smooth"                                => Some(MotionPreset.Smooth)
      case "expressive" | "full" | "quick"         => Some(MotionPreset.Expressive)
      case "custom"                                => Some(MotionPreset.Custom)
      case _                                       => None

  private def parseEditorInsertionTransitionKind(value: String): Option[TransitionKind] =
    value.toLowerCase match
      case "fade"                                             => Some(TransitionKind.Fade)
      case "typed" | "typed-text" | "type"                    => Some(TransitionKind.TypedText)
      case "directional" | "directional-sweep" | "sweep"      => Some(TransitionKind.DirectionalSweep)
      case "tandem" | "line-and-character" | "line-character" => Some(TransitionKind.LineAndCharacterTandem)
      case "off" | "none" | "disabled"                        => Some(TransitionKind.Disabled)
      case _                                                  => None

  private def editorInsertionTransitionConfigKey(kind: TransitionKind): String =
    kind match
      case TransitionKind.Fade                   => "fade"
      case TransitionKind.TypedText              => "typed"
      case TransitionKind.DirectionalSweep       => "directional"
      case TransitionKind.LineAndCharacterTandem => "tandem"
      case TransitionKind.Disabled               => "off"
      case TransitionKind.OutlineThenContent     => "fade"

  private def parseDefaultDocumentMode(value: String): Option[DefaultDocumentMode] =
    value.toLowerCase match
      case "plain-text" | "plaintext" | "plain" | "text" => Some(DefaultDocumentMode.PlainText)
      case "markdown" | "md"                             => Some(DefaultDocumentMode.Markdown)
      case "rich-text" | "richtext" | "rich" | "rtf"     => Some(DefaultDocumentMode.RichText)
      case _                                             => None

  private def parseElementTransitionSpeedScale(value: String): Option[Double] =
    value.toDoubleOption
      .filter(scale =>
        scale >= AppConfig.MinElementTransitionSpeedScale &&
          scale <= AppConfig.MaxElementTransitionSpeedScale
      )

  private def parseUiElementGap(value: String): Option[Int] =
    value.toIntOption.filter(gap => gap >= AppConfig.MinUiElementGap && gap <= AppConfig.MaxUiElementGap)

  private def parseUiCornerRadiusPx(value: String): Option[Int] =
    value.toIntOption.filter(radius =>
      radius >= AppConfig.MinUiCornerRadiusPx && radius <= AppConfig.MaxUiCornerRadiusPx
    )

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

  private def parseLspConfigEntry(config: AppConfig, key: String, value: String): AppConfig =
    key.split("\\.", 3).toList match
      case "lsp" :: languageKey :: field :: Nil =>
        LanguageId.fromString(languageKey).fold(config) { languageId =>
          field match
            case "enabled" =>
              parseBoolean(value)
                .map(enabled => updateLspOverride(config, languageId)(_.copy(enabled = Some(enabled))))
                .getOrElse(config)
            case "command" =>
              updateLspOverride(config, languageId)(_.copy(command = Option(value).filter(_.nonEmpty)))
            case "args" =>
              val args = value
                .split(",")
                .toList
                .map(_.trim)
                .filter(_.nonEmpty)
              updateLspOverride(config, languageId)(_.copy(args = Some(args)))
            case _ =>
              config
        }
      case _ =>
        config

  private def updateLspOverride(
    config: AppConfig,
    languageId: LanguageId
  )(update: LspServerOverride => LspServerOverride): AppConfig =
    val servers  = config.lspUserConfig.servers.getOrElse(Map.empty)
    val existing = servers.getOrElse(languageId.id, LspServerOverride(command = None, args = None))
    config.withLspUserConfig(
      LspUserConfig(
        servers = Some(servers + (languageId.id -> update(existing)))
      )
    )

  private def parseBoolean(value: String): Option[Boolean] =
    value.toLowerCase match
      case "true" | "on" | "enabled"    => Some(true)
      case "false" | "off" | "disabled" => Some(false)
      case _                            => None

  private def lspConfigToString(config: LspUserConfig): String =
    config.servers
      .getOrElse(Map.empty)
      .toList
      .sortBy(_._1)
      .flatMap {
        case (languageId, override_) =>
          List(
            override_.enabled.map(enabled => s"lsp.$languageId.enabled = $enabled"),
            override_.command.map(command => s"lsp.$languageId.command = $command"),
            override_.args.map(args => s"lsp.$languageId.args = ${args.mkString(",")}")
          ).flatten
      }
      .mkString("\n")

  private def parseCommaList(value: String): List[String] =
    value
      .split(",")
      .toList
      .map(_.trim.toLowerCase)
      .filter(_.nonEmpty)
      .distinct

  /** Create a sample configuration file */
  def createSampleConfig(path: String): Boolean =
    try
      val sampleConfig = """# Serenity Editor Configuration
                          |# This is a sample configuration file
                          |config.version = 1
                          |
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
                           |font.scale.mode = auto
                           |font.text_scale = 1.0
                           |font.code.ligatures = true
                           |font.text.ligatures = true
                           |font.ui.ligatures = false
                          |
                          |# Cursor colour overrides. Leave empty to use the active theme cursor.
                          |cursor.active.color =
                          |cursor.inactive.color =
                          |cursor.info_bar = off
                          |
                          |# Interface density: compact, comfortable, spacious
                          |interface.density = comfortable
                          |
                          |# UI material and motion presets: solid, clear, frosted, crystal, custom / reduced, subtle, smooth, expressive, custom
                          |ui.material = frosted
                          |ui.motion = smooth
                          |ui.motion.speed_scale = 1.0
                          |ui.motion.editor_text = fade
                          |
                          |# Preferred desktop window size. Leave empty to use the default.
                          |window.preferred.width =
                          |window.preferred.height =
                          |
                          |# Spell-checking for prose buffers
                          |spellcheck.enabled = false
                          |spellcheck.languages = en
                          |spellcheck.words =
                          |
                          |# Hotkey overrides
                          |hotkey.command_palette = ctrl+p
                          |hotkey.file_search = ctrl+shift+f
                          |
                          |# Focused keymap overrides
                          |keymap.editor.page_down = pagedown
                          |keymap.editor.extend_selection_left = shift+left
                          |keymap.editor.extend_selection_right = shift+right
                          |keymap.editor.extend_selection_up = shift+up
                          |keymap.editor.extend_selection_down = shift+down
                          |keymap.command_runner.submit = enter
                          |keymap.modal.dismiss = escape
                          |""".stripMargin

      Files.write(Paths.get(path), sampleConfig.getBytes(StandardCharsets.UTF_8))
      true
    catch case _: Exception => false
