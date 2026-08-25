package com.serenity.config

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.Locale

import scala.jdk.CollectionConverters.*

import cats.effect.IO
import com.serenity.animation.{AnimationConfig, TransitionKind, TransitionScope, WindowSitterAction}
import com.serenity.io.AtomicFileWriter
import com.serenity.lsp.config.{LanguageId, LspServerOverride, LspUserConfig}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.TextScaleMode
import com.typesafe.config.{Config, ConfigException, ConfigFactory, ConfigParseOptions, ConfigValueType}

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
      try parseConfigResult(path)
      catch
        case _: Exception =>
          System.err.println(s"[CONFIG] Failed to load config from $path, using defaults")
          ConfigLoadResult(AppConfig.default, ConfigMigrationReport.empty)
    else ConfigLoadResult(AppConfig.default, ConfigMigrationReport.empty)

  /** Load configuration from file on the Cats Effect blocking pool. */
  def loadConfigIO(configPath: Option[String] = None): IO[AppConfig] =
    IO.blocking(loadConfig(configPath))

  /** Load configuration with migration/deprecation report on the Cats Effect blocking pool. */
  def loadConfigResultIO(configPath: Option[String] = None): IO[Either[ConfigError, ConfigLoadResult]] =
    IO.blocking {
      val path = configPath.map(Paths.get(_)).getOrElse(defaultConfigPath)
      if !Files.exists(path) then Right(ConfigLoadResult(AppConfig.default, ConfigMigrationReport.empty))
      else
        try
          val result = parseConfigResult(path)
          if result.report.invalidEntries.nonEmpty then
            Left(
              ConfigError(
                "load",
                path,
                s"Invalid configuration entries: ${result.report.invalidEntries.map(_.key).mkString(", ")}"
              )
            )
          else Right(result)
        catch
          case error: Exception =>
            Left(ConfigError("load", path, s"Failed to load configuration: ${error.getMessage}", Some(error)))
    }

  private def parseConfigResult(path: Path): ConfigLoadResult =
    parseConfigResult(parseHoconFile(path))

  private def parseConfigResult(source: Config): ConfigLoadResult =
    ConfigLoadResult(parseConfig(source), inspectConfig(source))

  private def parseConfig(source: Config): AppConfig =
    val entries = hoconEntries(source)

    val parsed = entries.foldLeft(AppConfig.default) { (config, entry) =>
      val HoconEntry(key, value, _) = entry
      key match
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
            case "custom" =>
              config.withCharacterAnimation(config.characterAnimation.getOrElse(AnimationConfig.smooth.get))
            case _ =>
              config // Unknown value, keep current config
        case "character.animation.duration_ms" | "character.animation.duration.ms" |
            "character_animation_duration_ms" =>
          value.trim.toIntOption
            .filter(_ > 0)
            .map(ms =>
              config.withCharacterAnimation(
                config.characterAnimation
                  .getOrElse(AnimationConfig.smooth.get)
                  .copy(totalDuration = scala.concurrent.duration.Duration.fromNanos(ms * 1_000_000L))
              )
            )
            .getOrElse(config)
        case "character.animation.steps" | "character_animation_steps" =>
          value.trim.toIntOption
            .filter(_ > 0)
            .map(steps =>
              config.withCharacterAnimation(
                config.characterAnimation
                  .getOrElse(AnimationConfig.smooth.get)
                  .copy(steps = steps)
              )
            )
            .getOrElse(config)
        case key if LanguageToolsConfig.Schema.handles(key) =>
          LanguageToolsConfig.Schema.parse(config, key, value).getOrElse(config)
        case "font.code.family" | "font_code_family" =>
          config.withFontConfig(config.fontConfig.copy(codeFontFamily = value.trim))
        case "font.text.family" | "font_text_family" =>
          config.withFontConfig(config.fontConfig.copy(textFontFamily = value.trim))
        case "font.ui.family" | "font_ui_family" =>
          val family = if value.trim == "${font.text.family}" then config.fontConfig.textFontFamily else value.trim
          config.withFontConfig(config.fontConfig.copy(uiFontFamily = family))
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
            .map(mode => config.withFontConfig(config.fontConfig.copy(textScaleMode = mode).resolveAutoTextScale(1.0)))
            .getOrElse(config)
        case "font.text_scale" | "font.text.scale" | "font_text_scale" =>
          parseTextScaleMultiplier(value.trim)
            .map(scale =>
              config.withFontConfig(
                config.fontConfig.copy(
                  textScaleMultiplier = scale,
                  textScaleMode = if scale == 1.0 then config.fontConfig.textScaleMode else TextScaleMode.Manual
                )
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
        case key if CursorConfig.Schema.handles(key) =>
          CursorConfig.Schema.parse(config, key, value).getOrElse(config)
        case "interface.density" | "interface_density" =>
          InterfaceDensity.fromConfigKey(value).map(config.withInterfaceDensity).getOrElse(config)
        case "ui.element_gap" | "ui.element.gap" | "ui_element_gap" =>
          parseUiElementGap(value.trim).map(config.withUiElementGap).getOrElse(config)
        case "ui.corner_radius" | "ui.corner.radius" | "ui_corner_radius" =>
          parseUiCornerRadiusPx(value.trim).map(config.withUiCornerRadiusPx).getOrElse(config)
        case "ui.outline_thickness" | "ui.outline.thickness" | "ui_outline_thickness" =>
          parseUiOutlineThicknessPx(value.trim).map(config.withUiOutlineThicknessPx).getOrElse(config)
        case key if SurfaceConfig.Schema.handles(key) =>
          SurfaceConfig.Schema.parse(config, key, value).getOrElse(config)
        case key if InterfaceConfig.Schema.handles(key) =>
          InterfaceConfig.Schema.parse(config, key, value).getOrElse(config)
        case key if DocumentConfig.Schema.handles(key) =>
          DocumentConfig.Schema.parse(config, key, value).getOrElse(config)
        case key if WindowConfig.Schema.handles(key) =>
          WindowConfig.Schema.parse(config, key, value).getOrElse(config)
        case "window.sitter.enabled" =>
          parseBoolean(value)
            .map(enabled => config.withWindowSitterConfig(config.windowSitterConfig.copy(enabled = enabled)))
            .getOrElse(config)
        case "window.sitter.action" =>
          WindowSitterAction
            .fromConfigKey(value)
            .map(action => config.withWindowSitterConfig(config.windowSitterConfig.copy(action = action)))
            .getOrElse(config)
        case "window.sitter.frames" =>
          config.withWindowSitterConfig(config.windowSitterConfig.copy(frames = value.split(",").toVector))
        case "window.sitter.active_ticks" =>
          value.toIntOption
            .map(ticks => config.withWindowSitterConfig(config.windowSitterConfig.copy(activeTicks = ticks)))
            .getOrElse(config)
        case "window.sitter.fast_active_ticks" =>
          value.toIntOption
            .map(ticks => config.withWindowSitterConfig(config.windowSitterConfig.copy(fastActiveTicks = ticks)))
            .getOrElse(config)
        case "window.sitter.fast_typing_threshold_ms" =>
          value.toIntOption
            .map(ms => config.withWindowSitterConfig(config.windowSitterConfig.copy(fastTypingThresholdMs = ms)))
            .getOrElse(config)
        case lspKey if lspKey.startsWith("lsp.") =>
          parseLspConfigEntry(config, lspKey, value.trim)
        case hotkeyKey if hotkeyKey.startsWith("hotkey.") =>
          HotkeyAction.values
            .find(action => s"hotkey.${action.configKey}" == hotkeyKey)
            .flatMap { action =>
              val triggers = value
                .split(",")
                .toList
                .map(_.trim)
                .filter(_.nonEmpty)
                .map(HotkeyTrigger.parse)
              if triggers.nonEmpty && triggers.forall(_.isDefined) then
                Some(
                  config.withHotkeyConfig(
                    HotkeyConfig(config.hotkeyConfig.bindings + (action -> triggers.flatten))
                  )
                )
              else None
            }
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
          config
    }

    val withLists    = applyHoconLists(parsed, source)
    val withLspLists = applyHoconLspLists(withLists, source)
    HotkeyConfig
      .fromBindings(withLspLists.hotkeyConfig.bindings)
      .fold(_ => withLspLists.withHotkeyConfig(HotkeyConfig()), withLspLists.withHotkeyConfig)

  /** Generate configuration file content from AppConfig */
  def configToString(config: AppConfig): String =
    val animationSetting = config.characterAnimation match
      case None                                             => "none"
      case Some(anim) if anim == AnimationConfig.quick.get  => "quick"
      case Some(anim) if anim == AnimationConfig.smooth.get => "smooth"
      case Some(anim) if anim == AnimationConfig.subtle.get => "subtle"
      case Some(_)                                          => "custom" // For custom configurations
    val characterAnimationDetails =
      if animationSetting == "custom" then config.characterAnimation.fold("")(anim => s"""
             |character.animation.duration_ms = ${anim.durationMs}
             |character.animation.steps = ${anim.steps}""".stripMargin)
      else ""
    def motionAnimationSetting(animation: Option[AnimationConfig]): String =
      animation match
        case None                                             => "none"
        case Some(anim) if anim == AnimationConfig.quick.get  => "quick"
        case Some(anim) if anim == AnimationConfig.smooth.get => "smooth"
        case Some(anim) if anim == AnimationConfig.subtle.get => "subtle"
        case Some(_)                                          => "custom"
    val motionConfiguration = config.surfaceConfig.motionConfiguration match
      case Some(configuration) =>
        configuration.withFallback(MotionConfig.fromLegacy(config.surfaceConfig, configuration.baseline))
      case None => MotionConfig.fromLegacy(config.surfaceConfig)
    val motionFamilySettings = MotionFamily.values
      .map { family =>
        val settings = motionConfiguration.families(family)
        val speedScale = family match
          case MotionFamily.EditorText      => config.editorTextTransitionSpeedScale.getOrElse(settings.speedScale)
          case MotionFamily.CommandSurfaces => config.commandRunnerTransitionSpeedScale.getOrElse(settings.speedScale)
          case MotionFamily.UiTransitions   => config.uiTransitionSpeedScale.getOrElse(settings.speedScale)
          case MotionFamily.Cursor          => config.cursorTransitionSpeedScale.getOrElse(settings.speedScale)
          case MotionFamily.PinnedPanels    => settings.speedScale
        val animationSetting = motionAnimationSetting(settings.animation)
        val customAnimationDetails =
          if animationSetting == "custom" then settings.animation.fold("")(animation => s"""
               |ui.motion.family.${family.configKey}.animation.duration_ms = ${animation.durationMs}
               |ui.motion.family.${family.configKey}.animation.steps = ${animation.steps}""".stripMargin)
          else ""
        val scopedTransitions =
          if family == MotionFamily.PinnedPanels then
            s"""
               |ui.motion.family.${family.configKey}.open_transition = ${transitionKindConfigKey(settings.transitionKindFor(TransitionScope.PanelOpen))}
               |ui.motion.family.${family.configKey}.close_transition = ${transitionKindConfigKey(settings.transitionKindFor(TransitionScope.PanelClose))}""".stripMargin
          else ""
        s"""ui.motion.family.${family.configKey}.enabled = ${settings.enabled}
         |ui.motion.family.${family.configKey}.transition = ${transitionKindConfigKey(settings.transitionKind)}
         |"ui.motion.family.${family.configKey}.animation" = $animationSetting$customAnimationDetails
         |ui.motion.family.${family.configKey}.speed_scale = $speedScale$scopedTransitions""".stripMargin
      }
      .mkString("\n")
    val lspSettings = lspConfigToString(config.lspUserConfig)
    def bindingValue(bindings: List[HotkeyTrigger], defaults: List[HotkeyTrigger]): String =
      hoconString(bindings.headOption.orElse(defaults.headOption).fold("")(_.render))
    def editorBinding(action: EditorKeyAction): String =
      bindingValue(
        config.focusedKeymapConfig.editor.bindingsFor(action),
        EditorKeymapConfig.defaultBindings.getOrElse(action, Nil)
      )
    val hotkeySettings = HotkeyAction.values
      .map { action =>
        val bindings = config.hotkeyConfig.bindingsFor(action).map(_.render)
        s"hotkey.${action.configKey} = ${hoconList(bindings)}"
      }
      .mkString("\n")

    s"""# Serenity Editor Configuration
       |config.version = ${ConfigVersion.Current.value}
       |
       |# Character animation style: none, quick, smooth, subtle, custom
       |"character.animation" = $animationSetting$characterAnimationDetails
       |
       |# Syntax highlighting: true, false
       |syntax.highlighting = ${config.syntaxHighlightingEnabled}
       |
       |# Font configuration
        |font.code.family = ${hoconString(config.fontConfig.codeFontFamily)}
        |font.text.family = ${hoconString(config.fontConfig.textFontFamily)}
        |font.ui.family = ${hoconString(config.fontConfig.uiFontFamily)}
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
       |cursor.mode = ${config.cursorMode.configKey}
       |cursor.active.color = ${hoconString(config.cursorColors.active.map(formatColor).getOrElse(""))}
       |cursor.inactive.color = ${hoconString(config.cursorColors.inactive.map(formatColor).getOrElse(""))}
       |"cursor.info_bar" = ${config.cursorInfoBarMode.configKey}
       |cursor.info_bar.placement = ${config.cursorInfoBarPlacement.configKey}
       |
       |# Interface density: compact, comfortable, spacious
       |interface.density = ${config.interfaceDensity.configKey}
       |# Window chrome: auto uses themed chrome on Linux; native preserves OS snap/window animations; native-themed uses Windows system chrome colours; custom is themed and applies after restart
       |window.chrome = ${config.windowChromeMode.configKey}
       |window.sitter.enabled = ${config.windowSitterConfig.enabled}
       |window.sitter.action = ${config.windowSitterConfig.action.configKey}
       |window.sitter.frames = ${hoconList(config.windowSitterConfig.frames.toList)}
       |window.sitter.active_ticks = ${config.windowSitterConfig.activeTicks}
       |window.sitter.fast_active_ticks = ${config.windowSitterConfig.fastActiveTicks}
       |window.sitter.fast_typing_threshold_ms = ${config.windowSitterConfig.fastTypingThresholdMs}
       |ui.element_gap = ${config.uiElementGap}
       |ui.corner_radius = ${config.uiCornerRadiusPx}
       |ui.outline_thickness = ${config.uiOutlineThicknessPx}
       |command_runner.visible_rows = ${config.commandRunnerVisibleRows.map(_.toString).getOrElse("auto")}
       |command_runner.item_gap_rows = ${config.commandRunnerItemGapRows}
       |command_runner.cursor_gap_rows = ${config.commandRunnerCursorGapRows.map(_.toString).getOrElse("auto")}
       |render.fps = ${config.renderFpsTarget.configKey}
       |# Damage granularity the renderer honours: rows redraws whole visible lines; cells honours column ranges on
       |# monospaced buffers only, falling back to rows for proportional or ligature-shaped text
       |render.damage_granularity = ${config.renderDamageGranularity.configKey}
       |display.word_wrap = ${config.wordWrapEnabled}
       |display.pane_headers = ${config.showPaneHeaders}
       |display.focused_text_body = ${config.focusedTextBodyEnabled}
       |display.contextual_toolbar = ${config.contextualToolbarEnabled}
       |display.contextual_toolbar_mode = ${config.contextualToolbarDisplayMode.configKey}
       |
       |# UI material and motion presets: solid, clear, frosted, crystal, custom / reduced, subtle, smooth, expressive, custom
       |ui.material = ${config.materialPreset.configKey}
       |# Post-processing: off, scanlines, glow, scanlines-glow
       |ui.post_processing = ${config.postProcessingEffect.configKey}
       |# Draw soft shadows behind menus and panels
       |ui.shadows = ${config.uiShadowsEnabled}
       |"ui.motion" = ${motionConfiguration.baseline.configKey}
       |ui.motion.accessibility = ${motionConfiguration.accessibility.configKey}
       |${config.editorTextTransitionSpeedScale.map(value => s"ui.motion.editor_text.speed_scale = $value").getOrElse("")}
       |${config.commandRunnerTransitionSpeedScale.map(value => s"ui.motion.command_runner.speed_scale = $value").getOrElse("")}
       |${config.uiTransitionSpeedScale.map(value => s"ui.motion.ui.speed_scale = $value").getOrElse("")}
       |${config.cursorTransitionSpeedScale.map(value => s"ui.motion.cursor.speed_scale = $value").getOrElse("")}
       |$motionFamilySettings
       |
       |# Markdown rendering mode: source, split-preview, inline-lens
       |document.markdown_view = ${config.markdownViewMode.configKey}
       |
       |# Default mode for new buffers: plain-text, markdown, rich-text
       |document.default_mode = ${config.defaultDocumentMode.configKey}
       |
       |# Preferred desktop window size. Leave empty to use the default.
       |window.preferred.width = ${hoconString(config.preferredWindowSize.map(_.width).fold("")(_.toString))}
       |window.preferred.height = ${hoconString(config.preferredWindowSize.map(_.height).fold("")(_.toString))}
       |
        |# Text area insets as percentages of the central workspace.
        |text_area.left.percent = ${config.textAreaInsets.leftPercent}
        |text_area.right.percent = ${config.textAreaInsets.rightPercent}
        |text_area.top.percent = ${config.textAreaInsets.topPercent}
        |text_area.bottom.percent = ${config.textAreaInsets.bottomPercent}
        |viewport.width.percent = ${config.viewportSizing.width.percentValue}
       |viewport.width.max = ${hoconString(config.viewportSizing.width.maxCells.fold("")(_.toString))}
       |viewport.height.percent = ${config.viewportSizing.height.percentValue}
       |viewport.height.max = ${hoconString(config.viewportSizing.height.maxCells.fold("")(_.toString))}
       |
       |# LSP server overrides
       |$lspSettings
       |
       |# Spell-checking for prose buffers
       |spellcheck.enabled = ${config.spellCheck.enabled}
       |spellcheck.languages = ${hoconList(config.spellCheck.normalized.languages)}
       |spellcheck.dictionary_paths = ${hoconList(config.spellCheck.normalized.dictionaryPaths)}
       |spellcheck.words = ${hoconList(config.spellCheck.normalized.additionalWords)}
       |
       |# Hotkey overrides
       |$hotkeySettings
       |
       |# Focused keymap overrides
       |keymap.editor.page_down = ${editorBinding(EditorKeyAction.PageDown)}
       |keymap.editor.extend_selection_left = ${editorBinding(EditorKeyAction.ExtendSelectionLeft)}
       |keymap.editor.extend_selection_right = ${editorBinding(EditorKeyAction.ExtendSelectionRight)}
       |keymap.editor.extend_selection_up = ${editorBinding(EditorKeyAction.ExtendSelectionUp)}
       |keymap.editor.extend_selection_down = ${editorBinding(EditorKeyAction.ExtendSelectionDown)}
       |keymap.command_runner.submit = ${bindingValue(
        config.focusedKeymapConfig.commandRunner.bindingsFor(CommandRunnerKeyAction.Submit),
        CommandRunnerKeymapConfig.defaultBindings.getOrElse(CommandRunnerKeyAction.Submit, Nil)
      )}
       |keymap.modal.dismiss = ${bindingValue(config.focusedKeymapConfig.modal.bindingsFor(ModalKeyAction.Dismiss), ModalKeymapConfig.defaultBindings.getOrElse(ModalKeyAction.Dismiss, Nil))}
       |""".stripMargin

  /** Save configuration to file */
  def saveConfig(config: AppConfig, configPath: String): Boolean =
    saveConfig(config, Paths.get(configPath))

  def saveConfig(config: AppConfig, configPath: Path): Boolean =
    try
      AtomicFileWriter.writeBytesBlocking(configPath, configToString(config).getBytes(StandardCharsets.UTF_8))
      true
    catch case _: Exception => false

  /** Save configuration on the Cats Effect blocking pool with a structured failure result. */
  def saveConfigIO(config: AppConfig, configPath: Path): IO[Either[ConfigError, Unit]] =
    IO.blocking {
      try
        AtomicFileWriter.writeBytesBlocking(configPath, configToString(config).getBytes(StandardCharsets.UTF_8))
        Right(())
      catch
        case error: Exception =>
          Left(ConfigError("save", configPath, s"Failed to save configuration: ${error.getMessage}", Some(error)))
    }

  def saveConfigIO(config: AppConfig, configPath: String): IO[Either[ConfigError, Unit]] =
    saveConfigIO(config, Paths.get(configPath))

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

  private def inspectConfig(source: Config): ConfigMigrationReport =
    val entries = hoconEntries(source)
    val deprecatedEntries = entries
      .flatMap(entry =>
        deprecatedReplacement(entry.key).map(replacement => DeprecatedConfigEntry(entry.key, replacement))
      )
      .distinctBy(_.key)
    val unknownKeys = entries
      .map(_.key)
      .filterNot(isKnownConfigKey)
      .distinct
    val invalidEntries = entries.flatMap(entry => invalidEntry(entry.key, entry.value, entry.valueType))

    ConfigMigrationReport(
      version = ConfigVersion.Current,
      deprecatedEntries = deprecatedEntries,
      unknownKeys = unknownKeys,
      invalidEntries = invalidEntries
    )

  private def parseHoconFile(path: Path): Config =
    val options = ConfigParseOptions
      .defaults()
      .setOriginDescription(path.toString)
    val content = Files.readString(path, StandardCharsets.UTF_8)
    parseLegacyConfig(content).getOrElse(ConfigFactory.parseFile(path.toFile, options)).resolve()

  private def parseLegacyConfig(content: String): Option[Config] =
    val entries = content.linesIterator
      .map(_.trim)
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .map { line =>
        line.split("=", 2).toList match
          case key :: value :: Nil if key.trim.matches("[A-Za-z0-9_.]+") =>
            Some(LegacyEntry(key.trim, value.trim))
          case _ => None
      }
      .toList

    Option.when(entries.nonEmpty && entries.forall(_.isDefined)) {
      val flatEntries = entries.flatten
      val byKey       = flatEntries.map(entry => entry.key -> entry).toMap

      def referencedKeys(keys: Set[String]): Set[String] =
        val expanded = keys ++ keys.flatMap(key => byKey.get(key).toSet.flatMap(entry => substitutionKeys(entry.value)))
        if expanded == keys then keys else referencedKeys(expanded)

      val references = referencedKeys(flatEntries.flatMap(entry => substitutionKeys(entry.value)).toSet)
      val resolutionScope = references
        .flatMap(byKey.get)
        .foldLeft(ConfigFactory.empty())((scope, entry) => parseLegacyLookupEntry(entry).withFallback(scope))
        .resolve()

      val resolvedEntries = flatEntries.flatMap { entry =>
        val quotedKey = s""""${entry.key}""""
        try
          val resolved = ConfigFactory
            .parseString(s"$quotedKey = ${entry.value}")
            .withFallback(resolutionScope)
            .resolve()
          Option.when(resolved.hasPath(quotedKey))(quotedKey -> resolved.getValue(quotedKey).unwrapped())
        catch case _: ConfigException.Parse => Some(quotedKey -> entry.value)
      }

      ConfigFactory.parseMap(resolvedEntries.toMap.asJava)
    }

  final private case class LegacyEntry(key: String, value: String)

  private val substitutionPattern = """\$\{\??([^}]+)\}""".r

  private def substitutionKeys(value: String): Set[String] =
    substitutionPattern.findAllMatchIn(value).map(_.group(1)).toSet

  private def parseLegacyLookupEntry(entry: LegacyEntry): Config =
    try ConfigFactory.parseString(s"${entry.key} = ${entry.value}")
    catch
      case _: ConfigException.Parse =>
        ConfigFactory.parseMap(Map(entry.key -> entry.value).asJava)

  final private case class HoconEntry(key: String, value: String, valueType: ConfigValueType)

  private def hoconEntries(source: Config): List[HoconEntry] =
    source
      .entrySet()
      .asScala
      .toList
      .sortBy(_.getKey)
      .map { entry =>
        val key = entry.getKey.stripPrefix("\"").stripSuffix("\"").toLowerCase(Locale.ROOT)
        val value = entry.getValue.valueType match
          case ConfigValueType.LIST =>
            source.getList(entry.getKey).asScala.map(_.unwrapped().toString).mkString(",")
          case _ => entry.getValue.unwrapped().toString
        HoconEntry(key, value, entry.getValue.valueType)
      }

  private def applyHoconLists(config: AppConfig, source: Config): AppConfig =
    def strings(path: String): Option[List[String]] =
      source
        .entrySet()
        .asScala
        .find(_.getKey.stripPrefix("\"").stripSuffix("\"") == path)
        .flatMap { entry =>
          if entry.getValue.valueType == ConfigValueType.LIST then
            Some(source.getList(entry.getKey).asScala.map(_.unwrapped().toString).toList)
          else None
        }

    val spellCheck = config.spellCheck
    val updatedSpellCheck = spellCheck.copy(
      languages = strings("spellcheck.languages").getOrElse(spellCheck.languages),
      dictionaryPaths = strings("spellcheck.dictionary_paths").getOrElse(spellCheck.dictionaryPaths),
      additionalWords = strings("spellcheck.words").getOrElse(spellCheck.additionalWords)
    )
    val withSpellCheck = config.withSpellCheck(updatedSpellCheck)
    source
      .entrySet()
      .asScala
      .find(_.getKey.stripPrefix("\"").stripSuffix("\"") == "window.sitter.frames")
      .filter(_.getValue.valueType == ConfigValueType.LIST)
      .map(entry => source.getList(entry.getKey).asScala.map(_.unwrapped().toString).toVector)
      .fold(withSpellCheck)(frames =>
        withSpellCheck.withWindowSitterConfig(withSpellCheck.windowSitterConfig.copy(frames = frames))
      )

  private def applyHoconLspLists(config: AppConfig, source: Config): AppConfig =
    source.entrySet().asScala.foldLeft(config) { (current, entry) =>
      val key = entry.getKey.stripPrefix("\"").stripSuffix("\"").toLowerCase(Locale.ROOT)
      if key.startsWith("lsp.") && key.endsWith(".args") && entry.getValue.valueType == ConfigValueType.LIST then
        key.split("\\.", 3).toList match
          case "lsp" :: languageKey :: "args" :: Nil =>
            LanguageId.fromString(languageKey).fold(current) { languageId =>
              val args = source.getList(entry.getKey).asScala.map(_.unwrapped().toString).toList
              updateLspOverride(current, languageId)(_.copy(args = Some(args)))
            }
          case _ => current
      else current
    }

  private def deprecatedReplacement(key: String): Option[String] =
    ConfigKeySchema.deprecatedReplacement(key)

  private def isKnownConfigKey(key: String): Boolean =
    ConfigKeySchema.isKnownKey(key)

  private def invalidEntry(
    key: String,
    value: String,
    valueType: ConfigValueType
  ): Option[InvalidConfigEntry] =
    val normalizedValue = value.trim.toLowerCase
    val invalid =
      key match
        case "character.animation" | "character_animation" =>
          !Set("none", "false", "off", "disabled", "quick", "smooth", "subtle", "custom").contains(normalizedValue)
        case "character.animation.duration_ms" | "character.animation.duration.ms" | "character_animation_duration_ms" |
            "character.animation.steps" | "character_animation_steps" =>
          value.trim.toIntOption.forall(_ <= 0)
        case key if LanguageToolsConfig.Schema.handles(key) =>
          LanguageToolsConfig.Schema.invalidValue(key, value)
        case "font.code.ligatures" | "font_code_ligatures" | "font.text.ligatures" | "font.prose.ligatures" |
            "font_text_ligatures" | "font_prose_ligatures" | "font.ui.ligatures" | "font_ui_ligatures" |
            "font.ligatures" | "font_ligatures" =>
          parseBoolean(value).isEmpty
        case key if SurfaceConfig.Schema.handles(key) =>
          SurfaceConfig.Schema.invalidValue(key, value)
        case "font.code.size" | "font_code_size" | "font.text.size" | "font.prose.size" | "font_text_size" |
            "font_prose_size" | "font.size" | "font_size" | "font.ui.size" | "font_ui_size" =>
          value.trim.toFloatOption.isEmpty
        case "font.scale.mode" | "font_scale_mode" =>
          parseTextScaleMode(value).isEmpty
        case "font.text_scale" | "font.text.scale" | "font_text_scale" =>
          parseTextScaleMultiplier(value).isEmpty
        case key if CursorConfig.Schema.handles(key) =>
          CursorConfig.Schema.invalidValue(key, value)
        case key if DocumentConfig.Schema.handles(key) =>
          DocumentConfig.Schema.invalidValue(key, value)
        case key if WindowConfig.Schema.handles(key) =>
          WindowConfig.Schema.invalidValue(key, value)
        case "window.sitter.enabled" =>
          parseBoolean(value).isEmpty
        case "window.sitter.action" =>
          WindowSitterAction.fromConfigKey(value).isEmpty
        case "window.sitter.frames" =>
          value.split(",").forall(_.trim.isEmpty)
        case "window.sitter.active_ticks" | "window.sitter.fast_active_ticks" |
            "window.sitter.fast_typing_threshold_ms" =>
          value.trim.toIntOption.forall(_ <= 0)
        case key if InterfaceConfig.Schema.handles(key) =>
          InterfaceConfig.Schema.invalidValue(key, value)
        case key if key.startsWith("hotkey.") || key.startsWith("keymap.") =>
          value.split(",").toList.map(_.trim).filter(_.nonEmpty).exists(HotkeyTrigger.parse(_).isEmpty)
        case key if key.startsWith("lsp.") =>
          key.split("\\.", 3).toList match
            case "lsp" :: _ :: "enabled" :: Nil => parseBoolean(value).isEmpty
            case "lsp" :: _ :: "command" :: Nil => value.trim.isEmpty
            case "lsp" :: _ :: "args" :: Nil    => valueType != ConfigValueType.LIST && value.trim.isEmpty
            case _                              => false
        case _ =>
          false

    Option.when(invalid)(InvalidConfigEntry(key, value, "Invalid value for supported config key"))

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

  private def transitionKindConfigKey(kind: TransitionKind): String =
    kind match
      case TransitionKind.Fade                   => "fade"
      case TransitionKind.TypedText              => "typed"
      case TransitionKind.DirectionalSweep       => "directional"
      case TransitionKind.LineAndCharacterTandem => "tandem"
      case TransitionKind.Disabled               => "off"
      case TransitionKind.OutlineThenContent     => "outline"

  private def parseUiElementGap(value: String): Option[Double] =
    value.toDoubleOption.filter(gap =>
      gap.isFinite && gap >= AppConfig.MinUiElementGap && gap <= AppConfig.MaxUiElementGap
    )

  private def parseUiCornerRadiusPx(value: String): Option[Int] =
    value.toIntOption.filter(radius =>
      radius >= AppConfig.MinUiCornerRadiusPx && radius <= AppConfig.MaxUiCornerRadiusPx
    )

  private def parseUiOutlineThicknessPx(value: String): Option[Int] =
    value.toIntOption.filter(thickness =>
      thickness >= AppConfig.MinUiOutlineThicknessPx && thickness <= AppConfig.MaxUiOutlineThicknessPx
    )

  private def formatColor(color: java.awt.Color): String =
    val rgb = f"#${color.getRed}%02X${color.getGreen}%02X${color.getBlue}%02X"
    if color.getAlpha == 255 then rgb else f"$rgb${color.getAlpha}%02X"

  private def hoconString(value: String): String =
    val safe = value.nonEmpty && value.forall(char => char.isLetterOrDigit || "_./-".contains(char))
    if safe then value else s"\"${value.replace("\\", "\\\\").replace("\"", "\\\"")}\""

  private def hoconList(values: List[String]): String =
    values.map(value => s"\"${value.replace("\\", "\\\\").replace("\"", "\\\"")}\"").mkString("[", ", ", "]")

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
            override_.command.map(command => s"lsp.$languageId.command = ${hoconString(command)}"),
            override_.args.map(args => s"lsp.$languageId.args = ${hoconList(args)}")
          ).flatten
      }
      .mkString("\n")

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
                          |render.fps = 60
                          |render.damage_granularity = rows
                          |
                          |# UI material and motion presets: solid, clear, frosted, crystal, custom / reduced, subtle, smooth, expressive, custom
                          |ui.material = frosted
                          |ui.motion = smooth
                          |ui.motion.speed_scale = 1.0
                          |ui.motion.editor_text.speed_scale = 1.0
                          |ui.motion.command_runner.speed_scale = 1.0
                          |ui.motion.ui.speed_scale = 1.0
                          |ui.motion.command_runner = smooth
                          |ui.motion.ui = smooth
                          |ui.motion.editor_text = fade
                          |ui.motion.panel_open = outline
                          |ui.motion.panel_close = fade
                          |
                          |# Preferred desktop window size. Leave empty to use the default.
                          |window.preferred.width =
                          |window.preferred.height =
                          |
                          |# Spell-checking for prose buffers
                          |spellcheck.enabled = false
                          |spellcheck.languages = en
                          |spellcheck.dictionary_paths =
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

      AtomicFileWriter.writeBytesBlocking(Paths.get(path), sampleConfig.getBytes(StandardCharsets.UTF_8))
      true
    catch case _: Exception => false
