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
            case lspKey if lspKey.startsWith("lsp.") =>
              parseLspConfigEntry(config, lspKey, value.trim)
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
    val characterAnimationDetails =
      if animationSetting == "custom" then config.characterAnimation.fold("")(anim => s"""
             |character.animation.duration_ms = ${anim.durationMs}
             |character.animation.steps = ${anim.steps}""".stripMargin)
      else ""
    val commandRunnerAnimationSetting = config.commandRunnerAnimation match
      case None                                             => "none"
      case Some(anim) if anim == AnimationConfig.quick.get  => "quick"
      case Some(anim) if anim == AnimationConfig.smooth.get => "smooth"
      case Some(anim) if anim == AnimationConfig.subtle.get => "subtle"
      case Some(_)                                          => "custom"
    val uiAnimationSetting = config.uiAnimation match
      case None                                             => "none"
      case Some(anim) if anim == AnimationConfig.quick.get  => "quick"
      case Some(anim) if anim == AnimationConfig.smooth.get => "smooth"
      case Some(anim) if anim == AnimationConfig.subtle.get => "subtle"
      case Some(_)                                          => "custom"
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
        s"""ui.motion.family.${family.configKey}.enabled = ${settings.enabled}
         |ui.motion.family.${family.configKey}.transition = ${transitionKindConfigKey(settings.transitionKind)}
         |ui.motion.family.${family.configKey}.animation = ${motionAnimationSetting(settings.animation)}
         |ui.motion.family.${family.configKey}.speed_scale = ${settings.speedScale}""".stripMargin
      }
      .mkString("\n")
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
       |# Character animation style: none, quick, smooth, subtle, custom
       |character.animation = $animationSetting$characterAnimationDetails
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
       |cursor.mode = ${config.cursorMode.configKey}
       |cursor.active.color = ${config.cursorColors.active.map(formatColor).getOrElse("")}
       |cursor.inactive.color = ${config.cursorColors.inactive.map(formatColor).getOrElse("")}
       |cursor.info_bar = ${config.cursorInfoBarMode.configKey}
       |cursor.info_bar.placement = ${config.cursorInfoBarPlacement.configKey}
       |
       |# Interface density: compact, comfortable, spacious
       |interface.density = ${config.interfaceDensity.configKey}
       |# Window chrome: native preserves OS snap/window animations; native-themed uses Windows system chrome colours; custom is themed and applies after restart
       |window.chrome = ${config.windowChromeMode.configKey}
       |ui.element_gap = ${config.uiElementGap}
       |ui.corner_radius = ${config.uiCornerRadiusPx}
       |ui.outline_thickness = ${config.uiOutlineThicknessPx}
       |command_runner.visible_rows = ${config.commandRunnerVisibleRows.map(_.toString).getOrElse("auto")}
       |command_runner.item_gap_rows = ${config.commandRunnerItemGapRows}
       |command_runner.cursor_gap_rows = ${config.commandRunnerCursorGapRows.map(_.toString).getOrElse("auto")}
       |render.fps = ${config.renderFpsTarget.configKey}
       |display.word_wrap = ${config.wordWrapEnabled}
       |display.focused_text_body = ${config.focusedTextBodyEnabled}
       |display.contextual_toolbar = ${config.contextualToolbarEnabled}
       |display.contextual_toolbar_mode = ${config.contextualToolbarDisplayMode.configKey}
       |
       |# UI material and motion presets: solid, clear, frosted, crystal, custom / reduced, subtle, smooth, expressive, custom
       |ui.material = ${config.materialPreset.configKey}
       |# Post-processing: off, scanlines, glow
       |ui.post_processing = ${config.postProcessingEffect.configKey}
       |ui.motion = ${motionConfiguration.baseline.configKey}
       |ui.motion.speed_scale = ${config.elementTransitionSpeedScale}
       |ui.motion.editor_text.speed_scale = ${config.effectiveEditorTextTransitionSpeedScale}
        |ui.motion.command_runner.speed_scale = ${config.effectiveCommandRunnerTransitionSpeedScale}
       |ui.motion.ui.speed_scale = ${config.effectiveUiTransitionSpeedScale}
       |ui.motion.cursor.speed_scale = ${config.effectiveCursorTransitionSpeedScale}
        |ui.motion.command_runner = $commandRunnerAnimationSetting
        |ui.motion.command_runner_reveal = ${transitionKindConfigKey(config.commandRunnerTransitionKind.getOrElse(TransitionKind.Fade))}
        |ui.motion.ui = $uiAnimationSetting
       |ui.motion.editor_text = ${transitionKindConfigKey(config.editorInsertionTransitionKind)}
       |ui.motion.panel_open = ${transitionKindConfigKey(config.panelOpenTransitionKind.getOrElse(TransitionKind.OutlineThenContent))}
       |ui.motion.panel_close = ${transitionKindConfigKey(config.panelCloseTransitionKind.getOrElse(TransitionKind.Fade))}
       |ui.motion.accessibility = ${motionConfiguration.accessibility.configKey}
       |$motionFamilySettings
       |
       |# Markdown rendering mode: source, split-preview, inline-lens
       |document.markdown_view = ${config.markdownViewMode.configKey}
       |
       |# Default mode for new buffers: plain-text, markdown, rich-text
       |document.default_mode = ${config.defaultDocumentMode.configKey}
       |
       |# Preferred desktop window size. Leave empty to use the default.
       |window.preferred.width = ${config.preferredWindowSize.map(_.width).fold("")(_.toString)}
       |window.preferred.height = ${config.preferredWindowSize.map(_.height).fold("")(_.toString)}
       |
        |# Text area insets as percentages of the central workspace.
        |text_area.left.percent = ${config.textAreaInsets.leftPercent}
        |text_area.right.percent = ${config.textAreaInsets.rightPercent}
        |text_area.top.percent = ${config.textAreaInsets.topPercent}
        |text_area.bottom.percent = ${config.textAreaInsets.bottomPercent}
        |viewport.width.percent = ${config.viewportSizing.width.percentValue}
       |viewport.width.max = ${config.viewportSizing.width.maxCells.fold("")(_.toString)}
       |viewport.height.percent = ${config.viewportSizing.height.percentValue}
       |viewport.height.max = ${config.viewportSizing.height.maxCells.fold("")(_.toString)}
       |
       |# LSP server overrides
       |$lspSettings
       |
       |# Spell-checking for prose buffers
       |spellcheck.enabled = ${config.spellCheck.enabled}
       |spellcheck.languages = ${config.spellCheck.normalized.languages.mkString(",")}
       |spellcheck.dictionary_paths = ${config.spellCheck.normalized.dictionaryPaths.mkString(",")}
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
    ConfigKeySchema.deprecatedReplacement(key)

  private def isKnownConfigKey(key: String): Boolean =
    ConfigKeySchema.isKnownKey(key)

  private def invalidEntry(key: String, value: String): Option[InvalidConfigEntry] =
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
        case key if InterfaceConfig.Schema.handles(key) =>
          InterfaceConfig.Schema.invalidValue(key, value)
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

  private def parseUiElementGap(value: String): Option[Int] =
    value.toIntOption.filter(gap => gap >= AppConfig.MinUiElementGap && gap <= AppConfig.MaxUiElementGap)

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

      Files.write(Paths.get(path), sampleConfig.getBytes(StandardCharsets.UTF_8))
      true
    catch case _: Exception => false
