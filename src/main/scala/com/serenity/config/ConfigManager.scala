package com.serenity.config

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.Locale

import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.control.NonFatal

import cats.effect.IO
import com.serenity.animation.{AnimationConfig, WindowSitterAction}
import com.serenity.io.AtomicFileWriter
import com.serenity.lsp.config.{LanguageId, LspServerOverride, LspUserConfig}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.TextScaleMode
import com.typesafe.config.{Config, ConfigException, ConfigFactory, ConfigParseOptions, ConfigValue, ConfigValueType}

/** Manages loading and saving application configuration */
object ConfigManager:

  val defaultConfigPath: Path =
    Paths.get(System.getProperty("user.home"), ".serenity", "config.conf")

  /** Available animation presets */
  object Presets:
    val none   = AppConfig.default.withoutCharacterAnimation
    val quick  = AppConfig.default.withCharacterAnimation(AnimationConfig.Enabled.quick)
    val smooth = AppConfig.default.withCharacterAnimation(AnimationConfig.Enabled.smooth)
    val subtle = AppConfig.default.withCharacterAnimation(AnimationConfig.Enabled.subtle)

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
      val HoconEntry(key, value, _, raw) = entry
      // The registry knows every setting that is one key to one value, in both directions at once. Only the settings
      // that are not -- the animation presets, the motion families, the key groups, and the spellings that set more
      // than one field -- are still spelled out below.
      ConfigRegistry
        .find(key)
        .flatMap(field => field.readValue(config, raw))
        .orElse(ConfigLegacyKeys.find(key).flatMap(_.read(config, value)))
        .getOrElse(key match
        case "character.animation" | "character.animation.preset" | "character_animation" =>
          value.trim.toLowerCase match
            case "none" | "false" | "off" | "disabled" =>
              config.withoutCharacterAnimation
            case "quick" =>
              config.withCharacterAnimation(AnimationConfig.Enabled.quick)
            case "smooth" =>
              config.withCharacterAnimation(AnimationConfig.Enabled.smooth)
            case "subtle" =>
              config.withCharacterAnimation(AnimationConfig.Enabled.subtle)
            case "custom" =>
              config.withCharacterAnimation(
                config.editorConfig.characterAnimation.getOrElse(AnimationConfig.Enabled.smooth)
              )
            case _ =>
              config // Unknown value, keep current config
        case "character.animation.duration_ms" | "character.animation.duration.ms" |
            "character_animation_duration_ms" =>
          value.trim.toIntOption
            .filter(_ > 0)
            .map(ms =>
              config.withCharacterAnimation(
                config.editorConfig.characterAnimation
                  .getOrElse(AnimationConfig.Enabled.smooth)
                  .copy(totalDuration = scala.concurrent.duration.Duration.fromNanos(ms * 1_000_000L))
              )
            )
            .getOrElse(config)
        case "character.animation.steps" | "character_animation_steps" =>
          value.trim.toIntOption
            .filter(_ > 0)
            .map(steps =>
              config.withCharacterAnimation(
                config.editorConfig.characterAnimation
                  .getOrElse(AnimationConfig.Enabled.smooth)
                  .copy(steps = steps)
              )
            )
            .getOrElse(config)
        case "editor.minimum_pane_width" | "editor.minimum.pane.width" | "editor_minimum_pane_width" =>
          value.trim.toIntOption.map(config.withMinimumPaneWidth).getOrElse(config)
        case key if LanguageToolsConfig.Schema.handles(key) =>
          LanguageToolsConfig.Schema.parse(config, key, value).getOrElse(config)
        case "font.code.family" | "font_code_family" =>
          config.withFontConfig(config.editorConfig.fontConfig.copy(codeFontFamily = value.trim))
        case "font.text.family" | "font_text_family" =>
          config.withFontConfig(config.editorConfig.fontConfig.copy(textFontFamily = value.trim))
        case "font.ui.family" | "font_ui_family" =>
          val family =
            if value.trim == "${font.text.family}" then config.editorConfig.fontConfig.textFontFamily else value.trim
          config.withFontConfig(config.editorConfig.fontConfig.copy(uiFontFamily = family))
        case "font.code.size" | "font_code_size" =>
          value.trim.toFloatOption
            .map(size => config.withFontConfig(config.editorConfig.fontConfig.copy(fontSize = clampFontSize(size))))
            .getOrElse(config)
        case "font.text.size" | "font.prose.size" | "font_text_size" | "font_prose_size" =>
          value.trim.toFloatOption
            .map(size => config.withFontConfig(config.editorConfig.fontConfig.copy(textFontSize = clampFontSize(size))))
            .getOrElse(config)
        case "font.size" | "font_size" =>
          value.trim.toFloatOption
            .map(size =>
              config.withFontConfig(
                config.editorConfig.fontConfig.copy(fontSize = clampFontSize(size), textFontSize = clampFontSize(size))
              )
            )
            .getOrElse(config)
        case "font.ui.size" | "font_ui_size" =>
          value.trim.toFloatOption
            .map(size => config.withFontConfig(config.editorConfig.fontConfig.copy(uiFontSize = clampFontSize(size))))
            .getOrElse(config)
        case "font.scale.mode" | "font_scale_mode" =>
          // Only the mode. Automatic scaling derives its multiplier rather than reading one, but resolving it here
          // depended on this key being folded after `font.text_scale`, which is an ordering the key names happened to
          // give rather than one anything stated. `resolveAutoTextScale` runs once the whole file has been read.
          parseTextScaleMode(value.trim)
            .map(mode => config.withFontConfig(config.editorConfig.fontConfig.copy(textScaleMode = mode)))
            .getOrElse(config)
        case "font.text_scale" | "font.text.scale" | "font_text_scale" =>
          // Only the multiplier. Inferring "manual" from a non-default multiplier is for files that never say what the
          // mode is (see `inferTextScaleMode` below); doing it here overrode an explicit `font.scale.mode = off`,
          // because entries are folded in key order and the multiplier's key sorts after the mode's.
          parseTextScaleMultiplier(value.trim)
            .map(scale => config.withFontConfig(config.editorConfig.fontConfig.copy(textScaleMultiplier = scale)))
            .getOrElse(config)
        case "font.code.ligatures" | "font_code_ligatures" =>
          value.trim.toLowerCase match
            case "true" | "on" | "enabled" =>
              config.withFontConfig(config.editorConfig.fontConfig.copy(enableLigatures = true))
            case "false" | "off" | "disabled" =>
              config.withFontConfig(config.editorConfig.fontConfig.copy(enableLigatures = false))
            case _ =>
              config
        case "font.text.ligatures" | "font.prose.ligatures" | "font_text_ligatures" | "font_prose_ligatures" =>
          value.trim.toLowerCase match
            case "true" | "on" | "enabled" =>
              config.withFontConfig(config.editorConfig.fontConfig.copy(textLigatures = true))
            case "false" | "off" | "disabled" =>
              config.withFontConfig(config.editorConfig.fontConfig.copy(textLigatures = false))
            case _ =>
              config
        case "font.ui.ligatures" | "font_ui_ligatures" =>
          value.trim.toLowerCase match
            case "true" | "on" | "enabled" =>
              config.withFontConfig(config.editorConfig.fontConfig.copy(uiLigatures = true))
            case "false" | "off" | "disabled" =>
              config.withFontConfig(config.editorConfig.fontConfig.copy(uiLigatures = false))
            case _ =>
              config
        case "font.ligatures" | "font_ligatures" =>
          value.trim.toLowerCase match
            case "true" | "on" | "enabled" =>
              config.withFontConfig(config.editorConfig.fontConfig.copy(enableLigatures = true, textLigatures = true))
            case "false" | "off" | "disabled" =>
              config.withFontConfig(config.editorConfig.fontConfig.copy(enableLigatures = false, textLigatures = false))
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
        case key if InputConfig.Schema.handles(key) =>
          InputConfig.Schema.parse(config, key, value).getOrElse(config)
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
                    HotkeyConfig(config.inputConfig.hotkeyConfig.bindings + (action -> triggers.flatten))
                  )
                )
              else None
            }
            .getOrElse(config)
        case keymapKey if keymapKey.startsWith("keymap.editor.") =>
          EditorKeyAction.values
            .find(action => s"keymap.editor.${action.configKey}" == keymapKey)
            .map(action => config.withKeymapBinding(KeymapGroup.Editor)(action, value.trim))
            .getOrElse(config)
        case keymapKey if keymapKey.startsWith("keymap.command_runner.") =>
          CommandRunnerKeyAction.values
            .find(action => s"keymap.command_runner.${action.configKey}" == keymapKey)
            .map(action => config.withKeymapBinding(KeymapGroup.CommandRunner)(action, value.trim))
            .getOrElse(config)
        case keymapKey if keymapKey.startsWith("keymap.modal.") =>
          ModalKeyAction.values
            .find(action => s"keymap.modal.${action.configKey}" == keymapKey)
            .map(action => config.withKeymapBinding(KeymapGroup.Modal)(action, value.trim))
            .getOrElse(config)
        case keymapKey if keymapKey.startsWith("keymap.panel.") =>
          PanelKeyAction.values
            .find(action => s"keymap.panel.${action.configKey}" == keymapKey)
            .map(action => config.withKeymapBinding(KeymapGroup.Panel)(action, value.trim))
            .getOrElse(config)
        case keymapKey if keymapKey.startsWith("keymap.peek.") =>
          PeekKeyAction.values
            .find(action => s"keymap.peek.${action.configKey}" == keymapKey)
            .map(action => config.withKeymapBinding(KeymapGroup.Peek)(action, value.trim))
            .getOrElse(config)
        case "config.version" =>
          config
        case _ =>
          config
      )
    }

    val scaled       = inferTextScaleMode(parsed, entries)
    val withLists    = applyHoconLists(scaled, source)
    val withLspLists = applyHoconLspLists(withLists, source)
    HotkeyConfig
      .fromBindings(withLspLists.inputConfig.hotkeyConfig.bindings)
      .fold(_ => withLspLists.withHotkeyConfig(HotkeyConfig()), withLspLists.withHotkeyConfig)

  /** Generate configuration file content from AppConfig */
  def configToString(config: AppConfig): String = ConfigFileFormat.render(config)

  /** Save configuration to file */
  def saveConfig(config: AppConfig, configPath: String): Boolean =
    saveConfig(config, Paths.get(configPath))

  def saveConfig(config: AppConfig, configPath: Path): Boolean =
    renderedConfig(config).fold(
      _ => false,
      text =>
        try
          AtomicFileWriter.writeBytesBlocking(configPath, text.getBytes(StandardCharsets.UTF_8))
          true
        catch case _: Exception => false
    )

  /** The config text to write, refused if it is not something this module could read back.
    *
    * An unparseable file is worse than a failed save: `loadConfigResult` falls back to defaults for the whole file, so
    * one bad value silently resets every other setting the user had. That is exactly what an unquoted comma in the
    * cursor info bar's segment list used to do. Checking here keeps a formatting mistake in one setting from reaching
    * the file at all, and leaves whatever the user already had in place. A setting the file would swallow rather than
    * reject -- a key written at a path that also has children -- costs the user that one setting just as silently, so
    * it is refused on the same terms.
    */
  private def renderedConfig(config: AppConfig): Either[String, String] =
    ConfigFileFormat.unwritableSettings(config) match
      case Nil =>
        val text = ConfigFileFormat.render(config)
        Try(ConfigFactory.parseString(text)).toEither
          .map(_ => text)
          .left
          .map(error => s"Configuration would not parse back: ${error.getMessage}")
      case lost =>
        Left(
          s"Configuration settings would not survive being written: ${lost.mkString(", ")}. A key cannot be both a " +
            "value and the parent of other keys."
        )

  /** Copy a config file that could not be read to a sibling `.unreadable` path, returning where it went.
    *
    * A file that fails to parse costs the user every setting in it for the session, and the next settings change writes
    * defaults over it -- so the only copy of what they had configured is gone, without them ever being told. Keeping it
    * aside makes that recoverable. Best-effort: if the copy itself fails there is nothing further to do about it, and
    * the load carries on with defaults either way.
    */
  def preserveUnreadableConfig(path: Path): Option[Path] =
    try
      val target = path.resolveSibling(s"${path.getFileName}.unreadable")
      Files.copy(path, target, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      Some(target)
    catch case NonFatal(_) => None

  /** Save configuration on the Cats Effect blocking pool with a structured failure result. */
  def saveConfigIO(config: AppConfig, configPath: Path): IO[Either[ConfigError, Unit]] =
    IO.blocking {
      renderedConfig(config) match
        case Left(problem) => Left(ConfigError("save", configPath, s"Failed to save configuration: $problem", None))
        case Right(text) =>
          try
            AtomicFileWriter.writeBytesBlocking(configPath, text.getBytes(StandardCharsets.UTF_8))
            Right(())
          catch
            case error: Exception =>
              Left(ConfigError("save", configPath, s"Failed to save configuration: ${error.getMessage}", Some(error)))
    }

  def saveConfigIO(config: AppConfig, configPath: String): IO[Either[ConfigError, Unit]] =
    saveConfigIO(config, Paths.get(configPath))

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

  final private case class HoconEntry(key: String, value: String, valueType: ConfigValueType, raw: ConfigValue)

  /** Entries in the order they are applied: shallower paths first, then alphabetically.
    *
    * The parse is a fold, so a broader setting has to be applied before the narrower ones that refine it --
    * `ui.motion.preset` rebuilds every motion family, and `ui.motion.family.command_surfaces.transition` then adjusts
    * one of them. Sorting on the key alone made that ordering an accident of the alphabet: it held while the key was
    * spelled `ui.motion` (which sorts before `ui.motion.family.…`) and broke the moment it was renamed. Depth says what
    * was meant.
    */
  private def hoconEntries(source: Config): List[HoconEntry] =
    source
      .entrySet()
      .asScala
      .toList
      .sortBy(entry => (entry.getKey.count(_ == '.'), entry.getKey))
      .map { entry =>
        val key = entry.getKey.stripPrefix("\"").stripSuffix("\"").toLowerCase(Locale.ROOT)
        val value = entry.getValue.valueType match
          case ConfigValueType.LIST =>
            source.getList(entry.getKey).asScala.map(_.unwrapped().toString).mkString(",")
          case _ => entry.getValue.unwrapped().toString
        HoconEntry(key, value, entry.getValue.valueType, entry.getValue)
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

    val spellCheck = config.languageToolsConfig.spellCheck
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
        case "character.animation" | "character.animation.preset" | "character_animation" =>
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

  /** A config that carries a text scale but never says which mode it is in means manual scaling -- that is what the
    * multiplier was for before `font.scale.mode` existed. A config that does say is taken at its word, including when
    * it says the scaling is off.
    */
  private def inferTextScaleMode(config: AppConfig, entries: List[HoconEntry]): AppConfig =
    val statesMode = entries.exists(entry => textScaleModeKeys.contains(entry.key))
    val fontConfig = config.editorConfig.fontConfig
    val withMode =
      if statesMode || fontConfig.textScaleMultiplier == 1.0 then fontConfig
      else fontConfig.copy(textScaleMode = TextScaleMode.Manual)
    config.withFontConfig(withMode.resolveAutoTextScale(1.0))

  private val textScaleModeKeys: Set[String] = Set("font.scale.mode", "font_scale_mode")

  private def parseTextScaleMode(value: String): Option[TextScaleMode] =
    value.toLowerCase match
      case "auto"                      => Some(TextScaleMode.Auto)
      case "manual" | "custom"         => Some(TextScaleMode.Manual)
      case "off" | "none" | "disabled" => Some(TextScaleMode.Off)
      case _                           => None

  private def parseTextScaleMultiplier(value: String): Option[Double] =
    value.toDoubleOption
      .filter(scale => scale >= FontLoader.FontConfig.MinTextScale && scale <= FontLoader.FontConfig.MaxTextScale)

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
    val servers  = config.languageToolsConfig.lspUserConfig.servers.getOrElse(Map.empty)
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
