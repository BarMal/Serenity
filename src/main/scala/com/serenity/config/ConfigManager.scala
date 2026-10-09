package com.serenity.config

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.Instant
import java.util.Locale

import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.control.NonFatal

import cats.effect.IO
import com.serenity.io.AtomicFileWriter
import com.serenity.lsp.config.{LanguageId, LspServerOverride, LspUserConfig}
import com.serenity.ui.fonts.FontLoader.TextScaleMode
import com.typesafe.config.{Config, ConfigException, ConfigFactory, ConfigParseOptions, ConfigValue, ConfigValueType}
import org.slf4j.LoggerFactory

/** Manages loading and saving application configuration */
object ConfigManager:

  // `loadConfigIO`'s fallback-to-defaults path logs the same way `CrashReporter` does rather than through the
  // `Logger[IO]` the structured `loadConfigResultIO` path uses.
  private val logger = LoggerFactory.getLogger("com.serenity.config.ConfigManager")

  val defaultConfigPath: Path =
    Paths.get(System.getProperty("user.home"), ".serenity", "config.conf")

  /** Load configuration from file on the Cats Effect blocking pool, falling back to defaults on a missing or
    * unparseable file.
    */
  def loadConfigIO(configPath: Option[String] = None): IO[AppConfig] =
    IO.blocking {
      val path = configPath.map(Paths.get(_)).getOrElse(defaultConfigPath)
      if Files.exists(path) then
        try parseConfigResult(path).config
        catch
          case NonFatal(error) =>
            logger.error(s"[CONFIG] Failed to load config from $path, using defaults", error)
            AppConfig.default
      else AppConfig.default
    }

  /** Load configuration with a migration/diagnostic report on the Cats Effect blocking pool.
    *
    * An invalid value costs that one setting, which falls back to its default and is listed in the report; every other
    * setting is kept. Only a file that cannot be parsed at all is a `Left`, because then there is nothing to keep.
    */
  def loadConfigResultIO(configPath: Option[String] = None): IO[Either[ConfigError, ConfigLoadResult]] =
    IO.blocking {
      val path = configPath.map(Paths.get(_)).getOrElse(defaultConfigPath)
      if !Files.exists(path) then Right(ConfigLoadResult(AppConfig.default, ConfigMigrationReport.empty))
      else
        try Right(parseConfigResult(path))
        catch
          case error: Exception =>
            Left(ConfigError("load", path, s"Failed to load configuration: ${error.getMessage}", Some(error)))
    }

  private[config] def parseConfigResult(
    path: Path,
    plan: ConfigMigrations.Plan = ConfigMigrations.installed
  ): ConfigLoadResult =
    parseConfigResult(parseHoconFile(path), plan)

  private def parseConfigResult(raw: Config, plan: ConfigMigrations.Plan): ConfigLoadResult =
    val migration           = ConfigMigrations.migrate(raw, plan)
    val source              = migration.config
    val (config, conflicts) = parseConfig(source)
    val report = inspectConfig(source, migration.found, ConfigVersionStatus.classify(raw, plan.target), plan.target)
    ConfigLoadResult(
      config,
      report.copy(
        hotkeyConflicts = conflicts,
        migratedFrom = migration.applied.headOption.map(_ => migration.found),
        migrationNotes = migration.notes
      )
    )

  private def parseConfig(source: Config): (AppConfig, List[HotkeyConflict]) =
    val entries = hoconEntries(source)

    val parsed = entries.foldLeft(AppConfig.default) { (config, entry) =>
      val HoconEntry(key, value, _, raw) = entry
      // The registry knows every setting that is one key to one value, in both directions at once. Only the settings
      // that are not -- the key groups, and the spellings that set more than one field -- are still spelled out below.
      ConfigRegistry
        .find(key)
        .flatMap(field => field.readValue(config, raw))
        .getOrElse(key match
          case "editor.minimum_pane_width" | "editor.minimum.pane.width" | "editor_minimum_pane_width" =>
            value.trim.toIntOption.map(config.withMinimumPaneWidth).getOrElse(config)
          case lspKey if lspKey.startsWith("lsp.") =>
            parseLspConfigEntry(config, lspKey, value.trim)
          case hotkeyKey if hotkeyKey.startsWith("hotkey.") =>
            parseHotkeyEntry(config, hotkeyKey, value).getOrElse(config)
          case keymapKey if keymapKey.startsWith("keymap.") =>
            parseKeymapEntry(config, keymapKey, value.trim).getOrElse(config)
          case "config.version" =>
            config
          case _ =>
            config)
    }

    val scaled               = inferTextScaleMode(parsed, entries)
    val withStatus           = LegacyStatusLineKeys.applied(scaled, entries.map(entry => entry.key -> entry.value))
    val withLists            = applyHoconLists(withStatus, source)
    val withLspLists         = PreferredWindowSizeParsing.applied(applyHoconLspLists(withLists, source), source)
    val explicitCommandIds   = entries.flatMap(entry => ConfigGroups.commandIdOf(entry.key)).toSet
    val hotkeys              = withLspLists.inputConfig.hotkeyConfig.yieldingDefaultCommandBindings(explicitCommandIds)
    val (settled, conflicts) = HotkeyConflict.settle(hotkeys)
    (withLspLists.withHotkeyConfig(settled), conflicts)

  /** Generate configuration file content from AppConfig */
  def configToString(config: AppConfig): String = ConfigFileFormat.render(config)

  /** The config text to write, refused if it is not something this module could read back.
    *
    * An unparseable file is worse than a failed save: loading falls back to defaults for the whole file, so one bad
    * value silently resets every other setting the user had. That is exactly what an unquoted comma in the cursor info
    * bar's segment list used to do. Checking here keeps a formatting mistake in one setting from reaching the file at
    * all, and leaves whatever the user already had in place. A setting the file would swallow rather than reject -- a
    * key written at a path that also has children -- costs the user that one setting just as silently, so it is refused
    * on the same terms.
    */
  private[config] def renderedConfig(config: AppConfig): Either[String, String] =
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

  /** Copy the config file to a timestamped sibling (`config.conf.bak-<UTC time>`), returning where it went. See
    * [[ConfigBackup]].
    */
  def backUpConfig(path: Path, at: Instant): Option[Path] = ConfigBackup(path, at)

  /** Save configuration on the Cats Effect blocking pool with a structured failure result.
    *
    * What is already on disk is respected. A file that does not parse is left alone and the save refused, because
    * writing over it would replace the user's settings with whatever this session holds. A readable file is edited in
    * place: only the lines of the settings that changed are touched, so the user's comments, ordering, spellings,
    * unrecognised keys and even invalid values stay exactly as they wrote them. It is copied to a timestamped backup
    * first when the edit replaces an invalid value or the file comes from a newer version, and rewritten whole -- also
    * after a backup -- only when a line edit cannot express the change.
    */
  def saveConfigIO(config: AppConfig, configPath: Path): IO[Either[ConfigError, Unit]] =
    saveConfigWith(config, configPath, ConfigMigrations.installed)

  private[config] def saveConfigWith(
    config: AppConfig,
    configPath: Path,
    plan: ConfigMigrations.Plan
  ): IO[Either[ConfigError, Unit]] =
    IO.realTimeInstant.flatMap(now => IO.blocking(saveBlocking(config, configPath, now, plan)))

  private def saveBlocking(
    config: AppConfig,
    path: Path,
    now: Instant,
    plan: ConfigMigrations.Plan
  ): Either[ConfigError, Unit] =
    def failure(problem: String, cause: Option[Throwable] = None): Either[ConfigError, Unit] =
      Left(ConfigError("save", path, s"Failed to save configuration: $problem", cause))

    def write(text: String): Either[ConfigError, Unit] =
      try Right(AtomicFileWriter.writeBytesBlocking(path, text.getBytes(StandardCharsets.UTF_8)))
      catch case error: Exception => failure(error.getMessage, Some(error))

    def writeAfterBackup(text: String): Either[ConfigError, Unit] =
      backUpConfig(path, now) match
        case None => failure(s"could not back up $path first, so it was left untouched")
        case Some(backup) =>
          logger.info(s"[CONFIG] Backed up $path to $backup before changing it")
          write(text)

    renderedConfig(config) match
      case Left(problem) => failure(problem)
      case Right(text) =>
        existingConfig(path, plan) match
          case ExistingConfig.Absent => write(text)
          case ExistingConfig.Unparseable(problem) =>
            failure(
              s"$path cannot be parsed ($problem). It was left untouched so none of your settings are lost; " +
                "fix or remove it to save settings again."
            )
          case existing: ExistingConfig.Readable =>
            editInPlace(path, existing, config, plan) match
              case Edit.Unchanged => Right(())
              case Edit.Patched(patched, replacedKeys) =>
                val invalidKeys = existing.loaded.report.invalidEntries.map(entry => canonicalKey(entry.key)).toSet
                if existing.loaded.report.newerThanSupported || replacedKeys.exists(invalidKeys.contains) then
                  writeAfterBackup(patched)
                else write(patched)
              case Edit.Whole =>
                val written =
                  ConfigVersioning.stamp(text, ConfigVersioning.writtenVersion(existing.loaded.report, plan.target))
                ConfigPreservedSettings(written, existing.source).fold(failure(_), writeAfterBackup)

  private enum Edit:
    case Unchanged
    case Patched(text: String, replacedKeys: Set[String])
    case Whole

  private def canonicalKey(key: String): String = ConfigRegistry.find(key).map(_.key).getOrElse(key)

  private def editInPlace(
    path: Path,
    existing: ExistingConfig.Readable,
    config: AppConfig,
    plan: ConfigMigrations.Plan
  ): Edit =
    val before = ConfigFileFormat.settings(existing.loaded.config).map((key, value) => key -> value.rendered).toMap
    val after  = ConfigFileFormat.settings(config)
    val kept   = after.map(_._1).toSet
    def spellingsOf(key: String): Set[String] = ConfigRegistry.find(key).fold(Set(key))(_.spellings)
    val settingChanges =
      after.collect {
        case (key, value) if !before.get(key).contains(value.rendered) =>
          ConfigTextPatch.Change(key, spellingsOf(key), Some(value.rendered))
      } ++ before.keys.toList.sorted
        .filterNot(kept.contains)
        .map(key => ConfigTextPatch.Change(key, spellingsOf(key), None))

    val changes = settingChanges ++ ConfigVersioning.stampChange(existing.loaded.report, plan.target)

    def unmet(text: String, wanted: List[ConfigTextPatch.Change]): Option[List[ConfigTextPatch.Change]] =
      settingsOfText(path, text).map(loaded => wanted.filterNot(change => loaded.get(change.key) == change.value))

    if changes.isEmpty then Edit.Unchanged
    else if existing.loaded.report.migratedFrom.nonEmpty then Edit.Whole
    else
      val patched = ConfigTextPatch(existing.text, changes)
      unmet(patched.text, settingChanges) match
        case Some(Nil) => Edit.Patched(patched.text, patched.replacedKeys)
        case Some(stuck) =>
          val overridden = ConfigTextPatch.appendOverrides(patched.text, stuck)
          if unmet(overridden, settingChanges).contains(Nil) then Edit.Patched(overridden, patched.replacedKeys)
          else Edit.Whole
        case None => Edit.Whole

  /** The settings a text would load as, read the way the file will be: from a sibling file, so relative includes work.
    */
  private def settingsOfText(path: Path, text: String): Option[Map[String, String]] =
    val directory = Option(path.toAbsolutePath.getParent)
    val probe     = directory.map(dir => Files.createTempFile(dir, ".config-check", ".conf"))
    probe.flatMap { file =>
      try
        Files.writeString(file, text, StandardCharsets.UTF_8)
        Try(parseConfigResult(file).config).toOption
          .map(loaded => ConfigFileFormat.settings(loaded).map((key, value) => key -> value.rendered).toMap)
      catch case NonFatal(_) => None
      finally Files.deleteIfExists(file): Unit
    }

  private enum ExistingConfig:
    case Absent
    case Unparseable(problem: String)
    case Readable(text: String, source: Config, loaded: ConfigLoadResult)

  private def existingConfig(path: Path, plan: ConfigMigrations.Plan): ExistingConfig =
    if !Files.exists(path) then ExistingConfig.Absent
    else
      try
        val raw = parseHoconFile(path)
        ExistingConfig.Readable(
          Files.readString(path, StandardCharsets.UTF_8),
          ConfigMigrations.migrate(raw, plan).config,
          parseConfigResult(raw, plan)
        )
      catch case NonFatal(error) => ExistingConfig.Unparseable(error.getMessage)

  def saveConfigIO(config: AppConfig, configPath: String): IO[Either[ConfigError, Unit]] =
    saveConfigIO(config, Paths.get(configPath))

  private def inspectConfig(
    source: Config,
    found: ConfigVersion,
    status: ConfigVersionStatus,
    supported: ConfigVersion
  ): ConfigMigrationReport =
    val entries = hoconEntries(source)
    val deprecatedEntries = entries
      .flatMap(entry =>
        deprecatedReplacement(entry.key).map(replacement => DeprecatedConfigEntry(entry.key, replacement))
      )
      .distinctBy(_.key)
    val removedKeys = entries
      .map(_.key)
      .filter(RemovedConfigKeys.isRemoved)
      .distinct
    val unknownKeys = entries
      .map(_.key)
      .filterNot(key => isKnownConfigKey(key) || RemovedConfigKeys.isRemoved(key))
      .distinct
    val invalidEntries = entries.flatMap(entry => invalidEntry(entry.key, entry.value, entry.valueType))

    ConfigMigrationReport(
      version = found,
      versionStatus = status,
      supportedVersion = supported,
      deprecatedEntries = deprecatedEntries,
      unknownKeys = unknownKeys,
      invalidEntries = invalidEntries,
      removedKeys = removedKeys
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
    * The parse is a fold, so a broader setting has to be applied before the narrower ones that refine it.
    */
  private def hoconEntries(source: Config): List[HoconEntry] =
    source
      .entrySet()
      .asScala
      .toList
      .sortBy { entry =>
        val key = entry.getKey.stripPrefix("\"").stripSuffix("\"").toLowerCase(Locale.ROOT)
        (key.count(_ == '.'), key)
      }
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
    config.withSpellCheck(updatedSpellCheck)

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

  /** Whether an entry names a setting this can read but carries a value it cannot.
    *
    * A registered setting answers for itself: its codec is what the parser would use, so a value the codec refuses is
    * exactly a value that would be dropped. Only the settings with no single field left -- the key and LSP groups --
    * still need a rule written out here.
    */
  private def invalidEntry(
    key: String,
    value: String,
    valueType: ConfigValueType
  ): Option[InvalidConfigEntry] =
    val reason: Option[String] =
      ConfigRegistry.find(key) match
        case Some(field) if field.key == "window.preferred.width" || field.key == "window.preferred.height" =>
          val (word, default) =
            if field.key.endsWith("width") then ("width", PreferredWindowSize.Default.width)
            else ("height", PreferredWindowSize.Default.height)
          Option.when(field.codec.parse(value).isEmpty)(
            s"not a whole number of pixels, so the window $word falls back to $default and the other size is kept"
          )
        case Some(field) =>
          Option.when(field.codec.parse(value).isEmpty)(
            s"not a value this setting accepts, using its default (${field.setting(AppConfig.default)._2.rendered})"
          )
        case None =>
          key match
            case "config.version" =>
              Option.when(value.trim.toIntOption.forall(_ <= 0))(
                "not a whole number above zero, read as an unversioned file"
              )
            case key if LegacyStatusLineKeys.handles(key) =>
              Option.when(LegacyStatusLineKeys.rejects(key, value))("not a recognised value, using the default")
            case key if key.startsWith("hotkey.") || key.startsWith("keymap.") =>
              value
                .split(",")
                .toList
                .map(_.trim)
                .filter(_.nonEmpty)
                .find(HotkeyTrigger.parse(_).isEmpty)
                .map(bad => s"'$bad' is not a key binding, using the default binding")
            case key if key.startsWith("lsp.") =>
              key.split("\\.", 3).toList match
                case "lsp" :: _ :: "enabled" :: Nil =>
                  Option.when(parseBoolean(value).isEmpty)("expected true or false, using the server's default")
                case "lsp" :: _ :: "command" :: Nil =>
                  Option.when(value.trim.isEmpty)("the command is empty, using the default command")
                case "lsp" :: _ :: "args" :: Nil =>
                  Option.when(valueType != ConfigValueType.LIST && value.trim.isEmpty)(
                    "expected a list of arguments, using the default arguments"
                  )
                case _ => None
            case _ =>
              None

    reason.map(InvalidConfigEntry(key, value, _))

  /** One `hotkey.<action>` or `hotkey.command.<command id>` entry, applied over the defaults already in `config`. An
    * empty list unbinds. A name that is no action is ignored, so a file from a newer build costs only that line.
    */
  private def parseHotkeyEntry(config: AppConfig, key: String, value: String): Option[AppConfig] =
    val hotkeys  = config.inputConfig.hotkeyConfig
    val triggers = value.split(",").toList.map(_.trim).filter(_.nonEmpty).map(HotkeyTrigger.parse)
    Option
      .when(triggers.forall(_.isDefined))(triggers.flatten)
      .flatMap { parsed =>
        ConfigGroups.commandIdOf(key) match
          case Some(commandId) =>
            Some(hotkeys.copy(commandBindings = hotkeys.commandBindings + (commandId -> parsed)))
          case None =>
            val action = HotkeyAction.values.find(action => s"hotkey.${action.configKey}" == key)
            if action.isEmpty then logger.warn(s"[CONFIG] Ignoring $key: there is no hotkey action of that name")
            action.map(named => hotkeys.copy(bindings = hotkeys.bindings + (named -> parsed)))
      }
      .map(config.withHotkeyConfig)

  /** One `keymap.<group>.<action> = binding` entry, for whichever of the five focused keymap groups the key names. */
  private def parseKeymapEntry(config: AppConfig, key: String, binding: String): Option[AppConfig] =
    def bind[A <: KeymapEventAction[E], E <: com.serenity.keystroke.events.Event](
      prefix: String,
      actions: Array[A],
      group: KeymapGroup[A, E]
    ): Option[AppConfig] =
      Option
        .when(key.startsWith(prefix))(actions.find(action => s"$prefix${action.configKey}" == key))
        .flatten
        .map(action => config.withKeymapBinding(group)(action, binding))
    bind("keymap.editor.", EditorKeyAction.values, KeymapGroup.Editor)
      .orElse(bind("keymap.command_runner.", CommandRunnerKeyAction.values, KeymapGroup.CommandRunner))
      .orElse(bind("keymap.modal.", ModalKeyAction.values, KeymapGroup.Modal))
      .orElse(bind("keymap.panel.", PanelKeyAction.values, KeymapGroup.Panel))
      .orElse(bind("keymap.peek.", PeekKeyAction.values, KeymapGroup.Peek))

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
