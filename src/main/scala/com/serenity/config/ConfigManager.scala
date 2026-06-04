package com.serenity.config

import java.nio.file.{Files, Paths}

import scala.io.Source

import com.serenity.animation.AnimationConfig

/** Manages loading and saving application configuration */
object ConfigManager:

  /** Available animation presets */
  object Presets:
    val none   = AppConfig.default.withoutCharacterAnimation
    val quick  = AppConfig.default.withCharacterAnimation(AnimationConfig.quick.get)
    val smooth = AppConfig.default.withCharacterAnimation(AnimationConfig.smooth.get)
    val subtle = AppConfig.default.withCharacterAnimation(AnimationConfig.subtle.get)

  /** Load configuration from file or return default */
  def loadConfig(configPath: Option[String] = None): AppConfig =
    configPath match
      case Some(path) if Files.exists(Paths.get(path)) =>
        try parseConfig(Source.fromFile(path).mkString)
        catch
          case _: Exception =>
            System.err.println(s"[CONFIG] Failed to load config from $path, using defaults")
            AppConfig.default
      case _ => AppConfig.default

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
            case hotkeyKey if hotkeyKey.startsWith("hotkey.") =>
              HotkeyAction.values
                .find(action => s"hotkey.${action.configKey}" == hotkeyKey)
                .flatMap(action =>
                  HotkeyTrigger.parse(value.trim).map(trigger => config.withHotkeyConfig(config.hotkeyConfig.withBinding(action, trigger)))
                )
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
       |# Hotkey overrides
       |hotkey.command_palette = ${config.hotkeyConfig.bindingsFor(HotkeyAction.ToggleCommandRunner).head.render}
       |hotkey.file_search = ${config.hotkeyConfig.bindingsFor(HotkeyAction.FileSearch).head.render}
       |""".stripMargin

  /** Save configuration to file */
  def saveConfig(config: AppConfig, configPath: String): Boolean =
    try
      Files.write(Paths.get(configPath), configToString(config).getBytes)
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
                          |# Hotkey overrides
                          |hotkey.command_palette = ctrl+p
                          |hotkey.file_search = ctrl+shift+f
                          |""".stripMargin

      Files.write(Paths.get(path), sampleConfig.getBytes)
      true
    catch case _: Exception => false
