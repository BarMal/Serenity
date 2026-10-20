package com.serenity.command

import com.serenity.ui.presets.UiPreset

/** Applies a settings intent to a stored preset instead of the live config, through the preset's own [[UiPreset.Patch]]
  * so only the area the intent belongs to changes. A preset holds cursor, font, document-default and spell-check
  * settings; its panel layout is only ever captured whole (Overwrite Preset).
  */
object PresetSettingsEdit:

  /** `preset` with `intent` applied, or none when `intent` is not a setting a preset can hold. */
  def apply(intent: CommandIntent, preset: UiPreset): Option[UiPreset] =
    val config = preset.config
    intent match
      case CommandIntent.Settings(SettingsIntent.Cursor(CursorIntent.SetCursorMode(mode))) =>
        Some(UiPreset.Patch.Appearance(config.withCursorMode(mode)).applyTo(preset))
      case CommandIntent.Settings(SettingsIntent.Font(font)) =>
        val edited = FontConfigEdit(font)(config.editorConfig.fontConfig)
        Some(UiPreset.Patch.Typography(config.withFontConfig(edited)).applyTo(preset))
      case CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetDropCapsEnabled(enabled))) =>
        Some(UiPreset.Patch.DocumentDefaults(config.withDropCapsEnabled(enabled)).applyTo(preset))
      case CommandIntent.View(ViewIntent.SetDefaultDocumentMode(mode)) =>
        Some(UiPreset.Patch.DocumentDefaults(config.withDefaultDocumentMode(mode)).applyTo(preset))
      case CommandIntent.View(ViewIntent.SetMarkdownViewMode(mode)) =>
        Some(UiPreset.Patch.DocumentDefaults(config.withMarkdownViewMode(mode)).applyTo(preset))
      case CommandIntent.Settings(SettingsIntent.SpellCheck(spellCheck)) =>
        SpellCheckConfigEdit(spellCheck).map { edit =>
          UiPreset.Patch
            .LanguageTools(config.withSpellCheck(edit(config.languageToolsConfig.spellCheck)))
            .applyTo(preset)
        }
      case _ => None
