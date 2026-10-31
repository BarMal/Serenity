package com.serenity.command

import com.serenity.config.{SpellCheckConfig, SpellCheckLanguage}
import com.serenity.ui.fonts.FontLoader.{FontConfig, TextScaleMode}
import com.serenity.ui.fonts.FontSizes

/** What a font intent does to a [[FontConfig]], shared by the live settings and by a preset being edited. */
object FontConfigEdit:

  def apply(intent: FontIntent): FontConfig => FontConfig =
    intent match
      case FontIntent.IncreaseFontSize =>
        config =>
          config.copy(
            fontSize = clampFontSize(config.fontSize + 1.0f),
            textFontSize = clampFontSize(config.textFontSize + 1.0f)
          )
      case FontIntent.DecreaseFontSize =>
        config =>
          config.copy(
            fontSize = clampFontSize(config.fontSize - 1.0f),
            textFontSize = clampFontSize(config.textFontSize - 1.0f)
          )
      case FontIntent.SetFontSize(size) =>
        _.copy(fontSize = clampFontSize(size), textFontSize = clampFontSize(size))
      case FontIntent.SetCodeFontSize(size)     => _.copy(fontSize = clampFontSize(size))
      case FontIntent.SetTextFontSize(size)     => _.copy(textFontSize = clampFontSize(size))
      case FontIntent.SetUiFontSize(size)       => _.copy(uiFontSize = clampFontSize(size))
      case FontIntent.SetAllFontSizes(size)     => _.withSizes(FontSizes.all(size))
      case FontIntent.ConfirmSetAllFontSizes(_) => identity
      case FontIntent.UndoFontSizeChange =>
        config => config.previousSizes.fold(config)(previous => config.withSizes(previous).copy(previousSizes = None))
      case FontIntent.SetTextScaleMode(mode) => _.copy(textScaleMode = mode)
      case FontIntent.SetTextScaleMultiplier(scale) =>
        _.copy(textScaleMode = TextScaleMode.Manual, textScaleMultiplier = FontConfig.clampTextScale(scale))
      case FontIntent.SetCodeFontFamily(family) => _.copy(codeFontFamily = family)
      case FontIntent.SetTextFontFamily(family) => _.copy(textFontFamily = family)
      case FontIntent.SetUiFontFamily(family)   => _.copy(uiFontFamily = family)
      case FontIntent.SetLigatures(enabled)     => _.copy(enableLigatures = enabled, textLigatures = enabled)
      case FontIntent.SetCodeLigatures(enabled) => _.copy(enableLigatures = enabled)
      case FontIntent.SetTextLigatures(enabled) => _.copy(textLigatures = enabled)
      case FontIntent.SetUiLigatures(enabled)   => _.copy(uiLigatures = enabled)
      case FontIntent.ToggleLigatures =>
        config => config.copy(enableLigatures = !config.enableLigatures, textLigatures = !config.textLigatures)

  /** The live settings' edit: the same change, remembering the sizes it replaced so one level of it can be undone. A
    * preset being edited takes [[apply]] instead and remembers nothing.
    */
  def recording(intent: FontIntent): FontConfig => FontConfig =
    intent match
      case FontIntent.UndoFontSizeChange => apply(intent)
      case _ =>
        config =>
          val edited = apply(intent)(config)
          if edited.sizes == config.sizes then edited else edited.copy(previousSizes = Some(config.sizes))

  private def clampFontSize(size: Float): Float = FontSizes.clamp(size)

/** What a spell-check intent does to a [[SpellCheckConfig]]; none for the intent that acts on the cursor's word. */
object SpellCheckConfigEdit:

  def apply(intent: SpellCheckIntent): Option[SpellCheckConfig => SpellCheckConfig] =
    intent match
      case SpellCheckIntent.SetSpellCheckEnabled(enabled) => Some(_.copy(enabled = enabled))
      case SpellCheckIntent.SetSpellCheckLanguages(languages) =>
        Some(_.copy(languages = languages.map(SpellCheckLanguage.canonical)))
      case SpellCheckIntent.SetSpellCheckDictionaryPaths(paths) => Some(_.copy(dictionaryPaths = paths))
      case SpellCheckIntent.SetSpellCheckWords(words)           => Some(_.copy(additionalWords = words))
      case SpellCheckIntent.AddWordAtCursorToDictionary         => None
