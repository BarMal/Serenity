package com.serenity.config

import com.serenity.ui.fonts.FontLoader

/** Language tools and typography: syntax highlighting, spell check, fonts. */
private[config] object ConfigFieldsLanguageAndTypography:

  import ConfigFieldSyntax.*
  import FieldCodec.*

  val fields: List[ConfigField[?]] = List(
    // -- Language tools --------------------------------------------------------------------------------------------------
    named("editor.syntax_highlighting", "syntaxHighlightingEnabled", "syntax.highlighting", "syntax_highlighting")(
      boolean
    )(
      _.languageToolsConfig.syntaxHighlightingEnabled,
      (config, value) => config.withSyntaxHighlighting(value)
    ),
    field("spellcheck.enabled", "spellcheck_enabled")(boolean)(
      _.languageToolsConfig.spellCheck.enabled,
      (config, value) => config.withSpellCheck(config.languageToolsConfig.spellCheck.copy(enabled = value))
    ),
    field("spellcheck.languages", "spellcheck_languages")(stringList)(
      _.languageToolsConfig.spellCheck.normalized.languages,
      (config, value) =>
        config.withSpellCheck(config.languageToolsConfig.spellCheck.copy(languages = value.map(_.toLowerCase)))
    ),
    field("spellcheck.dictionary_paths", "spellcheck.dictionary.paths", "spellcheck_dictionary_paths")(stringList)(
      _.languageToolsConfig.spellCheck.normalized.dictionaryPaths,
      (config, value) => config.withSpellCheck(config.languageToolsConfig.spellCheck.copy(dictionaryPaths = value))
    ),
    field("spellcheck.words", "spellcheck_words")(stringList)(
      _.languageToolsConfig.spellCheck.normalized.additionalWords,
      (config, value) =>
        config.withSpellCheck(config.languageToolsConfig.spellCheck.copy(additionalWords = value.map(_.toLowerCase)))
    ),

    // -- Fonts -----------------------------------------------------------------------------------------------------------
    field("typography.code.family", "font.code.family", "font_code_family")(string)(
      _.editorConfig.fontConfig.codeFontFamily,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(codeFontFamily = value))
    ),
    field("typography.prose.family", "font.text.family", "font_text_family")(string)(
      _.editorConfig.fontConfig.textFontFamily,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(textFontFamily = value))
    ),
    field("typography.ui.family", "font.ui.family", "font_ui_family")(string)(
      _.editorConfig.fontConfig.uiFontFamily,
      (config, value) =>
        // `${font.text.family}` is a substitution the old writer emitted rather than a family anyone has.
        val family = if value == "${font.text.family}" then config.editorConfig.fontConfig.textFontFamily else value
        config.withFontConfig(config.editorConfig.fontConfig.copy(uiFontFamily = family))
    ),
    field("typography.code.size", "font.code.size", "font_code_size")(fontSize)(
      _.editorConfig.fontConfig.codeFontSize,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(fontSize = value))
    ),
    field("typography.prose.size", "font.text.size", "font.prose.size", "font_text_size", "font_prose_size")(fontSize)(
      _.editorConfig.fontConfig.textFontSize,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(textFontSize = value))
    ),
    field("typography.ui.size", "font.ui.size", "font_ui_size")(fontSize)(
      _.editorConfig.fontConfig.uiFontSize,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(uiFontSize = value))
    ),
    field("typography.scale.mode", "font.scale.mode", "font_scale_mode")(textScaleMode)(
      _.editorConfig.fontConfig.textScaleMode,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(textScaleMode = value))
    ),
    field("typography.scale.factor", "font.text_scale", "font.text.scale", "font_text_scale")(
      double.filtered(scale =>
        scale >= FontLoader.FontConfig.MinTextScale && scale <= FontLoader.FontConfig.MaxTextScale
      )
    )(
      _.editorConfig.fontConfig.textScaleMultiplier,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(textScaleMultiplier = value))
    ),
    field("typography.code.ligatures", "font.code.ligatures", "font_code_ligatures")(boolean)(
      _.editorConfig.fontConfig.codeLigatures,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(enableLigatures = value))
    ),
    field(
      "typography.prose.ligatures",
      "font.text.ligatures",
      "font.prose.ligatures",
      "font_text_ligatures",
      "font_prose_ligatures"
    )(boolean)(
      _.editorConfig.fontConfig.textLigatures,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(textLigatures = value))
    ),
    field("typography.ui.ligatures", "font.ui.ligatures", "font_ui_ligatures")(boolean)(
      _.editorConfig.fontConfig.uiLigatures,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(uiLigatures = value))
    )
  )
