package com.serenity.config

import java.awt.Color
import java.util.Locale

import com.serenity.animation.WindowSitterAction
import com.serenity.animation.sprite.{CompanionCharacter, CompanionSpriteConfig}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.TextScaleMode
import com.serenity.ui.layout.PanelPosition
import com.serenity.ui.theme.ColorFormat

/** Every setting the config file persists, declared once.
  *
  * The file writer, the file parser, the key schema and session state all read this list rather than each carrying
  * their own copy of what a setting is called and how it converts. That is the point: a setting used to be written down
  * in three or four places, and the bugs were always a place that had been missed -- ten settings the parser knew and
  * the writer did not, sixteen the config file kept and session state dropped.
  *
  * Composite settings whose shape is not one key to one value -- the motion families, the animation presets, the
  * LSP/hotkey/keymap groups -- are declared in [[ConfigGroups]] instead, and the coverage tests treat both alike.
  */
object ConfigRegistry:

  import FieldCodec.*

  /** `#RRGGBB` or `#RRGGBBAA`, which is what [[ColorFormat.toHex]] writes. */
  private def colorFromHex(value: String): Option[Color] =
    val hex = value.trim.stripPrefix("#")
    Option
      .when(hex.length == 6 || hex.length == 8)(hex)
      .filter(_.forall(character => Character.digit(character, 16) >= 0))
      .flatMap { normalized =>
        scala.util.Try {
          val red   = Integer.parseInt(normalized.substring(0, 2), 16)
          val green = Integer.parseInt(normalized.substring(2, 4), 16)
          val blue  = Integer.parseInt(normalized.substring(4, 6), 16)
          val alpha = if normalized.length == 8 then Integer.parseInt(normalized.substring(6, 8), 16) else 255
          Color(red, green, blue, alpha)
        }.toOption
      }

  private def colorToHex(value: Color): String = ColorFormat.toHex(value, withAlpha = true)

  private val color: FieldCodec[Color] =
    given io.circe.Encoder[Color] = io.circe.Encoder.encodeString.contramap(colorToHex)
    given io.circe.Decoder[Color] =
      io.circe.Decoder.decodeString.emap(text => colorFromHex(text).toRight(s"Not a colour: $text"))
    FieldCodec.of(colorFromHex, value => HoconValue.string(colorToHex(value)))

  /** The segment list, which also accepts the older single-word presets (`minimal`, `detailed`) it replaced. */
  private val infoBarSegments: FieldCodec[List[CursorInfoBarSegment]] =
    given io.circe.Encoder[List[CursorInfoBarSegment]] =
      io.circe.Encoder.encodeList(using io.circe.Encoder.encodeString.contramap(_.configKey))
    given io.circe.Decoder[List[CursorInfoBarSegment]] =
      io.circe.Decoder.decodeList(using
        io.circe.Decoder.decodeString.emap(key =>
          CursorInfoBarSegment
            .fromConfigKey(key)
            .orElse(CursorInfoBarSegment.values.find(_.toString == key))
            .toRight(s"Unknown cursor info bar segment: $key")
        )
      )
    FieldCodec.of(
      CursorInfoBarSegment.parseList,
      values => HoconValue.string(if values.isEmpty then "off" else values.map(_.configKey).mkString(","))
    )

  /** Font sizes are clamped rather than refused: a file asking for 400pt is a file that means "as big as you allow". */
  private val fontSize: FieldCodec[Float] =
    FieldCodec.of(text => text.trim.toFloatOption.map(size => size.max(8.0f).min(48.0f)), HoconValue.number)

  private val textScaleMode: FieldCodec[TextScaleMode] =
    enumerated(
      text =>
        text.toLowerCase(Locale.ROOT) match
          case "auto"                      => Some(TextScaleMode.Auto)
          case "manual" | "custom"         => Some(TextScaleMode.Manual)
          case "off" | "none" | "disabled" => Some(TextScaleMode.Off)
          case _                           => None
      ,
      _.configKey
    )

  private[config] def field[A](key: String, aliases: String*)(codec: FieldCodec[A])(
    get: AppConfig => A,
    set: (AppConfig, A) => AppConfig
  ): ConfigField[A] = ConfigField(key, aliases.toSet, codec, get, set)

  private[config] def named[A](key: String, jsonKey: String, aliases: String*)(codec: FieldCodec[A])(
    get: AppConfig => A,
    set: (AppConfig, A) => AppConfig
  ): ConfigField[A] = ConfigField(key, aliases.toSet, codec, get, set, Some(jsonKey))

  /** For a setting whose setter adjusts a neighbour: putting back what was saved should touch only the field itself. */
  extension [A](configField: ConfigField[A])
    private[config] def restoredBy(assign: (AppConfig, A) => AppConfig): ConfigField[A] =
      configField.copy(restore = Some(assign))

  val fields: List[ConfigField[?]] = List(
    // -- Language tools ------------------------------------------------------------------------------------------
    named("syntax.highlighting", "syntaxHighlightingEnabled", "syntax_highlighting")(boolean)(
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

    // -- Fonts ---------------------------------------------------------------------------------------------------
    field("font.code.family", "font_code_family")(string)(
      _.editorConfig.fontConfig.codeFontFamily,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(codeFontFamily = value))
    ),
    field("font.text.family", "font_text_family")(string)(
      _.editorConfig.fontConfig.textFontFamily,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(textFontFamily = value))
    ),
    field("font.ui.family", "font_ui_family")(string)(
      _.editorConfig.fontConfig.uiFontFamily,
      (config, value) =>
        // `${font.text.family}` is a substitution the old writer emitted rather than a family anyone has.
        val family = if value == "${font.text.family}" then config.editorConfig.fontConfig.textFontFamily else value
        config.withFontConfig(config.editorConfig.fontConfig.copy(uiFontFamily = family))
    ),
    field("font.code.size", "font_code_size")(fontSize)(
      _.editorConfig.fontConfig.codeFontSize,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(fontSize = value))
    ),
    field("font.text.size", "font.prose.size", "font_text_size", "font_prose_size")(fontSize)(
      _.editorConfig.fontConfig.textFontSize,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(textFontSize = value))
    ),
    field("font.ui.size", "font_ui_size")(fontSize)(
      _.editorConfig.fontConfig.uiFontSize,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(uiFontSize = value))
    ),
    field("font.scale.mode", "font_scale_mode")(textScaleMode)(
      _.editorConfig.fontConfig.textScaleMode,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(textScaleMode = value))
    ),
    field("font.text_scale", "font.text.scale", "font_text_scale")(
      double.filtered(scale =>
        scale >= FontLoader.FontConfig.MinTextScale && scale <= FontLoader.FontConfig.MaxTextScale
      )
    )(
      _.editorConfig.fontConfig.textScaleMultiplier,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(textScaleMultiplier = value))
    ),
    field("font.code.ligatures", "font_code_ligatures")(boolean)(
      _.editorConfig.fontConfig.codeLigatures,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(enableLigatures = value))
    ),
    field("font.text.ligatures", "font.prose.ligatures", "font_text_ligatures", "font_prose_ligatures")(boolean)(
      _.editorConfig.fontConfig.textLigatures,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(textLigatures = value))
    ),
    field("font.ui.ligatures", "font_ui_ligatures")(boolean)(
      _.editorConfig.fontConfig.uiLigatures,
      (config, value) => config.withFontConfig(config.editorConfig.fontConfig.copy(uiLigatures = value))
    ),

    // -- Cursor --------------------------------------------------------------------------------------------------
    named("cursor.mode", "cursorMode", "cursor_mode")(
      enumerated(CursorMode.fromConfigKey, _.configKey, text => CursorMode.values.find(_.toString == text))
    )(
      _.cursorMode,
      (config, value) => config.withCursorMode(value)
    ),
    named("cursor.active.color", "cursorActiveColor", "cursor_active_color")(color.orEmpty)(
      _.cursorColors.active,
      (config, value) => config.withCursorColors(config.cursorColors.copy(active = value))
    ),
    named("cursor.inactive.color", "cursorInactiveColor", "cursor_inactive_color")(color.orEmpty)(
      _.cursorColors.inactive,
      (config, value) => config.withCursorColors(config.cursorColors.copy(inactive = value))
    ),
    // #1295: independent of the active theme -- `None` (default) keeps the theme's own panel colour for the cursor
    // info bar, matching every other floating panel.
    named("cursor.info_bar.foreground_color", "cursor_info_bar_foreground_color")(color.orEmpty)(
      _.cursorInfoBarColors.foreground,
      (config, value) => config.withCursorInfoBarColors(config.cursorInfoBarColors.copy(foreground = value))
    ),
    named("cursor.info_bar.background_color", "cursor_info_bar_background_color")(color.orEmpty)(
      _.cursorInfoBarColors.background,
      (config, value) => config.withCursorInfoBarColors(config.cursorInfoBarColors.copy(background = value))
    ),
    named(
      "cursor.info_bar.segments",
      "cursorInfoBarSegments",
      "cursor.info_bar",
      "cursor.info.bar",
      "cursor_info_bar",
      "cursor.info.bar.segments"
    )(infoBarSegments)(
      _.cursorInfoBarSegments,
      (config, value) => config.withCursorInfoBarSegments(value)
    ),
    named(
      "cursor.info_bar.placement",
      "cursorInfoBarPlacement",
      "cursor.info.bar.placement",
      "cursor_info_bar_placement"
    )(
      enumerated(
        CursorInfoBarPlacement.fromConfigKey,
        _.configKey,
        text => CursorInfoBarPlacement.values.find(_.toString == text)
      )
    )(
      _.cursorInfoBarPlacement,
      (config, value) => config.withCursorInfoBarPlacement(value)
    ),

    // -- Interface -----------------------------------------------------------------------------------------------
    named("interface.density", "interfaceDensity", "interface_density")(
      enumerated(InterfaceDensity.fromConfigKey, _.configKey, text => InterfaceDensity.values.find(_.toString == text))
    )(_.interfaceDensity, (config, value) => config.withInterfaceDensity(value)),
    named("ui.element_gap", "uiElementGap", "ui.element.gap", "ui_element_gap")(
      double.filtered(gap => gap.isFinite && gap >= AppConfig.MinUiElementGap && gap <= AppConfig.MaxUiElementGap)
    )(_.uiElementGap, (config, value) => config.withUiElementGap(value)),
    named("ui.corner_radius", "uiCornerRadiusPx", "ui.corner.radius", "ui_corner_radius")(
      int.filtered(radius => radius >= AppConfig.MinUiCornerRadiusPx && radius <= AppConfig.MaxUiCornerRadiusPx)
    )(_.uiCornerRadiusPx, (config, value) => config.withUiCornerRadiusPx(value)),
    named("ui.outline_thickness", "uiOutlineThicknessPx", "ui.outline.thickness", "ui_outline_thickness")(
      int.filtered(thickness =>
        thickness >= AppConfig.MinUiOutlineThicknessPx && thickness <= AppConfig.MaxUiOutlineThicknessPx
      )
    )(_.uiOutlineThicknessPx, (config, value) => config.withUiOutlineThicknessPx(value)),

    // -- Window --------------------------------------------------------------------------------------------------
    named("window.chrome", "windowChromeMode", "window.chrome.mode", "window_chrome", "window_chrome_mode")(
      enumerated(WindowChromeMode.fromConfigKey, _.configKey, text => WindowChromeMode.values.find(_.toString == text))
    )(_.windowChromeMode, (config, value) => config.withWindowChromeMode(value)),
    field("window.sitter.enabled")(boolean)(
      _.windowSitterConfig.enabled,
      (config, value) => config.withWindowSitterConfig(config.windowSitterConfig.copy(enabled = value))
    ),
    field("window.sitter.action")(
      enumerated(
        WindowSitterAction.fromConfigKey,
        _.configKey,
        text => WindowSitterAction.values.find(_.toString == text)
      )
    )(
      _.windowSitterConfig.action,
      (config, value) => config.withWindowSitterConfig(config.windowSitterConfig.copy(action = value))
    ),
    field("window.sitter.frames")(stringList)(
      _.windowSitterConfig.frames.toList,
      (config, value) => config.withWindowSitterConfig(config.windowSitterConfig.copy(frames = value.toVector))
    ),
    field("window.sitter.active_ticks")(int)(
      _.windowSitterConfig.activeTicks,
      (config, value) => config.withWindowSitterConfig(config.windowSitterConfig.copy(activeTicks = value))
    ),
    field("window.sitter.fast_active_ticks")(int)(
      _.windowSitterConfig.fastActiveTicks,
      (config, value) => config.withWindowSitterConfig(config.windowSitterConfig.copy(fastActiveTicks = value))
    ),
    field("window.sitter.fast_typing_threshold_ms")(int)(
      _.windowSitterConfig.fastTypingThresholdMs,
      (config, value) => config.withWindowSitterConfig(config.windowSitterConfig.copy(fastTypingThresholdMs = value))
    ),

    // -- Companion sprite ------------------------------------------------------------------------------------------
    field("companion.sprite.enabled")(boolean)(
      _.companionSpriteConfig.enabled,
      (config, value) => config.withCompanionSpriteConfig(config.companionSpriteConfig.copy(enabled = value))
    ),
    field("companion.sprite.character")(
      enumerated(CompanionCharacter.fromConfigKey, _.id)
    )(
      _.companionSpriteConfig.character,
      (config, value) => config.withCompanionSpriteConfig(config.companionSpriteConfig.copy(character = value))
    ),
    field("companion.sprite.position")(
      enumeratedValues(PanelPosition.values, _.toString.toLowerCase(Locale.ROOT))
    )(
      _.companionSpriteConfig.position,
      (config, value) => config.withCompanionSpriteConfig(config.companionSpriteConfig.copy(position = value))
    ),
    field("companion.sprite.size")(
      int.filtered(size => size >= CompanionSpriteConfig.MinSize && size <= CompanionSpriteConfig.MaxSize)
    )(
      _.companionSpriteConfig.size,
      (config, value) => config.withCompanionSpriteConfig(config.companionSpriteConfig.copy(size = value))
    ),
    field("visual.flair.level")(
      enumerated(VisualFlairLevel.fromConfigKey, _.configKey)
    )(_.visualFlairLevel, (config, value) => config.withVisualFlairLevel(value)),
    named("window.preferred.width", "preferredWindowWidth", "window_preferred_width")(int.orEmpty)(
      _.preferredWindowSize.map(_.width),
      (config, value) =>
        value.fold(config)(width =>
          config.withPreferredWindowSize(
            config.preferredWindowSize.getOrElse(PreferredWindowSize(width, 768)).copy(width = width)
          )
        )
    ),
    named("window.preferred.height", "preferredWindowHeight", "window_preferred_height")(int.orEmpty)(
      _.preferredWindowSize.map(_.height),
      (config, value) =>
        value.fold(config)(height =>
          config.withPreferredWindowSize(
            config.preferredWindowSize.getOrElse(PreferredWindowSize(1024, height)).copy(height = height)
          )
        )
    ),

    // -- Documents and editor ------------------------------------------------------------------------------------
    named("document.markdown_view", "markdownViewMode", "document.markdown.view", "document_markdown_view")(
      enumerated(MarkdownViewMode.fromConfigKey, _.configKey, text => MarkdownViewMode.values.find(_.toString == text))
    )(_.markdownViewMode, (config, value) => config.withMarkdownViewMode(value)),
    named("document.default_mode", "defaultDocumentMode", "document.default.mode", "document_default_mode")(
      enumerated(
        DefaultDocumentMode.fromConfigKey,
        _.configKey,
        text => DefaultDocumentMode.values.find(_.toString == text)
      )
    )(_.defaultDocumentMode, (config, value) => config.withDefaultDocumentMode(value)),
    named("app.mode", "appMode")(
      enumerated(AppMode.fromConfigKey, _.configKey, text => AppMode.values.find(_.toString == text))
    )(_.appMode, (config, value) => config.withAppMode(value)),
    field("app.show_all_settings", "app_show_all_settings")(boolean)(
      _.showAllSettingsRegardlessOfMode,
      (config, value) => config.withShowAllSettingsRegardlessOfMode(value)
    ),
    named("widget.mode_tab_corner", "modeTabWidgetCornerPosition", "widget_mode_tab_corner")(
      enumerated(CornerPosition.fromConfigKey, _.configKey, text => CornerPosition.values.find(_.toString == text))
    )(_.modeTabWidgetCornerPosition, (config, value) => config.withModeTabWidgetCornerPosition(value)),
    named("editor.minimum_pane_width", "minimumPaneWidth", "editor.minimum.pane.width", "editor_minimum_pane_width")(
      int
    )(_.editorConfig.minimumPaneWidth, (config, value) => config.withMinimumPaneWidth(value)),
    named("input.wheel_scroll_lines", "wheelScrollLines", "input_wheel_scroll_lines")(int)(
      _.inputConfig.wheelScrollLines,
      (config, value) => config.withWheelScrollLines(value)
    )
  ) ++ ConfigRegistrySurfaceFields.fields

  private val byKey: Map[String, ConfigField[?]] =
    fields.flatMap(configField => configField.spellings.map(_ -> configField)).toMap

  def find(key: String): Option[ConfigField[?]] = byKey.get(key)

  /** Apply one key's value, as reading a config file does. `None` when the key is unknown or the value unusable. */
  def read(config: AppConfig, key: String, value: String): Option[AppConfig] =
    find(key).flatMap(_.read(config, value))

  /** Whether a registered key would reject this value. An unknown key is not this function's business, so it says no.
    */
  def rejects(key: String, value: String): Boolean =
    find(key).exists(_.codec.parse(value).isEmpty)

  val writtenKeys: List[String] = fields.map(_.key)

  val allKeys: Set[String] = byKey.keySet

  /** The order settings are applied in when a whole config is read at once: broader paths before narrower ones, then
    * alphabetically. It is the order the config file is folded in, and it matters because a handful of setters
    * deliberately adjust a neighbouring setting -- a custom blur radius switches the material preset to custom, and the
    * preset's own saved value has to come after that to have the last word.
    */
  /** Every setting's default value, in the order the file writes them.
    *
    * The literals still sit in the config case classes' constructors, but this is where to read them: one list, keyed
    * the way the config file is, rather than spread over parameter lists and an override block that restates some of
    * them. `docs/default-config.conf` is generated from it, so a change to any default shows up as a diff there.
    */
  lazy val defaults: List[(String, HoconValue)] = fields.map(_.setting(AppConfig.default))

  def defaultFor(key: String): Option[HoconValue] = find(key).map(_.setting(AppConfig.default)._2)

  /** Put one setting back the way it ships, leaving every other setting alone. */
  def resetToDefault(config: AppConfig, key: String): Option[AppConfig] =
    find(key).map(_.restoreDefault(config, AppConfig.default))

  val readOrder: List[ConfigField[?]] =
    fields.sortBy(field => (field.key.count(_ == '.'), field.key))
