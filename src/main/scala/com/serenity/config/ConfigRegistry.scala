package com.serenity.config

import java.awt.Color
import java.util.Locale

import com.serenity.animation.WindowSitterAction
import com.serenity.keystroke.Modifier
import com.serenity.state.models.SurfacePlacement
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.TextScaleMode
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

  /** A percentage in the file, a fraction in the config. The rounding matters: reading `17.3` back as a raw division
    * gives 0.17299999999999996, which then writes out as a different number than the one that was saved.
    */
  private def fractionOfPercent(value: Double): Double =
    BigDecimal(value / 100.0).setScale(9, BigDecimal.RoundingMode.HALF_UP).toDouble

  private val insetPercent: FieldCodec[Double] =
    double.filtered(percent => percent >= 0.0 && percent <= TextAreaInsets.MaxInset * 100.0)

  private val viewportPercent: FieldCodec[Double] =
    double.filtered(percent =>
      percent >= ViewportAxisSizing.MinPercent * 100.0 && percent <= ViewportAxisSizing.MaxPercent * 100.0
    )

  double.filtered(scale =>
    scale >= AppConfig.MinElementTransitionSpeedScale && scale <= AppConfig.MaxElementTransitionSpeedScale
  )

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

  private def lowercased[A](values: Array[A]): FieldCodec[A] =
    enumerated(
      text => values.find(_.toString.equalsIgnoreCase(text.replace("-", ""))),
      value => value.toString.toLowerCase(Locale.ROOT)
    )

  /** Font sizes are clamped rather than refused: a file asking for 400pt is a file that means "as big as you allow". */
  /** The segment list, which also accepts the older single-word presets (`minimal`, `detailed`) it replaced. */
  private val infoBarSegments: FieldCodec[List[CursorInfoBarSegment]] =
    given io.circe.Encoder[List[CursorInfoBarSegment]] =
      io.circe.Encoder.encodeList(io.circe.Encoder.encodeString.contramap(_.configKey))
    given io.circe.Decoder[List[CursorInfoBarSegment]] =
      io.circe.Decoder.decodeList(
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

  private val materialPreset: FieldCodec[MaterialPreset] =
    enumerated(
      text =>
        text.toLowerCase(Locale.ROOT) match
          case "solid" | "opaque"      => Some(MaterialPreset.Solid)
          case "clear" | "transparent" => Some(MaterialPreset.Clear)
          case "frosted" | "soft"      => Some(MaterialPreset.Frosted)
          case "crystal" | "glass"     => Some(MaterialPreset.Crystal)
          case "custom"                => Some(MaterialPreset.Custom)
          case _                       => None
      ,
      _.configKey
    )

  private def field[A](key: String, aliases: String*)(codec: FieldCodec[A])(
    get: AppConfig => A,
    set: (AppConfig, A) => AppConfig
  ): ConfigField[A] = ConfigField(key, aliases.toSet, codec, get, set)

  private def named[A](key: String, jsonKey: String, aliases: String*)(codec: FieldCodec[A])(
    get: AppConfig => A,
    set: (AppConfig, A) => AppConfig
  ): ConfigField[A] = ConfigField(key, aliases.toSet, codec, get, set, Some(jsonKey))

  /** For a setting whose setter adjusts a neighbour: putting back what was saved should touch only the field itself. */
  extension [A](configField: ConfigField[A])
    private def restoredBy(assign: (AppConfig, A) => AppConfig): ConfigField[A] =
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

    // -- Command runner ------------------------------------------------------------------------------------------
    field("command_runner.visible_rows", "command.runner.visible.rows", "command_runner_visible_rows")(
      int
        .filtered(rows =>
          rows >= AppConfig.MinCommandRunnerVisibleRows && rows <= AppConfig.MaxCommandRunnerVisibleRows
        )
        .orAuto
    )(_.surfaceConfig.commandRunnerVisibleRows, (config, value) => config.withCommandRunnerVisibleRows(value)),
    field("command_runner.item_gap_rows", "command.runner.item.gap.rows", "command_runner_item_gap_rows")(
      double.filtered(rows =>
        rows >= AppConfig.MinCommandRunnerItemGapRows && rows <= AppConfig.MaxCommandRunnerItemGapRows
      )
    )(_.surfaceConfig.commandRunnerItemGapRows, (config, value) => config.withCommandRunnerItemGapRows(value)),
    field("command_runner.cursor_gap_rows", "command.runner.cursor.gap.rows", "command_runner_cursor_gap_rows")(
      double
        .filtered(rows =>
          rows >= AppConfig.MinCommandRunnerCursorGapRows && rows <= AppConfig.MaxCommandRunnerCursorGapRows
        )
        .orAuto
    )(_.surfaceConfig.commandRunnerCursorGapRows, (config, value) => config.withCommandRunnerCursorGapRows(value)),
    field("command_runner.show_key_hints", "command.runner.show.key.hints", "command_runner_show_key_hints")(boolean)(
      _.surfaceConfig.commandRunnerShowKeyHints,
      (config, value) => config.withCommandRunnerShowKeyHints(value)
    ),
    field(
      "command_runner.cursor_peek.enabled",
      "command.runner.cursor.peek.enabled",
      "command_runner.cursor_peek",
      "command.runner.cursor.peek",
      "command_runner_cursor_peek"
    )(boolean)(
      _.surfaceConfig.commandRunnerCursorPeekEnabled,
      (config, value) => config.withCommandRunnerCursorPeekEnabled(value)
    ),
    field("command_runner.cursor_peek.modifier", "command.runner.cursor.peek.modifier")(lowercased(Modifier.values))(
      _.surfaceConfig.commandRunnerCursorPeekModifier,
      (config, value) => config.withCommandRunnerCursorPeekModifier(value)
    ),
    field("command_runner.cursor_peek.tap_window_ms", "command.runner.cursor.peek.tap.window.ms")(long)(
      _.surfaceConfig.commandRunnerCursorPeekTapWindowMillis,
      (config, value) => config.withCommandRunnerCursorPeekTapWindowMillis(value)
    ),
    field("command_runner.cursor_peek.placement", "command.runner.cursor.peek.placement")(
      lowercased(SurfacePlacement.values)
    )(
      _.surfaceConfig.commandRunnerCursorPeekPlacement,
      (config, value) => config.withCommandRunnerCursorPeekPlacement(value)
    ),

    // -- Rendering and display -----------------------------------------------------------------------------------
    named("render.fps", "renderFpsTarget", "render_fps", "ui.render.fps", "ui_render_fps")(
      enumerated(RenderFpsTarget.fromConfigKey, _.configKey, text => RenderFpsTarget.values.find(_.toString == text))
    )(_.surfaceConfig.renderFpsTarget, (config, value) => config.withRenderFpsTarget(value)),
    named(
      "render.damage_granularity",
      "renderDamageGranularity",
      "render.damage.granularity",
      "render_damage_granularity"
    )(
      enumerated(
        RenderDamageGranularity.fromConfigKey,
        _.configKey,
        text => RenderDamageGranularity.values.find(_.toString == text)
      )
    )(
      _.surfaceConfig.renderDamageGranularity,
      (config, value) => config.withRenderDamageGranularity(value)
    ),
    field(
      "display.cursor_info_bar_background_alpha",
      "display.cursor_info_bar.background_alpha",
      "display_cursor_info_bar_background_alpha"
    )(
      double
        .filtered(alpha =>
          alpha >= AppConfig.MinCursorInfoBarBackgroundAlpha && alpha <= AppConfig.MaxCursorInfoBarBackgroundAlpha
        )
        .orAuto
    )(_.surfaceConfig.cursorInfoBarBackgroundAlpha, (config, value) => config.withCursorInfoBarBackgroundAlpha(value)),
    named("display.word_wrap", "wordWrapEnabled", "display.word.wrap", "display_word_wrap")(boolean)(
      _.surfaceConfig.wordWrapEnabled,
      (config, value) => config.withWordWrap(value)
    ),
    named(
      "display.visual_line_navigation",
      "visualLineCursorNavigation",
      "display.visual.line.navigation",
      "display_visual_line_navigation"
    )(boolean)(
      _.surfaceConfig.visualLineCursorNavigation,
      (config, value) => config.withVisualLineCursorNavigation(value)
    ),
    named(
      "display.typewriter_scrolling",
      "typewriterScrollingEnabled",
      "display.typewriter.scrolling",
      "display_typewriter_scrolling"
    )(boolean)(
      _.surfaceConfig.typewriterScrollingEnabled,
      (config, value) => config.withTypewriterScrolling(value)
    ),
    named("display.line_numbers", "showLineNumbers", "display.line.numbers", "display_line_numbers")(boolean)(
      _.surfaceConfig.showLineNumbers,
      (config, value) => config.withLineNumbers(value)
    ),
    named("display.gutter", "showGutter", "display_gutter")(boolean)(
      _.surfaceConfig.showGutter,
      (config, value) => config.withGutter(value)
    ),
    named("display.word_count", "showWordCount", "display.word.count", "display_word_count")(boolean)(
      _.surfaceConfig.showWordCount,
      (config, value) => config.withWordCount(value)
    ),
    field("display.comments", "display_comments")(
      enumerated(
        CommentDisplayMode.fromConfigKey,
        _.configKey,
        text => CommentDisplayMode.values.find(_.toString == text)
      )
    )(
      _.surfaceConfig.commentDisplayMode,
      (config, value) => config.withCommentDisplayMode(value)
    ),
    named("display.pane_headers", "showPaneHeaders", "display.pane.headers", "display_pane_headers")(boolean)(
      _.surfaceConfig.showPaneHeaders,
      (config, value) => config.withPaneHeaders(value)
    ),
    named(
      "display.focused_text_body",
      "focusedTextBodyEnabled",
      "display.focused.text.body",
      "display_focused_text_body"
    )(boolean)(_.surfaceConfig.focusedTextBodyEnabled, (config, value) => config.withFocusedTextBody(value)),
    named(
      "display.contextual_toolbar",
      "contextualToolbarEnabled",
      "display.contextual.toolbar",
      "display_contextual_toolbar"
    )(boolean)(_.surfaceConfig.contextualToolbarEnabled, (config, value) => config.withContextualToolbarEnabled(value)),
    named(
      "display.contextual_toolbar_mode",
      "contextualToolbarDisplayMode",
      "display.contextual.toolbar.mode",
      "display_contextual_toolbar_mode"
    )(
      enumerated(
        ToolbarDisplayMode.fromConfigKey,
        _.configKey,
        text => ToolbarDisplayMode.values.find(_.toString == text)
      )
    )(
      _.surfaceConfig.contextualToolbarDisplayMode,
      (config, value) => config.withContextualToolbarDisplayMode(value)
    ),

    // -- Material and background ---------------------------------------------------------------------------------
    named("ui.material", "materialPreset", "ui_material", "material.preset", "material_preset")(materialPreset)(
      _.surfaceConfig.materialPreset,
      (config, value) => config.withMaterialPreset(value)
    ).restoredBy((config, value) => config.withSurfaceConfig(config.surfaceConfig.copy(materialPreset = value))),
    named("ui.post_processing", "postProcessingEffect")(
      enumerated(
        PostProcessingEffect.fromConfigKey,
        _.configKey,
        text => PostProcessingEffect.values.find(_.toString == text)
      )
    )(
      _.surfaceConfig.postProcessingEffect,
      (config, value) => config.withPostProcessingEffect(value)
    ),
    named("ui.shadows", "uiShadowsEnabled", "ui_shadows")(boolean)(
      _.surfaceConfig.uiShadowsEnabled,
      (config, value) => config.withUiShadowsEnabled(value)
    ),
    named("ui.background_style", "backgroundStyle", "ui.background.style", "ui_background_style")(
      enumerated(BackgroundStyle.fromConfigKey, _.configKey, text => BackgroundStyle.values.find(_.toString == text))
    )(_.surfaceConfig.backgroundStyle, (config, value) => config.withBackgroundStyle(value))
      .restoredBy((config, value) => config.withSurfaceConfig(config.surfaceConfig.copy(backgroundStyle = value))),
    named("ui.blur_radius", "blurRadius", "ui.blur.radius", "ui_blur_radius")(
      float.filtered(radius => radius >= 0.0f && radius <= 1.0f)
    )(_.surfaceConfig.blurRadius, (config, value) => config.withBlurRadius(value))
      .restoredBy((config, value) => config.withSurfaceConfig(config.surfaceConfig.copy(blurRadius = value))),

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
    named("editor.minimum_pane_width", "minimumPaneWidth", "editor.minimum.pane.width", "editor_minimum_pane_width")(
      int
    )(_.editorConfig.minimumPaneWidth, (config, value) => config.withMinimumPaneWidth(value)),
    named("input.wheel_scroll_lines", "wheelScrollLines", "input_wheel_scroll_lines")(int)(
      _.inputConfig.wheelScrollLines,
      (config, value) => config.withWheelScrollLines(value)
    ),

    // -- Text area and viewport ----------------------------------------------------------------------------------
    named("text_area.left.percent", "textAreaLeftPercent", "text.area.left.percent", "text_area_left_percent")(
      insetPercent
    )(
      _.surfaceConfig.textAreaInsets.leftPercent,
      (config, value) => config.withTextAreaLeftInset(fractionOfPercent(value))
    ),
    named("text_area.right.percent", "textAreaRightPercent", "text.area.right.percent", "text_area_right_percent")(
      insetPercent
    )(
      _.surfaceConfig.textAreaInsets.rightPercent,
      (config, value) => config.withTextAreaRightInset(fractionOfPercent(value))
    ),
    named("text_area.top.percent", "textAreaTopPercent", "text.area.top.percent", "text_area_top_percent")(
      insetPercent
    )(
      _.surfaceConfig.textAreaInsets.topPercent,
      (config, value) => config.withTextAreaTopInset(fractionOfPercent(value))
    ),
    named("text_area.bottom.percent", "textAreaBottomPercent", "text.area.bottom.percent", "text_area_bottom_percent")(
      insetPercent
    )(
      _.surfaceConfig.textAreaInsets.bottomPercent,
      (config, value) => config.withTextAreaBottomInset(fractionOfPercent(value))
    ),
    named("viewport.width.percent", "viewportWidthPercent", "viewport_width_percent")(viewportPercent)(
      _.surfaceConfig.viewportSizing.width.percentValue,
      (config, value) =>
        config.withViewportWidthSizing(
          config.surfaceConfig.viewportSizing.width.copy(percent = fractionOfPercent(value))
        )
    ),
    named("viewport.width.max", "viewportWidthMax", "viewport_width_max")(int.filtered(_ >= 1).orEmpty)(
      _.surfaceConfig.viewportSizing.width.maxCells,
      (config, value) =>
        config.withViewportWidthSizing(config.surfaceConfig.viewportSizing.width.copy(maxCells = value))
    ),
    named("viewport.height.percent", "viewportHeightPercent", "viewport_height_percent")(viewportPercent)(
      _.surfaceConfig.viewportSizing.height.percentValue,
      (config, value) =>
        config.withViewportHeightSizing(
          config.surfaceConfig.viewportSizing.height.copy(percent = fractionOfPercent(value))
        )
    ),
    named("viewport.height.max", "viewportHeightMax", "viewport_height_max")(int.filtered(_ >= 1).orEmpty)(
      _.surfaceConfig.viewportSizing.height.maxCells,
      (config, value) =>
        config.withViewportHeightSizing(config.surfaceConfig.viewportSizing.height.copy(maxCells = value))
    )
  )

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
