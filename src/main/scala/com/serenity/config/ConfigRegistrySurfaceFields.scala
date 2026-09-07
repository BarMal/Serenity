package com.serenity.config

import java.util.Locale

import com.serenity.keystroke.Modifier
import com.serenity.state.models.SurfacePlacement

/** The command-runner, rendering/display, material/background and text-area/viewport settings from [[ConfigRegistry]]'s
  * `fields` list, split out to keep that file under the architecture ratchet's file-length target. See
  * [[ConfigRegistry]]'s own doc comment for why this list exists at all.
  */
object ConfigRegistrySurfaceFields:

  import FieldCodec.*
  import ConfigRegistry.{field, named, restoredBy}

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

  private def lowercased[A](values: Array[A]): FieldCodec[A] =
    enumerated(
      text => values.find(_.toString.equalsIgnoreCase(text.replace("-", ""))),
      value => value.toString.toLowerCase(Locale.ROOT)
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

  val fields: List[ConfigField[?]] = List(
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
    // Above/below the cursor only (issue #1310: `SurfacePlacement.Corner` isn't a valid value here, and being a
    // parameterized case, it also means `.values` is no longer generated for the enum).
    field("command_runner.cursor_peek.placement", "command.runner.cursor.peek.placement")(
      lowercased(Array(SurfacePlacement.AboveCursor, SurfacePlacement.BelowCursor))
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
