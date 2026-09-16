package com.serenity.config

import AppConfigMotionOps.*

/** Rendering cadence and everything `display.*`: chrome rows, wrap, scrolling, toolbar. */
private[config] object ConfigFieldsDisplay:

  import ConfigFieldSyntax.*
  import FieldCodec.*

  val fields: List[ConfigField[?]] = List(
    // -- Rendering and display -------------------------------------------------------------------------------------------
    named("ui.render.fps", "renderFpsTarget", "render.fps", "render_fps", "ui_render_fps")(
      enumerated(RenderFpsTarget.fromConfigKey, _.configKey, text => RenderFpsTarget.values.find(_.toString == text))
    )(_.surfaceConfig.renderFpsTarget, (config, value) => config.withRenderFpsTarget(value)),
    named(
      "ui.render.damage_granularity",
      "renderDamageGranularity",
      "render.damage_granularity",
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
      "ui.render.cache_capacity",
      "render.frame_state_cache_capacity",
      "render.frame_state.cache_capacity",
      "render_frame_state_cache_capacity"
    )(
      int.filtered(capacity =>
        capacity >= AppConfig.MinRendererFrameStateCacheCapacity && capacity <= AppConfig.MaxRendererFrameStateCacheCapacity
      )
    )(
      _.surfaceConfig.rendererFrameStateCacheCapacity,
      (config, value) => config.withRendererFrameStateCacheCapacity(value)
    ),
    named("editor.word_wrap", "wordWrapEnabled", "display.word_wrap", "display.word.wrap", "display_word_wrap")(
      boolean
    )(
      _.surfaceConfig.wordWrapEnabled,
      (config, value) => config.withWordWrap(value)
    ),
    named(
      "editor.visual_line_navigation",
      "visualLineCursorNavigation",
      "display.visual_line_navigation",
      "display.visual.line.navigation",
      "display_visual_line_navigation"
    )(boolean)(
      _.surfaceConfig.visualLineCursorNavigation,
      (config, value) => config.withVisualLineCursorNavigation(value)
    ),
    named(
      "editor.typewriter_scrolling",
      "typewriterScrollingEnabled",
      "display.typewriter_scrolling",
      "display.typewriter.scrolling",
      "display_typewriter_scrolling"
    )(boolean)(
      _.surfaceConfig.typewriterScrollingEnabled,
      (config, value) => config.withTypewriterScrolling(value)
    ),
    named(
      "editor.line_numbers",
      "showLineNumbers",
      "display.line_numbers",
      "display.line.numbers",
      "display_line_numbers"
    )(boolean)(
      _.surfaceConfig.showLineNumbers,
      (config, value) => config.withLineNumbers(value)
    ),
    named("editor.line_number_side", "lineNumberSide", "display.line_number_side", "display.line.number.side")(
      enumerated(LineNumberSide.fromConfigKey, _.configKey, text => LineNumberSide.values.find(_.toString == text))
    )(
      _.surfaceConfig.lineNumberLayout.side,
      (config, value) => config.withLineNumberLayout(config.lineNumberLayout.copy(side = value))
    ),
    named(
      "editor.line_number_margin_left",
      "lineNumberMarginLeft",
      "display.line_number_margin_left",
      "display.line.number.margin.left"
    )(
      int.filtered(cells => cells >= 0 && cells <= LineNumberLayout.MaxCells)
    )(
      _.surfaceConfig.lineNumberLayout.marginLeft,
      (config, value) => config.withLineNumberLayout(config.lineNumberLayout.copy(marginLeft = value))
    ),
    named(
      "editor.line_number_margin_right",
      "lineNumberMarginRight",
      "display.line_number_margin_right",
      "display.line.number.margin.right"
    )(
      int.filtered(cells => cells >= 0 && cells <= LineNumberLayout.MaxCells)
    )(
      _.surfaceConfig.lineNumberLayout.marginRight,
      (config, value) => config.withLineNumberLayout(config.lineNumberLayout.copy(marginRight = value))
    ),
    named(
      "editor.line_number_padding",
      "lineNumberPadding",
      "display.line_number_padding",
      "display.line.number.padding"
    )(
      int.filtered(cells => cells >= 0 && cells <= LineNumberLayout.MaxCells)
    )(
      _.surfaceConfig.lineNumberLayout.padding,
      (config, value) => config.withLineNumberLayout(config.lineNumberLayout.copy(padding = value))
    ),
    field("editor.comments", "display.comments", "display_comments")(
      enumerated(
        CommentDisplayMode.fromConfigKey,
        _.configKey,
        text => CommentDisplayMode.values.find(_.toString == text)
      )
    )(
      _.surfaceConfig.commentDisplayMode,
      (config, value) => config.withCommentDisplayMode(value)
    ),
    named(
      "editor.pane_headers",
      "showPaneHeaders",
      "display.pane_headers",
      "display.pane.headers",
      "display_pane_headers"
    )(boolean)(
      _.surfaceConfig.showPaneHeaders,
      (config, value) => config.withPaneHeaders(value)
    ),
    named(
      "editor.focused_text_body",
      "focusedTextBodyEnabled",
      "display.focused_text_body",
      "display.focused.text.body",
      "display_focused_text_body"
    )(boolean)(_.surfaceConfig.focusedTextBodyEnabled, (config, value) => config.withFocusedTextBody(value)),
    named(
      "editor.contextual_toolbar",
      "contextualToolbarEnabled",
      "display.contextual_toolbar",
      "display.contextual.toolbar",
      "display_contextual_toolbar"
    )(boolean)(_.surfaceConfig.contextualToolbarEnabled, (config, value) => config.withContextualToolbarEnabled(value)),
    named(
      "editor.contextual_toolbar_mode",
      "contextualToolbarDisplayMode",
      "display.contextual_toolbar_mode",
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
    )
  )
