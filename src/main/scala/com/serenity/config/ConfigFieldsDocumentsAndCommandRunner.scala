package com.serenity.config

import com.serenity.keystroke.Modifier
import com.serenity.state.models.SurfacePlacement

import AppConfigMotionOps.*

/** Document defaults, editor basics and the command runner's own knobs. */
private[config] object ConfigFieldsDocumentsAndCommandRunner:

  import ConfigFieldSyntax.*
  import FieldCodec.*

  val fields: List[ConfigField[?]] = List(
    // -- Documents and editor --------------------------------------------------------------------------------------------
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
    named("editor.minimum_pane_width", "minimumPaneWidth", "editor.minimum.pane.width", "editor_minimum_pane_width")(
      int
    )(_.editorConfig.minimumPaneWidth, (config, value) => config.withMinimumPaneWidth(value)),
    named("input.wheel_scroll_lines", "wheelScrollLines", "input_wheel_scroll_lines")(int)(
      _.inputConfig.wheelScrollLines,
      (config, value) => config.withWheelScrollLines(value)
    ),

    // -- Command runner --------------------------------------------------------------------------------------------------
    field("command_runner.visible_rows", "command.runner.visible.rows", "command_runner_visible_rows")(
      int
        .filtered(rows =>
          rows >= AppConfig.MinCommandRunnerVisibleRows && rows <= AppConfig.MaxCommandRunnerVisibleRows
        )
        .orAuto
    )(_.surfaceConfig.commandRunnerVisibleRows, (config, value) => config.withCommandRunnerVisibleRows(value)),
    field("command_runner.item_gap_rows", "command.runner.item.gap.rows", "command_runner_item_gap_rows")(
      double
        .filtered(rows =>
          rows >= AppConfig.MinCommandRunnerItemGapRows && rows <= AppConfig.MaxCommandRunnerItemGapRows
        )
        .orAuto
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
    )
  )
