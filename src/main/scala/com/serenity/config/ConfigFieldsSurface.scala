package com.serenity.config

/** Diagnostic highlight blending, text-area insets and viewport sizing. */
private[config] object ConfigFieldsSurface:

  import ConfigFieldSyntax.*
  import FieldCodec.*

  val fields: List[ConfigField[?]] = List(
    // issue #1530/#1529: how strongly a diagnostic's severity colour shows through its highlight, versus the colour
    // it's painted over -- was a hardcoded literal in `RendererHighlights`.
    named("ui.diagnostic_highlight_blend_weight", "diagnosticHighlightBlendWeight")(
      double.filtered(weight =>
        weight >= AppConfig.MinDiagnosticHighlightBlendWeight && weight <= AppConfig.MaxDiagnosticHighlightBlendWeight
      )
    )(
      _.surfaceConfig.diagnosticHighlightBlendWeight,
      (config, value) => config.withDiagnosticHighlightBlendWeight(value)
    )
      .restoredBy((config, value) =>
        config.withSurfaceConfig(config.surfaceConfig.copy(diagnosticHighlightBlendWeight = value))
      ),
    // -- Text area and viewport ------------------------------------------------------------------------------------------
    named(
      "editor.text_area.left",
      "textAreaLeftPercent",
      "text_area.left.percent",
      "text.area.left.percent",
      "text_area_left_percent"
    )(
      insetPercent
    )(
      _.surfaceConfig.textAreaInsets.leftPercent,
      (config, value) => config.withTextAreaLeftInset(fractionOfPercent(value))
    ),
    named(
      "editor.text_area.right",
      "textAreaRightPercent",
      "text_area.right.percent",
      "text.area.right.percent",
      "text_area_right_percent"
    )(
      insetPercent
    )(
      _.surfaceConfig.textAreaInsets.rightPercent,
      (config, value) => config.withTextAreaRightInset(fractionOfPercent(value))
    ),
    named(
      "editor.text_area.top",
      "textAreaTopPercent",
      "text_area.top.percent",
      "text.area.top.percent",
      "text_area_top_percent"
    )(
      insetPercent
    )(
      _.surfaceConfig.textAreaInsets.topPercent,
      (config, value) => config.withTextAreaTopInset(fractionOfPercent(value))
    ),
    named(
      "editor.text_area.bottom",
      "textAreaBottomPercent",
      "text_area.bottom.percent",
      "text.area.bottom.percent",
      "text_area_bottom_percent"
    )(
      insetPercent
    )(
      _.surfaceConfig.textAreaInsets.bottomPercent,
      (config, value) => config.withTextAreaBottomInset(fractionOfPercent(value))
    ),
    named("window.viewport.width_percent", "viewportWidthPercent", "viewport.width.percent", "viewport_width_percent")(
      viewportPercent
    )(
      _.surfaceConfig.viewportSizing.width.percentValue,
      (config, value) =>
        config.withViewportWidthSizing(
          config.surfaceConfig.viewportSizing.width.copy(percent = fractionOfPercent(value))
        )
    ),
    named("window.viewport.width_max", "viewportWidthMax", "viewport.width.max", "viewport_width_max")(
      int.filtered(_ >= 1).orEmpty
    )(
      _.surfaceConfig.viewportSizing.width.maxCells,
      (config, value) =>
        config.withViewportWidthSizing(config.surfaceConfig.viewportSizing.width.copy(maxCells = value))
    ),
    named(
      "window.viewport.height_percent",
      "viewportHeightPercent",
      "viewport.height.percent",
      "viewport_height_percent"
    )(viewportPercent)(
      _.surfaceConfig.viewportSizing.height.percentValue,
      (config, value) =>
        config.withViewportHeightSizing(
          config.surfaceConfig.viewportSizing.height.copy(percent = fractionOfPercent(value))
        )
    ),
    named("window.viewport.height_max", "viewportHeightMax", "viewport.height.max", "viewport_height_max")(
      int.filtered(_ >= 1).orEmpty
    )(
      _.surfaceConfig.viewportSizing.height.maxCells,
      (config, value) =>
        config.withViewportHeightSizing(config.surfaceConfig.viewportSizing.height.copy(maxCells = value))
    )
  )
