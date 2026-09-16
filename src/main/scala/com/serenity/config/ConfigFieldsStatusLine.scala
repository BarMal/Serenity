package com.serenity.config

/** The status line: which segments it shows, where it lives, and its colour overrides. */
private[config] object ConfigFieldsStatusLine:

  import ConfigFieldSyntax.*
  import FieldCodec.*

  val fields: List[ConfigField[?]] = List(
    // -- Status line ---------------------------------------------------------------------------------------------
    named("status.segments", "statusSegments", "status_segments")(statusSegments)(
      _.statusLine.segments,
      (config, value) => config.withStatusLineSegments(value)
    ),
    named("status.placement", "statusPlacement", "status_placement")(
      enumerated(
        StatusLinePlacement.fromConfigKey,
        _.configKey,
        text => StatusLinePlacement.values.find(_.toString == text)
      )
    )(_.statusLine.placement, (config, value) => config.withStatusLinePlacement(value)),
    named(
      "status.foreground_color",
      "statusForegroundColor",
      "status_foreground_color",
      "cursor.info_bar.foreground_color",
      "cursor_info_bar_foreground_color"
    )(color.orEmpty)(
      _.statusLine.colors.foreground,
      (config, value) => config.withStatusLineColors(config.statusLine.colors.copy(foreground = value))
    ),
    named(
      "status.background_color",
      "statusBackgroundColor",
      "status_background_color",
      "cursor.info_bar.background_color",
      "cursor_info_bar_background_color"
    )(color.orEmpty)(
      _.statusLine.colors.background,
      (config, value) => config.withStatusLineColors(config.statusLine.colors.copy(background = value))
    ),
    named(
      "status.background_alpha",
      "statusBackgroundAlpha",
      "status_background_alpha",
      "display.cursor_info_bar_background_alpha",
      "display.cursor_info_bar.background_alpha",
      "display_cursor_info_bar_background_alpha"
    )(
      double
        .filtered(alpha => alpha >= StatusLineConfig.MinBackgroundAlpha && alpha <= StatusLineConfig.MaxBackgroundAlpha)
        .orAuto
    )(
      _.statusLine.colors.backgroundAlpha,
      (config, value) => config.withStatusLineColors(config.statusLine.colors.copy(backgroundAlpha = value))
    )
  )
