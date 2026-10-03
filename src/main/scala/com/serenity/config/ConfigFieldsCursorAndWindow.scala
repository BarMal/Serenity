package com.serenity.config

/** Cursor, interface density and window chrome. */
private[config] object ConfigFieldsCursorAndWindow:

  import ConfigFieldSyntax.*
  import FieldCodec.*

  private def panelEscapeTarget(mode: AppMode): ConfigField[PanelEscapeTarget] =
    field(s"ui.panel.escape_returns_to.${mode.configKey}")(
      enumerated(PanelEscapeTarget.fromConfigKey, _.configKey)
    )(_.inputConfig.panelEscapeReturnsTo.forMode(mode), (config, value) => config.withPanelEscapeTarget(mode, value))

  val fields: List[ConfigField[?]] = List(
    // -- Cursor ----------------------------------------------------------------------------------------------------------
    named("editor.cursor.mode", "cursorMode", "cursor.mode", "cursor_mode")(
      enumerated(CursorMode.fromConfigKey, _.configKey, text => CursorMode.values.find(_.toString == text))
    )(
      _.cursorMode,
      (config, value) => config.withCursorMode(value)
    ),
    named("editor.cursor.active_color", "cursorActiveColor", "cursor.active.color", "cursor_active_color")(
      color.orEmpty
    )(
      _.cursorColors.active,
      (config, value) => config.withCursorColors(config.cursorColors.copy(active = value))
    ),
    named("editor.cursor.inactive_color", "cursorInactiveColor", "cursor.inactive.color", "cursor_inactive_color")(
      color.orEmpty
    )(
      _.cursorColors.inactive,
      (config, value) => config.withCursorColors(config.cursorColors.copy(inactive = value))
    ),
    // -- Interface -------------------------------------------------------------------------------------------------------
    named("ui.density", "interfaceDensity", "interface.density", "interface_density")(
      enumerated(InterfaceDensity.fromConfigKey, _.configKey, text => InterfaceDensity.values.find(_.toString == text))
    )(_.interfaceDensity, (config, value) => config.withInterfaceDensity(value)),
    named("ui.element_gap", "uiElementGap", "ui.element.gap", "ui_element_gap")(
      double
        .filtered(gap => gap.isFinite && gap >= AppConfig.MinUiElementGap && gap <= AppConfig.MaxUiElementGap)
        .orAuto
    )(_.uiElementGap, (config, value) => config.withUiElementGap(value)),
    named("ui.outline_thickness", "uiOutlineThicknessPx", "ui.outline.thickness", "ui_outline_thickness")(
      int.filtered(thickness =>
        thickness >= AppConfig.MinUiOutlineThicknessPx && thickness <= AppConfig.MaxUiOutlineThicknessPx
      )
    )(_.uiOutlineThicknessPx, (config, value) => config.withUiOutlineThicknessPx(value)),
    panelEscapeTarget(AppMode.Code),
    panelEscapeTarget(AppMode.Prose),

    // -- Window ----------------------------------------------------------------------------------------------------------
    named("window.chrome", "windowChromeMode", "window.chrome.mode", "window_chrome", "window_chrome_mode")(
      enumerated(WindowChromeMode.fromConfigKey, _.configKey, text => WindowChromeMode.values.find(_.toString == text))
    )(_.windowChromeMode, (config, value) => config.withWindowChromeMode(value)),
    field("window.translucent", "window_translucent")(boolean.orAuto)(
      _.windowTranslucent,
      (config, value) => config.withWindowTranslucent(value)
    ),
    // #1316: no preferred size to update yet means there is nothing to update -- inventing the other dimension made a
    // width-only edit fabricate a height nobody asked for.
    named("window.preferred.width", "preferredWindowWidth", "window_preferred_width")(int.orEmpty)(
      _.preferredWindowSize.map(_.width),
      (config, value) =>
        value.fold(config)(width =>
          config.preferredWindowSize.fold(config)(size => config.withPreferredWindowSize(size.copy(width = width)))
        )
    ),
    named("window.preferred.height", "preferredWindowHeight", "window_preferred_height")(int.orEmpty)(
      _.preferredWindowSize.map(_.height),
      (config, value) =>
        value.fold(config)(height =>
          config.preferredWindowSize.fold(config)(size => config.withPreferredWindowSize(size.copy(height = height)))
        )
    )
  )
