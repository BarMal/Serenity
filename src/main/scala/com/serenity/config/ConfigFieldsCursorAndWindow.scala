package com.serenity.config

import java.util.Locale

import com.serenity.animation.sprite.{CompanionCharacter, CompanionSpriteConfig, SpriteFrameCycle}
import com.serenity.ui.layout.PanelPosition

/** Cursor, interface density, window chrome and companion sprite. */
private[config] object ConfigFieldsCursorAndWindow:

  import ConfigFieldSyntax.*
  import FieldCodec.*

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
    named("ui.corner_radius", "uiCornerRadiusPx", "ui.corner.radius", "ui_corner_radius")(
      int.filtered(radius => radius >= AppConfig.MinUiCornerRadiusPx && radius <= AppConfig.MaxUiCornerRadiusPx)
    )(_.uiCornerRadiusPx, (config, value) => config.withUiCornerRadiusPx(value)),
    named("ui.outline_thickness", "uiOutlineThicknessPx", "ui.outline.thickness", "ui_outline_thickness")(
      int.filtered(thickness =>
        thickness >= AppConfig.MinUiOutlineThicknessPx && thickness <= AppConfig.MaxUiOutlineThicknessPx
      )
    )(_.uiOutlineThicknessPx, (config, value) => config.withUiOutlineThicknessPx(value)),

    // -- Window ----------------------------------------------------------------------------------------------------------
    named("window.chrome", "windowChromeMode", "window.chrome.mode", "window_chrome", "window_chrome_mode")(
      enumerated(WindowChromeMode.fromConfigKey, _.configKey, text => WindowChromeMode.values.find(_.toString == text))
    )(_.windowChromeMode, (config, value) => config.withWindowChromeMode(value)),
    // -- Companion sprite ------------------------------------------------------------------------------------------------
    // motion.window_sitter.* (#934) is gone: the companion sprite panel absorbed the window sitter's
    // typing-reactivity (#934 v2), so those keys are no longer registered here. An old config file naming them reads
    // as unknown keys (`ConfigKeySchema.isKnownKey`) rather than erroring -- the same precedent this feature's own
    // now-removed `frames` field set.
    field("ui.companion_sprite.enabled", "companion.sprite.enabled")(boolean)(
      _.companionSpriteConfig.enabled,
      (config, value) => config.withCompanionSpriteConfig(config.companionSpriteConfig.copy(enabled = value))
    ),
    field("ui.companion_sprite.character", "companion.sprite.character")(
      enumerated(CompanionCharacter.fromConfigKey, _.id)
    )(
      _.companionSpriteConfig.character,
      (config, value) => config.withCompanionSpriteConfig(config.companionSpriteConfig.copy(character = value))
    ),
    field("ui.companion_sprite.position", "companion.sprite.position")(
      enumeratedValues(PanelPosition.values, _.toString.toLowerCase(Locale.ROOT))
    )(
      _.companionSpriteConfig.position,
      (config, value) => config.withCompanionSpriteConfig(config.companionSpriteConfig.copy(position = value))
    ),
    field("ui.companion_sprite.size", "companion.sprite.size")(
      int.filtered(size => size >= CompanionSpriteConfig.MinSize && size <= CompanionSpriteConfig.MaxSize)
    )(
      _.companionSpriteConfig.size,
      (config, value) => config.withCompanionSpriteConfig(config.companionSpriteConfig.copy(size = value))
    ),
    field("ui.companion_sprite.typing_cycle", "companion.sprite.typing.cycle")(
      enumerated(
        SpriteFrameCycle.fromConfigKey,
        _.configKey,
        text => SpriteFrameCycle.values.find(_.toString == text)
      )
    )(
      _.companionSpriteConfig.typingCycle,
      (config, value) => config.withCompanionSpriteConfig(config.companionSpriteConfig.copy(typingCycle = value))
    ),
    field("ui.companion_sprite.typing_active_ticks", "companion.sprite.typing.active_ticks")(int)(
      _.companionSpriteConfig.typingActiveTicks,
      (config, value) => config.withCompanionSpriteConfig(config.companionSpriteConfig.copy(typingActiveTicks = value))
    ),
    field("ui.companion_sprite.typing_fast_active_ticks", "companion.sprite.typing.fast_active_ticks")(int)(
      _.companionSpriteConfig.typingFastActiveTicks,
      (config, value) =>
        config.withCompanionSpriteConfig(config.companionSpriteConfig.copy(typingFastActiveTicks = value))
    ),
    field("ui.companion_sprite.typing_fast_threshold_ms", "companion.sprite.typing.fast_threshold_ms")(int)(
      _.companionSpriteConfig.typingFastThresholdMs,
      (config, value) =>
        config.withCompanionSpriteConfig(config.companionSpriteConfig.copy(typingFastThresholdMs = value))
    ),
    field("ui.visual_flair", "visual.flair.level")(
      enumerated(VisualFlairLevel.fromConfigKey, _.configKey)
    )(_.visualFlairLevel, (config, value) => config.withVisualFlairLevel(value)),
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
