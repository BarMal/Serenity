package com.serenity.config

/** Surface material, background, blur, text-area insets and viewport sizing. */
private[config] object ConfigFieldsSurface:

  import ConfigFieldSyntax.*
  import FieldCodec.*

  val fields: List[ConfigField[?]] = List(
    // -- Material and background -----------------------------------------------------------------------------------------
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
    // -- Text area and viewport ------------------------------------------------------------------------------------------
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
