package com.serenity.config

import com.serenity.animation.*
import com.serenity.keystroke.Modifier
import com.serenity.state.models.SurfacePlacement

import AppConfigMotionOps.*

/** Parses one surface setting's text value against the keys [[SurfaceConfigSchemaKeys]] declares, returning the updated
  * configuration or `None` when the key is not a surface setting or the value is not one it accepts.
  */
object SurfaceConfigSchemaParser:

  import SurfaceConfigSchemaKeys.*

  def parse(config: AppConfig, key: String, value: String): Option[AppConfig] =
    val trimmed = value.trim
    if materialPresetKeys.contains(key) then parseMaterialPreset(trimmed).map(config.withMaterialPreset)
    else if postProcessingKeys.contains(key) then
      PostProcessingEffect.fromConfigKey(trimmed).map(config.withPostProcessingEffect)
    else if uiShadowsKeys.contains(key) then trimmed.toBooleanOption.map(config.withUiShadowsEnabled)
    else if motionPresetKeys.contains(key) then parseMotionPreset(trimmed).map(config.withMotionPreset)
    else if motionAccessibilityKeys.contains(key) then
      MotionAccessibility.fromConfigKey(trimmed).map(config.withMotionAccessibility)
    else if motionFamilyKeys.contains(key) then parseMotionFamily(config, key, trimmed)
    else if elementTransitionSpeedScaleKeys.contains(key) then
      parseElementTransitionSpeedScale(trimmed).map(config.withElementTransitionSpeedScale)
    else if editorTextTransitionSpeedScaleKeys.contains(key) then
      parseElementTransitionSpeedScale(trimmed).map(scale => config.withEditorTextTransitionSpeedScale(Some(scale)))
    else if commandRunnerTransitionSpeedScaleKeys.contains(key) then
      parseElementTransitionSpeedScale(trimmed).map(scale => config.withCommandRunnerTransitionSpeedScale(Some(scale)))
    else if uiTransitionSpeedScaleKeys.contains(key) then
      parseElementTransitionSpeedScale(trimmed).map(scale => config.withUiTransitionSpeedScale(Some(scale)))
    else if cursorTransitionSpeedScaleKeys.contains(key) then
      parseElementTransitionSpeedScale(trimmed).map(scale => config.withCursorTransitionSpeedScale(Some(scale)))
    else if commandRunnerAnimationKeys.contains(key) then
      parseAnimationPreset(trimmed).map(config.withCommandRunnerAnimation)
    else if commandRunnerTransitionKeys.contains(key) then
      parseTransitionKind(trimmed).map(kind => config.withCommandRunnerTransitionKind(Some(kind)))
    else if uiAnimationKeys.contains(key) then parseAnimationPreset(trimmed).map(config.withUiAnimation)
    else if editorTextTransitionKeys.contains(key) then
      parseTransitionKind(trimmed).map(config.withEditorInsertionTransitionKind)
    else if panelOpenTransitionKeys.contains(key) then
      parseTransitionKind(trimmed).map(kind => config.withPanelOpenTransitionKind(Some(kind)))
    else if panelCloseTransitionKeys.contains(key) then
      parseTransitionKind(trimmed).map(kind => config.withPanelCloseTransitionKind(Some(kind)))
    else if commandRunnerVisibleRowsKeys.contains(key) then
      parseCommandRunnerVisibleRows(trimmed).map(config.withCommandRunnerVisibleRows)
    else if commandRunnerItemGapRowsKeys.contains(key) then
      parseCommandRunnerItemGapRows(trimmed).map(config.withCommandRunnerItemGapRows)
    else if commandRunnerCursorGapRowsKeys.contains(key) then
      parseCommandRunnerCursorGapRows(trimmed).map(config.withCommandRunnerCursorGapRows)
    else if cursorInfoBarBackgroundAlphaKeys.contains(key) then
      parseCursorInfoBarBackgroundAlpha(trimmed).map(config.withCursorInfoBarBackgroundAlpha)
    else parseDisplayAndLayout(config, key, trimmed)

  private def parseDisplayAndLayout(config: AppConfig, key: String, trimmed: String): Option[AppConfig] =
    if renderFpsKeys.contains(key) then RenderFpsTarget.fromConfigKey(trimmed).map(config.withRenderFpsTarget)
    else if renderDamageGranularityKeys.contains(key) then
      RenderDamageGranularity.fromConfigKey(trimmed).map(config.withRenderDamageGranularity)
    else if wordWrapKeys.contains(key) then parseBoolean(trimmed).map(config.withWordWrap)
    else if visualLineNavigationKeys.contains(key) then parseBoolean(trimmed).map(config.withVisualLineCursorNavigation)
    else if blurRadiusKeys.contains(key) then
      trimmed.toFloatOption.filter(radius => radius >= 0.0f && radius <= 1.0f).map(config.withBlurRadius)
    else if backgroundStyleKeys.contains(key) then
      BackgroundStyle.fromConfigKey(trimmed).map(config.withBackgroundStyle)
    else if lineNumberKeys.contains(key) then parseBoolean(trimmed).map(config.withLineNumbers)
    else if gutterKeys.contains(key) then parseBoolean(trimmed).map(config.withGutter)
    else if wordCountKeys.contains(key) then parseBoolean(trimmed).map(config.withWordCount)
    else if commentDisplayModeKeys.contains(key) then
      CommentDisplayMode.fromConfigKey(trimmed).map(config.withCommentDisplayMode)
    else if commandRunnerShowKeyHintsKeys.contains(key) then
      parseBoolean(trimmed).map(config.withCommandRunnerShowKeyHints)
    else if commandRunnerCursorPeekKeys.contains(key) then
      parseBoolean(trimmed).map(config.withCommandRunnerCursorPeekEnabled)
    else if commandRunnerCursorPeekModifierKeys.contains(key) then
      parseModifier(trimmed).map(config.withCommandRunnerCursorPeekModifier)
    else if commandRunnerCursorPeekTapWindowKeys.contains(key) then
      trimmed.toLongOption.map(config.withCommandRunnerCursorPeekTapWindowMillis)
    else if commandRunnerCursorPeekPlacementKeys.contains(key) then
      parseSurfacePlacement(trimmed).map(config.withCommandRunnerCursorPeekPlacement)
    else if paneHeaderKeys.contains(key) then parseBoolean(trimmed).map(config.withPaneHeaders)
    else if focusedTextBodyKeys.contains(key) then parseBoolean(trimmed).map(config.withFocusedTextBody)
    else if contextualToolbarKeys.contains(key) then parseBoolean(trimmed).map(config.withContextualToolbarEnabled)
    else if contextualToolbarModeKeys.contains(key) then
      ToolbarDisplayMode.fromConfigKey(trimmed).map(config.withContextualToolbarDisplayMode)
    else if textAreaLeftPercentKeys.contains(key) then parseInsetPercent(trimmed).map(config.withTextAreaLeftInset)
    else if textAreaRightPercentKeys.contains(key) then parseInsetPercent(trimmed).map(config.withTextAreaRightInset)
    else if textAreaTopPercentKeys.contains(key) then parseInsetPercent(trimmed).map(config.withTextAreaTopInset)
    else if textAreaBottomPercentKeys.contains(key) then parseInsetPercent(trimmed).map(config.withTextAreaBottomInset)
    else if viewportWidthPercentKeys.contains(key) then
      parseViewportPercent(trimmed)
        .map(percent =>
          config.withViewportWidthSizing(config.surfaceConfig.viewportSizing.width.copy(percent = percent))
        )
    else if viewportWidthMaxKeys.contains(key) then
      parseViewportMaxCells(trimmed)
        .map(maxCells =>
          config.withViewportWidthSizing(config.surfaceConfig.viewportSizing.width.copy(maxCells = maxCells))
        )
    else if viewportHeightPercentKeys.contains(key) then
      parseViewportPercent(trimmed)
        .map(percent =>
          config.withViewportHeightSizing(config.surfaceConfig.viewportSizing.height.copy(percent = percent))
        )
    else if viewportHeightMaxKeys.contains(key) then
      parseViewportMaxCells(trimmed)
        .map(maxCells =>
          config.withViewportHeightSizing(config.surfaceConfig.viewportSizing.height.copy(maxCells = maxCells))
        )
    else None

  def invalidValue(key: String, value: String): Boolean =
    parse(AppConfig.default, key, value).isEmpty

  private def parseModifier(value: String): Option[Modifier] =
    Modifier.values.find(_.toString.equalsIgnoreCase(value))

  // The cursor-peek prototype's placement is only ever above/below the cursor -- `SurfacePlacement.Corner` (issue
  // #1310) isn't a value this setting can take, and being a parameterized case, it also means `.values` is no
  // longer generated for the enum.
  private def parseSurfacePlacement(value: String): Option[SurfacePlacement] =
    List(SurfacePlacement.AboveCursor, SurfacePlacement.BelowCursor)
      .find(placement => placement.toString.equalsIgnoreCase(value.replace("-", "")))

  private def parseBoolean(value: String): Option[Boolean] =
    value.toLowerCase match
      case "true" | "on" | "enabled"    => Some(true)
      case "false" | "off" | "disabled" => Some(false)
      case _                            => None

  private def parseMotionFamily(config: AppConfig, key: String, value: String): Option[AppConfig] =
    val parts = key.stripPrefix(motionFamilyPrefix).split("\\.")
    for
      familyName <- parts.headOption
      family     <- MotionFamily.values.find(_.configKey == familyName)
      field    = parts.drop(1).mkString(".")
      current  = config.surfaceConfig.motionConfiguration.getOrElse(MotionConfig.fromLegacy(config.surfaceConfig))
      settings = current.families(family)
      updated <- field match
        case "enabled"    => parseBoolean(value).map(enabled => settings.copy(enabled = enabled))
        case "transition" => parseTransitionKind(value).map(kind => settings.copy(transitionKind = kind))
        case "animation" | "animation.preset" if value.equalsIgnoreCase("custom") =>
          Some(settings.copy(animation = Some(settings.animation.getOrElse(AnimationConfig.Enabled.smooth))))
        case "animation" | "animation.preset" =>
          parseAnimationPreset(value).map(animation => settings.copy(animation = animation))
        case "animation.duration_ms" =>
          value.toIntOption
            .filter(_ > 0)
            .map(durationMs =>
              settings.copy(animation =
                Some(
                  settings.animation
                    .getOrElse(AnimationConfig.Enabled.smooth)
                    .copy(totalDuration = scala.concurrent.duration.Duration.fromNanos(durationMs * 1_000_000L))
                )
              )
            )
        case "animation.steps" =>
          value.toIntOption
            .filter(_ > 0)
            .map(steps =>
              settings
                .copy(animation =
                  Some(settings.animation.getOrElse(AnimationConfig.Enabled.smooth).copy(steps = steps))
                )
            )
        case "speed_scale" => parseElementTransitionSpeedScale(value).map(scale => settings.copy(speedScale = scale))
        case "open_transition" if family == MotionFamily.PinnedPanels =>
          parseTransitionKind(value).map(kind =>
            settings.copy(transitionOverrides = settings.transitionOverrides.updated(TransitionScope.PanelOpen, kind))
          )
        case "close_transition" if family == MotionFamily.PinnedPanels =>
          parseTransitionKind(value).map(kind =>
            settings.copy(transitionOverrides = settings.transitionOverrides.updated(TransitionScope.PanelClose, kind))
          )
        case _ => None
    yield config.withMotionFamilyConfiguration(family, updated)

  private def parseCommandRunnerVisibleRows(value: String): Option[Option[Int]] =
    value.toLowerCase match
      case "auto" | "default" | "" => Some(None)
      case other =>
        other.toIntOption
          .filter(rows =>
            rows >= AppConfig.MinCommandRunnerVisibleRows &&
              rows <= AppConfig.MaxCommandRunnerVisibleRows
          )
          .map(rows => Some(rows))

  private def parseCommandRunnerItemGapRows(value: String): Option[Double] =
    value.toDoubleOption.filter(rows =>
      rows >= AppConfig.MinCommandRunnerItemGapRows && rows <= AppConfig.MaxCommandRunnerItemGapRows
    )

  private def parseCommandRunnerCursorGapRows(value: String): Option[Option[Double]] =
    value.toLowerCase match
      case "auto" | "default" | "" => Some(None)
      case other =>
        other.toDoubleOption
          .filter(rows =>
            rows >= AppConfig.MinCommandRunnerCursorGapRows && rows <= AppConfig.MaxCommandRunnerCursorGapRows
          )
          .map(rows => Some(rows))

  private def parseCursorInfoBarBackgroundAlpha(value: String): Option[Option[Double]] =
    value.toLowerCase match
      case "auto" | "default" | "" => Some(None)
      case other =>
        other.toDoubleOption
          .filter(alpha =>
            alpha >= AppConfig.MinCursorInfoBarBackgroundAlpha && alpha <= AppConfig.MaxCursorInfoBarBackgroundAlpha
          )
          .map(alpha => Some(alpha))

  /** These are stored as fractions and written as percentages, so reading one back divides by 100 -- and binary
    * floating point turns 17.3 into 0.17299999999999996 rather than the 0.173 that was saved. Rounding to the precision
    * the file actually carries makes saving and loading a settings value give that value back.
    */
  private def fractionOfPercent(value: Double): Double =
    BigDecimal(value / 100.0).setScale(9, BigDecimal.RoundingMode.HALF_UP).toDouble

  private def parseInsetPercent(value: String): Option[Double] =
    value.toDoubleOption
      .map(fractionOfPercent)
      .filter(percent => percent >= 0.0 && percent <= TextAreaInsets.MaxInset)

  private def parseViewportPercent(value: String): Option[Double] =
    value.toDoubleOption
      .map(fractionOfPercent)
      .filter(percent =>
        percent >= ViewportAxisSizing.MinPercent &&
          percent <= ViewportAxisSizing.MaxPercent
      )

  private def parseViewportMaxCells(value: String): Option[Option[Int]] =
    if value.trim.isEmpty then Some(None)
    else value.toIntOption.filter(_ >= 1).map(Some(_))

  private def parseMaterialPreset(value: String): Option[MaterialPreset] =
    value.toLowerCase match
      case "solid" | "opaque"      => Some(MaterialPreset.Solid)
      case "clear" | "transparent" => Some(MaterialPreset.Clear)
      case "frosted" | "soft"      => Some(MaterialPreset.Frosted)
      case "crystal" | "glass"     => Some(MaterialPreset.Crystal)
      case "custom"                => Some(MaterialPreset.Custom)
      case _                       => None

  private def parseMotionPreset(value: String): Option[MotionPreset] =
    value.toLowerCase match
      case "reduced" | "none" | "off" | "disabled" => Some(MotionPreset.Reduced)
      case "subtle"                                => Some(MotionPreset.Subtle)
      case "smooth"                                => Some(MotionPreset.Smooth)
      case "expressive" | "full" | "quick"         => Some(MotionPreset.Expressive)
      case "custom"                                => Some(MotionPreset.Custom)
      case _                                       => None

  private def parseAnimationPreset(value: String): Option[Option[AnimationConfig]] =
    value.toLowerCase match
      case "none" | "false" | "off" | "disabled" => Some(None)
      case "quick" | "expressive"                => Some(AnimationConfig.quick)
      case "smooth"                              => Some(AnimationConfig.smooth)
      case "subtle"                              => Some(AnimationConfig.subtle)
      case _                                     => None

  private def parseTransitionKind(value: String): Option[TransitionKind] =
    value.toLowerCase match
      case "fade"                                             => Some(TransitionKind.Fade)
      case "typed" | "typed-text" | "type"                    => Some(TransitionKind.TypedText)
      case "directional" | "directional-sweep" | "sweep"      => Some(TransitionKind.DirectionalSweep)
      case "tandem" | "line-and-character" | "line-character" => Some(TransitionKind.LineAndCharacterTandem)
      case "outline" | "outline-then-content" | "frame-then-content" =>
        Some(TransitionKind.OutlineThenContent)
      case "off" | "none" | "disabled" => Some(TransitionKind.Disabled)
      case _                           => None

  private def parseElementTransitionSpeedScale(value: String): Option[Double] =
    value.toDoubleOption
      .filter(scale =>
        scale >= AppConfig.MinElementTransitionSpeedScale &&
          scale <= AppConfig.MaxElementTransitionSpeedScale
      )
