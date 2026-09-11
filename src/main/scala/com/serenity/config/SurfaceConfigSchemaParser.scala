package com.serenity.config

import com.serenity.animation.*

import AppConfigMotionOps.*

/** Parses one `ui.motion.*` setting's text value against the keys [[SurfaceConfigSchemaKeys]] declares, returning the
  * updated configuration or `None` when the key is not a motion setting or the value is not one it accepts.
  *
  * Everything this used to also parse for material/post-processing/display/command-runner/text-area/viewport settings
  * is gone (#1406) -- [[ConfigRegistry]] owns those end-to-end, and `ConfigManager.parseConfig` tries it first, so this
  * module's copy never ran.
  */
object SurfaceConfigSchemaParser:

  import SurfaceConfigSchemaKeys.*

  def parse(config: AppConfig, key: String, value: String): Option[AppConfig] =
    val trimmed = value.trim
    if motionPresetKeys.contains(key) then parseMotionPreset(trimmed).map(config.withMotionPreset)
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
    else None

  def invalidValue(key: String, value: String): Boolean =
    parse(AppConfig.default, key, value).isEmpty

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
        // `enabled` is derived from `transitionKind` (see `MotionFamilyConfig`), so setting it here only has an
        // effect when turning the family off; turning it on with no transition kind of its own to fall back to would
        // otherwise leave `transitionKind` at `Disabled` while `enabled` reported true.
        case "enabled" =>
          parseBoolean(value).map { enabled =>
            if enabled then settings
            else settings.copy(transitionKind = TransitionKind.Disabled)
          }
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
