package com.serenity.config

import com.serenity.animation.{AnimationConfig, TransitionKind}

/** Global accessibility policy applied after a preset and family configuration are resolved. */
enum MotionAccessibility(val configKey: String):
  case Standard extends MotionAccessibility("standard")
  case Reduced  extends MotionAccessibility("reduced")
  case Off      extends MotionAccessibility("off")

object MotionAccessibility:

  def fromConfigKey(value: String): Option[MotionAccessibility] =
    value.trim.toLowerCase match
      case "standard" | "system" | "none" => Some(MotionAccessibility.Standard)
      case "reduced"                      => Some(MotionAccessibility.Reduced)
      case "off" | "disabled"             => Some(MotionAccessibility.Off)
      case _                              => None

/** Independently configurable runtime motion families. */
enum MotionFamily(val configKey: String):
  case Cursor          extends MotionFamily("cursor")
  case EditorText      extends MotionFamily("editor_text")
  case CommandSurfaces extends MotionFamily("command_surfaces")
  case PinnedPanels    extends MotionFamily("pinned_panels")
  case UiTransitions   extends MotionFamily("ui_transitions")

/** Motion policy for one family before accessibility policy is applied. */
case class MotionFamilyConfig(
    enabled: Boolean,
    transitionKind: TransitionKind,
    animation: Option[AnimationConfig],
    speedScale: Double
):

  def normalized: MotionFamilyConfig =
    copy(speedScale = MotionConfig.clampSpeedScale(speedScale))

  def disabled: MotionFamilyConfig =
    copy(enabled = false, transitionKind = TransitionKind.Disabled, animation = None, speedScale = 0.0)

object MotionFamilyConfig:
  val disabled: MotionFamilyConfig = MotionFamilyConfig(false, TransitionKind.Disabled, None, 0.0)

/** Fully resolved policy consumed by runtime animation paths. */
case class EffectiveMotionConfig(families: Map[MotionFamily, MotionFamilyConfig]):

  def family(kind: MotionFamily): MotionFamilyConfig =
    families.getOrElse(kind, MotionFamilyConfig.disabled)

/** One authoritative hierarchy for baseline, per-family values, and accessibility override. */
case class MotionConfig(
    accessibility: MotionAccessibility,
    baseline: MotionPreset,
    families: Map[MotionFamily, MotionFamilyConfig]
):

  def normalized: MotionConfig =
    copy(families = families.view.mapValues(_.normalized).toMap)

  def withFallback(fallback: MotionConfig): MotionConfig =
    copy(families =
      MotionFamily.values.map(family => family -> families.getOrElse(family, fallback.families(family))).toMap
    )

  def effective: EffectiveMotionConfig =
    val normalizedFamilies = normalized.families
    val disabled           = accessibility != MotionAccessibility.Standard || baseline == MotionPreset.Reduced
    EffectiveMotionConfig(
      normalizedFamilies.view
        .mapValues(family =>
          if disabled || !family.enabled || family.speedScale <= 0.0 then family.disabled else family
        )
        .toMap
    )

object MotionConfig:

  val MinSpeedScale: Double = 0.0
  val MaxSpeedScale: Double = 4.0

  def clampSpeedScale(value: Double): Double =
    if value.isNaN || value.isInfinite then 1.0 else value.max(MinSpeedScale).min(MaxSpeedScale)

  def fromLegacy(config: SurfaceConfig): MotionConfig =
    val base                 = config.motionPreset.animationConfig
    val commandTransition    = config.commandRunnerTransitionKind.getOrElse(TransitionKind.Fade)
    val panelOpenTransition  = config.panelOpenTransitionKind.getOrElse(TransitionKind.OutlineThenContent)
    val panelCloseTransition = config.panelCloseTransitionKind.getOrElse(TransitionKind.Fade)
    MotionConfig(
      accessibility = MotionAccessibility.Standard,
      baseline = config.motionPreset,
      families = Map(
        MotionFamily.Cursor -> MotionFamilyConfig(
          enabled = true,
          transitionKind = TransitionKind.Fade,
          animation = base,
          speedScale = config.effectiveCursorTransitionSpeedScale
        ),
        MotionFamily.EditorText -> MotionFamilyConfig(
          enabled = config.editorInsertionTransitionKind != TransitionKind.Disabled,
          transitionKind = config.editorInsertionTransitionKind,
          animation = base,
          speedScale = config.effectiveEditorTextTransitionSpeedScale
        ),
        MotionFamily.CommandSurfaces -> MotionFamilyConfig(
          enabled = commandTransition != TransitionKind.Disabled,
          transitionKind = commandTransition,
          animation = config.commandRunnerAnimation,
          speedScale = config.effectiveCommandRunnerTransitionSpeedScale
        ),
        MotionFamily.PinnedPanels -> MotionFamilyConfig(
          enabled = panelOpenTransition != TransitionKind.Disabled || panelCloseTransition != TransitionKind.Disabled,
          transitionKind = panelOpenTransition,
          animation = config.uiAnimation,
          speedScale = config.effectiveUiTransitionSpeedScale
        ),
        MotionFamily.UiTransitions -> MotionFamilyConfig(
          enabled = true,
          transitionKind = TransitionKind.Fade,
          animation = config.uiAnimation,
          speedScale = config.effectiveUiTransitionSpeedScale
        )
      )
    )
