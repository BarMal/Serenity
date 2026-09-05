package com.serenity.config

import com.serenity.animation.{AnimationConfig, TransitionKind, TransitionScope}

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
final case class MotionFamilyConfig(
    enabled: Boolean,
    transitionKind: TransitionKind,
    animation: Option[AnimationConfig],
    speedScale: Double,
    transitionOverrides: Map[TransitionScope, TransitionKind] = Map.empty
):

  def normalized: MotionFamilyConfig =
    copy(speedScale = MotionConfig.clampSpeedScale(speedScale))

  def disabled: MotionFamilyConfig =
    copy(
      enabled = false,
      transitionKind = TransitionKind.Disabled,
      animation = None,
      speedScale = 0.0,
      transitionOverrides = transitionOverrides.view.mapValues(_ => TransitionKind.Disabled).toMap
    )

  def transitionKindFor(scope: TransitionScope): TransitionKind =
    transitionOverrides.getOrElse(scope, transitionKind)

object MotionFamilyConfig:
  val disabled: MotionFamilyConfig = MotionFamilyConfig(false, TransitionKind.Disabled, None, 0.0)

/** Fully resolved policy consumed by runtime animation paths. */
final case class EffectiveMotionConfig(families: Map[MotionFamily, MotionFamilyConfig]):

  def family(kind: MotionFamily): MotionFamilyConfig =
    families.getOrElse(kind, MotionFamilyConfig.disabled)

/** One authoritative hierarchy for baseline, per-family values, and accessibility override. */
final case class MotionConfig(
    accessibility: MotionAccessibility,
    baseline: MotionPreset,
    families: Map[MotionFamily, MotionFamilyConfig]
):

  def normalized: MotionConfig =
    copy(families = families.view.mapValues(_.normalized).toMap)

  def withFallback(fallback: MotionConfig): MotionConfig =
    val resolvedFamilies =
      MotionFamily.values.map(family => family -> families.getOrElse(family, fallback.families(family))).toMap
    val pinnedPanels = resolvedFamilies(MotionFamily.PinnedPanels)
    copy(families =
      resolvedFamilies.updated(
        MotionFamily.PinnedPanels,
        pinnedPanels.copy(transitionOverrides =
          Map(
            TransitionScope.PanelOpen  -> pinnedPanels.transitionKindFor(TransitionScope.PanelOpen),
            TransitionScope.PanelClose -> pinnedPanels.transitionKindFor(TransitionScope.PanelClose)
          )
        )
      )
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

  /** Normal per-family values for a named baseline, before an accessibility override is applied. */
  def forPreset(preset: MotionPreset): MotionConfig =
    fromLegacy(SurfaceConfig(motionPreset = preset), preset, useBaselineAnimations = true)

  def fromLegacy(config: SurfaceConfig): MotionConfig =
    fromLegacy(config, config.motionPreset, useBaselineAnimations = false)

  def fromLegacy(config: SurfaceConfig, baseline: MotionPreset): MotionConfig =
    fromLegacy(config, baseline, useBaselineAnimations = true)

  private def fromLegacy(
    config: SurfaceConfig,
    baseline: MotionPreset,
    useBaselineAnimations: Boolean
  ): MotionConfig =
    val base                 = baseline.animationConfig
    val commandAnimation     = if useBaselineAnimations then base else config.commandRunnerAnimation
    val uiAnimation          = if useBaselineAnimations then base else config.uiAnimation
    val commandTransition    = config.commandRunnerTransitionKind.getOrElse(TransitionKind.Fade)
    val panelOpenTransition  = config.panelOpenTransitionKind.getOrElse(TransitionKind.OutlineThenContent)
    val panelCloseTransition = config.panelCloseTransitionKind.getOrElse(TransitionKind.Fade)
    MotionConfig(
      accessibility = MotionAccessibility.Standard,
      baseline = baseline,
      families = Map(
        MotionFamily.Cursor -> MotionFamilyConfig(
          enabled = true,
          transitionKind = TransitionKind.Fade,
          animation = base,
          speedScale = config.legacyCursorTransitionSpeedScale
        ),
        MotionFamily.EditorText -> MotionFamilyConfig(
          enabled = config.editorInsertionTransitionKind != TransitionKind.Disabled,
          transitionKind = config.editorInsertionTransitionKind,
          animation = base,
          speedScale = config.legacyEditorTextTransitionSpeedScale
        ),
        MotionFamily.CommandSurfaces -> MotionFamilyConfig(
          enabled = commandTransition != TransitionKind.Disabled,
          transitionKind = commandTransition,
          animation = commandAnimation,
          speedScale = config.legacyCommandRunnerTransitionSpeedScale
        ),
        MotionFamily.PinnedPanels -> MotionFamilyConfig(
          enabled = panelOpenTransition != TransitionKind.Disabled || panelCloseTransition != TransitionKind.Disabled,
          transitionKind = panelOpenTransition,
          animation = uiAnimation,
          speedScale = config.legacyUiTransitionSpeedScale,
          transitionOverrides = Map(
            TransitionScope.PanelOpen  -> panelOpenTransition,
            TransitionScope.PanelClose -> panelCloseTransition
          )
        ),
        MotionFamily.UiTransitions -> MotionFamilyConfig(
          enabled = true,
          transitionKind = TransitionKind.Fade,
          animation = uiAnimation,
          speedScale = config.legacyUiTransitionSpeedScale
        )
      )
    )
