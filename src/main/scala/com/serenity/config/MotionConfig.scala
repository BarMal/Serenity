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
  // Column-based document layout (issue #1338, Phase 1 animation): the transition painted between one column's
  // content and the next when `ColumnLeft`/`ColumnRight` moves the active column. A genuinely new family rather than
  // folded into `EditorText` or `UiTransitions` -- it has its own on/off switch, speed and transition-kind, exactly
  // like every sibling family, and nothing about it is really "editor text" (no glyphs change) or a generic UI
  // transition (it is specific to column-mode paging).
  case ColumnTransitions extends MotionFamily("column_transitions")
  // Panel scale-in/out (issue #1085 phase 1): a pinned/docked panel growing from a collapsed rect at its anchor edge
  // to its full rect on open, and shrinking back on close. A genuinely new family, not folded into `PinnedPanels`'
  // `transitionOverrides` -- it has its own on/off switch and speed, so a user can turn off the colour fade
  // `PinnedPanels` already does while keeping this geometric grow/shrink, or vice versa (`PinnedPanelAnimations` seeds
  // each independently). `transitionKind` here is a nominal enabled/disabled marker only: the shape of this motion
  // (grow/shrink from the panel's docked edge) is fixed by `PanelPosition`, not chosen from `TransitionKind`'s cases.
  case PanelGeometry extends MotionFamily("panel_geometry")

/** Motion policy for one family before accessibility policy is applied.
  *
  * `enabled` is derived from `transitionKind` rather than stored: the two could otherwise disagree (`enabled = true`
  * with `transitionKind = Disabled`, or vice versa), which every read site below treats as meaning "disabled" anyway.
  * Deriving it removes that contradiction entirely instead of validating against it.
  */
final case class MotionFamilyConfig(
    transitionKind: TransitionKind,
    animation: Option[AnimationConfig],
    speedScale: Double,
    transitionOverrides: Map[TransitionScope, TransitionKind] = Map.empty
):

  def enabled: Boolean = transitionKind != TransitionKind.Disabled

  def normalized: MotionFamilyConfig =
    copy(speedScale = MotionConfig.clampSpeedScale(speedScale))

  def disabled: MotionFamilyConfig =
    copy(
      transitionKind = TransitionKind.Disabled,
      animation = None,
      speedScale = 0.0,
      transitionOverrides = transitionOverrides.view.mapValues(_ => TransitionKind.Disabled).toMap
    )

  def transitionKindFor(scope: TransitionScope): TransitionKind =
    transitionOverrides.getOrElse(scope, transitionKind)

object MotionFamilyConfig:
  val disabled: MotionFamilyConfig = MotionFamilyConfig(TransitionKind.Disabled, None, 0.0)

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
          transitionKind = TransitionKind.Fade,
          animation = base,
          speedScale = config.legacyCursorTransitionSpeedScale
        ),
        MotionFamily.EditorText -> MotionFamilyConfig(
          transitionKind = config.editorInsertionTransitionKind,
          animation = base,
          speedScale = config.legacyEditorTextTransitionSpeedScale
        ),
        MotionFamily.CommandSurfaces -> MotionFamilyConfig(
          transitionKind = commandTransition,
          animation = commandAnimation,
          speedScale = config.legacyCommandRunnerTransitionSpeedScale
        ),
        // `enabled` is derived from `transitionKind` (this family's open transition -- see `MotionFamilyConfig`'s
        // doc), so a legacy config with the open transition off but the close transition on now resolves this whole
        // family to disabled rather than the previous open-or-close union; nothing in this codebase's config
        // authoring surface produces that combination today.
        MotionFamily.PinnedPanels -> MotionFamilyConfig(
          transitionKind = panelOpenTransition,
          animation = uiAnimation,
          speedScale = config.legacyUiTransitionSpeedScale,
          transitionOverrides = Map(
            TransitionScope.PanelOpen  -> panelOpenTransition,
            TransitionScope.PanelClose -> panelCloseTransition
          )
        ),
        MotionFamily.UiTransitions -> MotionFamilyConfig(
          transitionKind = TransitionKind.Fade,
          animation = uiAnimation,
          speedScale = config.legacyUiTransitionSpeedScale
        ),
        // No legacy field mirrors this one (it postdates the legacy per-family fields entirely), so it always takes
        // the baseline animation and a neutral 1.0 speed scale -- a directional sweep reads as the natural shape for
        // a page-to-page transition (see `AnimationChoreography`/`RendererColumnTransition` for how it is lowered).
        MotionFamily.ColumnTransitions -> MotionFamilyConfig(
          transitionKind = TransitionKind.DirectionalSweep,
          animation = base,
          speedScale = 1.0
        ),
        // No legacy field mirrors this one either (see `ColumnTransitions`' note just above) -- `transitionKind` is
        // only the enabled/disabled marker `MotionFamilyConfig.enabled` derives from, not a chosen visual shape (see
        // `MotionFamily.PanelGeometry`'s doc comment), so `Fade` here means nothing beyond "on".
        MotionFamily.PanelGeometry -> MotionFamilyConfig(
          transitionKind = TransitionKind.Fade,
          animation = base,
          speedScale = 1.0
        )
      )
    )
