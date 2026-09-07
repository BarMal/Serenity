package com.serenity.config

import com.serenity.animation.*
import com.serenity.keystroke.Modifier
import com.serenity.state.models.SurfacePlacement

final case class SurfaceConfig(
    showLineNumbers: Boolean = true,
    showGutter: Boolean = true,
    showPaneHeaders: Boolean = true,
    // Opt-in (like `cursorInfoBarMode`): the status bar's default text (position/language/file) is covered by
    // exact-string assertions elsewhere, so this ships off and callers turn it on explicitly (#1203).
    showWordCount: Boolean = false,
    // Opt-in like `showWordCount`/`cursorInfoBarMode`: existing click/mouse-hit-testing and comment-lens behaviour
    // is covered by exact-state assertions elsewhere, so this defaults to the smaller, non-disruptive mode and
    // callers opt into margin mode explicitly once it ships (#1222).
    commentDisplayMode: CommentDisplayMode = CommentDisplayMode.Floating,
    wordWrapEnabled: Boolean = true,
    // Whether Up/Down under word wrap follow visual rows (the wrapped screen line) rather than jumping straight to
    // the previous/next logical line. Independent of wordWrapEnabled itself: only takes effect while wrap is also on.
    visualLineCursorNavigation: Boolean = true,
    // Off by default (preserves `CursorViewport.adjustForCursor`'s existing behaviour exactly): the cursor's line is
    // recentred on every move, but never past the document's own end, so a viewport near the last line falls back to
    // showing as much real content as fits rather than centring. On, that end clamp is lifted -- the caret's line
    // stays at its centred row even while typing at the very end of the document, padding with blank rows below it
    // the way iA Writer/Ulysses-style typewriter scrolling does (#1204, #1293).
    typewriterScrollingEnabled: Boolean = false,
    focusedTextBodyEnabled: Boolean = false,
    contextualToolbarEnabled: Boolean = true,
    contextualToolbarDisplayMode: ToolbarDisplayMode = ToolbarDisplayMode.IconAndText,
    blurRadius: Float = 0.18f,
    backgroundStyle: BackgroundStyle = BackgroundStyle.Frosted,
    materialPreset: MaterialPreset = MaterialPreset.Frosted,
    postProcessingEffect: PostProcessingEffect = PostProcessingEffect.Off,
    uiShadowsEnabled: Boolean = true,
    motionPreset: MotionPreset = MotionPreset.Reduced,
    elementTransitionSpeedScale: Double = 1.0,
    editorTextTransitionSpeedScale: Option[Double] = None,
    commandRunnerTransitionSpeedScale: Option[Double] = None,
    uiTransitionSpeedScale: Option[Double] = None,
    cursorTransitionSpeedScale: Option[Double] = None,
    commandRunnerAnimation: Option[AnimationConfig] = AnimationConfig.smooth,
    uiAnimation: Option[AnimationConfig] = AnimationConfig.smooth,
    commandRunnerVisibleRows: Option[Int] = None,
    commandRunnerItemGapRows: Double = 0.0,
    commandRunnerCursorGapRows: Option[Double] = None,
    // Opt-out (unlike `showWordCount`/`commentDisplayMode`): the persistent key-hint footer (issue #931, Stage 3) is
    // the discoverability fix the stage exists to deliver, so it ships on by default; callers who want the old
    // dynamic-footer-only behaviour turn it off explicitly.
    commandRunnerShowKeyHints: Boolean = true,
    // Experimental prototype (off by default, unlike `commandRunnerShowKeyHints`): holding or double-tapping a bare
    // modifier peeks/opens the command runner near the cursor line. Ships disabled -- this is a single-panel spike,
    // not the finished feature -- and callers opt in explicitly.
    commandRunnerCursorPeekEnabled: Boolean = false,
    // Configurable per-user given real risk of OS/WM collision with Super/Meta on Linux.
    commandRunnerCursorPeekModifier: Modifier = Modifier.Meta,
    // Hold-vs-double-tap threshold in milliseconds. Defaults to `ModifierTapDetector.WindowMillis` (200L) for
    // consistency with the codebase's existing bare-modifier double-tap window (`ctrl+ctrl`-style hotkeys) rather
    // than introducing a second magic number.
    commandRunnerCursorPeekTapWindowMillis: Long = 200L,
    commandRunnerCursorPeekPlacement: SurfacePlacement = SurfacePlacement.BelowCursor,
    renderFpsTarget: RenderFpsTarget = RenderFpsTarget.Fps60,
    renderDamageGranularity: RenderDamageGranularity = RenderDamageGranularity.Rows,
    editorInsertionTransitionKind: TransitionKind = TransitionKind.Fade,
    commandRunnerTransitionKind: Option[TransitionKind] = None,
    panelOpenTransitionKind: Option[TransitionKind] = None,
    panelCloseTransitionKind: Option[TransitionKind] = None,
    motionConfiguration: Option[MotionConfig] = None,
    textAreaInsets: TextAreaInsets = TextAreaInsets(),
    viewportSizing: ViewportSizing = ViewportSizing(),
    // None (default) keeps the active theme's own panel alpha, matching every other floating panel. Some overrides
    // just the cursor info bar's background alpha, independent of theme -- see `TextOverlayRenderer`'s per-row
    // background colour, the one paint site this is scoped to.
    cursorInfoBarBackgroundAlpha: Option[Double] = None
):

  def normalized: SurfaceConfig =
    copy(
      blurRadius = blurRadius.max(0.0f).min(1.0f),
      elementTransitionSpeedScale = AppConfig.clampElementTransitionSpeedScale(elementTransitionSpeedScale),
      editorTextTransitionSpeedScale = editorTextTransitionSpeedScale.map(AppConfig.clampElementTransitionSpeedScale),
      commandRunnerTransitionSpeedScale =
        commandRunnerTransitionSpeedScale.map(AppConfig.clampElementTransitionSpeedScale),
      uiTransitionSpeedScale = uiTransitionSpeedScale.map(AppConfig.clampElementTransitionSpeedScale),
      cursorTransitionSpeedScale = cursorTransitionSpeedScale.map(AppConfig.clampElementTransitionSpeedScale),
      motionConfiguration = motionConfiguration.map(_.normalized),
      commandRunnerVisibleRows = commandRunnerVisibleRows.map(AppConfig.clampCommandRunnerVisibleRows),
      commandRunnerItemGapRows = AppConfig.clampCommandRunnerItemGapRows(commandRunnerItemGapRows),
      commandRunnerCursorGapRows = commandRunnerCursorGapRows.map(AppConfig.clampCommandRunnerCursorGapRows),
      commandRunnerCursorPeekTapWindowMillis =
        AppConfig.clampCommandRunnerCursorPeekTapWindowMillis(commandRunnerCursorPeekTapWindowMillis),
      textAreaInsets = textAreaInsets.normalized,
      viewportSizing = viewportSizing.normalized,
      cursorInfoBarBackgroundAlpha = cursorInfoBarBackgroundAlpha.map(AppConfig.clampCursorInfoBarBackgroundAlpha)
    )

  /** Speed scales as the legacy fields alone describe them: a per-family override if there is one, otherwise the
    * element-wide scale.
    *
    * These are what [[MotionConfig.fromLegacy]] reads when it derives a hierarchy from a configuration that has none,
    * so they cannot themselves consult the hierarchy -- that is what the `effective*` accessors below are for, and
    * asking one of those here would be circular.
    */
  private[config] def legacyEditorTextTransitionSpeedScale: Double =
    editorTextTransitionSpeedScale.getOrElse(elementTransitionSpeedScale)

  private[config] def legacyCommandRunnerTransitionSpeedScale: Double =
    commandRunnerTransitionSpeedScale.getOrElse(elementTransitionSpeedScale)

  private[config] def legacyUiTransitionSpeedScale: Double =
    uiTransitionSpeedScale.getOrElse(elementTransitionSpeedScale)

  private[config] def legacyCursorTransitionSpeedScale: Double =
    cursorTransitionSpeedScale.getOrElse(elementTransitionSpeedScale)

  /** The speed scale configured for a family: an explicit per-family override if the user set one, otherwise whatever
    * the authoritative hierarchy holds, and the element-wide scale only when there is nothing else to go on. This is
    * the value the settings surface shows as current -- the runtime value the renderer plans with comes from
    * `elementTransitionSettings`, where a `Reduced` preset legitimately disables motion the user has scaled.
    *
    * The middle step is the one that was missing. A configuration loaded from a file carries its scales in the
    * hierarchy and its legacy fields at their defaults -- those `Option`s are not written, being a record of what was
    * set explicitly rather than settings in their own right -- so resolving from the legacy fields alone reported the
    * element-wide default for a family the file plainly gave a scale to, and the settings row showed the wrong number
    * after every restart.
    */
  private def configuredFamilySpeedScale(family: MotionFamily, explicit: Option[Double]): Double =
    explicit
      .orElse(motionConfiguration.map(_ => effectiveMotionConfiguration.family(family).speedScale))
      .getOrElse(elementTransitionSpeedScale)

  def effectiveEditorTextTransitionSpeedScale: Double =
    configuredFamilySpeedScale(MotionFamily.EditorText, editorTextTransitionSpeedScale)

  def effectiveCommandRunnerTransitionSpeedScale: Double =
    configuredFamilySpeedScale(MotionFamily.CommandSurfaces, commandRunnerTransitionSpeedScale)

  def effectiveUiTransitionSpeedScale: Double =
    configuredFamilySpeedScale(MotionFamily.UiTransitions, uiTransitionSpeedScale)

  def effectiveCursorTransitionSpeedScale: Double =
    configuredFamilySpeedScale(MotionFamily.Cursor, cursorTransitionSpeedScale)

  /** Resolve every runtime family from one hierarchy, preserving legacy fields when no hierarchy has been saved yet. */
  def effectiveMotionConfiguration: EffectiveMotionConfig =
    motionConfiguration match
      case Some(configuration) =>
        configuration.withFallback(MotionConfig.fromLegacy(this, configuration.baseline)).effective
      case None => MotionConfig.fromLegacy(this).effective

  def effectiveMotionBaseline: MotionPreset =
    motionConfiguration.fold(motionPreset)(_.baseline)

  def effectiveCommandRunnerTransitionKind: TransitionKind =
    motionConfiguration.fold(commandRunnerTransitionKind.getOrElse(TransitionKind.Fade))(_ =>
      effectiveMotionConfiguration.family(MotionFamily.CommandSurfaces).transitionKind
    )

  def effectivePanelOpenTransitionKind: TransitionKind =
    motionConfiguration.fold(panelOpenTransitionKind.getOrElse(TransitionKind.OutlineThenContent))(_ =>
      effectiveMotionConfiguration.family(MotionFamily.PinnedPanels).transitionKindFor(TransitionScope.PanelOpen)
    )

  def effectivePanelCloseTransitionKind: TransitionKind =
    motionConfiguration.fold(panelCloseTransitionKind.getOrElse(TransitionKind.Fade))(_ =>
      effectiveMotionConfiguration.family(MotionFamily.PinnedPanels).transitionKindFor(TransitionScope.PanelClose)
    )

  def elementTransitionSettings: ElementTransitionSettings =
    val uiMotion = effectiveMotionConfiguration.family(MotionFamily.UiTransitions)
    val baseSettings =
      if uiMotion.enabled then effectiveMotionBaseline.elementTransitionSettings else ElementTransitionSettings.disabled
    if !baseSettings.enabled then baseSettings
    else
      val transitionOverrides = motionConfiguration match
        case Some(_) =>
          List(
            TransitionScope.EditorInsertion -> effectiveMotionConfiguration
              .family(MotionFamily.EditorText)
              .transitionKind,
            TransitionScope.CommandRunner -> effectiveMotionConfiguration
              .family(MotionFamily.CommandSurfaces)
              .transitionKind,
            TransitionScope.PanelOpen -> effectiveMotionConfiguration
              .family(MotionFamily.PinnedPanels)
              .transitionKindFor(TransitionScope.PanelOpen),
            TransitionScope.PanelClose -> effectiveMotionConfiguration
              .family(MotionFamily.PinnedPanels)
              .transitionKindFor(TransitionScope.PanelClose)
          ).toMap
        case None =>
          List(
            Some(TransitionScope.EditorInsertion -> editorInsertionTransitionKind),
            commandRunnerTransitionKind.map(TransitionScope.CommandRunner -> _),
            panelOpenTransitionKind.map(TransitionScope.PanelOpen -> _),
            panelCloseTransitionKind.map(TransitionScope.PanelClose -> _)
          ).flatten.toMap

      baseSettings.copy(
        speedScale = uiMotion.speedScale,
        overrides = baseSettings.overrides ++ transitionOverrides
      )

  def editorInsertionTransitionSettings: ElementTransitionSettings =
    val editorMotion = effectiveMotionConfiguration.family(MotionFamily.EditorText)
    val baseSettings =
      if editorMotion.enabled then effectiveMotionBaseline.elementTransitionSettings
      else ElementTransitionSettings.disabled
    if !baseSettings.enabled then baseSettings
    else
      baseSettings.copy(
        speedScale = editorMotion.speedScale,
        overrides = baseSettings.overrides ++ Map(TransitionScope.EditorInsertion -> editorMotion.transitionKind)
      )

  /** Transition policy for pinned panels, with independent family timing and reveal strategy. */
  def pinnedPanelTransitionSettings: ElementTransitionSettings =
    motionConfiguration match
      case None => elementTransitionSettings
      case Some(_) =>
        val panelMotion = effectiveMotionConfiguration.family(MotionFamily.PinnedPanels)
        val baseSettings =
          if panelMotion.enabled then effectiveMotionBaseline.elementTransitionSettings
          else ElementTransitionSettings.disabled
        if !baseSettings.enabled then baseSettings
        else
          val timing = panelMotion.animation.fold(baseSettings.baseTiming)(animation =>
            baseSettings.baseTiming.copy(durationMs = animation.durationMs, staggerMs = animation.tickRateMs)
          )
          baseSettings.copy(
            baseTiming = timing,
            speedScale = panelMotion.speedScale,
            overrides = baseSettings.overrides ++ Map(
              TransitionScope.PanelOpen  -> panelMotion.transitionKindFor(TransitionScope.PanelOpen),
              TransitionScope.PanelClose -> panelMotion.transitionKindFor(TransitionScope.PanelClose)
            )
          )

object SurfaceConfig:

  /** The keys and parsing this format understands for surface settings, split into [[SurfaceConfigSchemaKeys]] and
    * [[SurfaceConfigSchemaParser]] to keep this file under the architecture ratchet's file-length target -- this object
    * re-exports their members, so callers see no difference.
    */
  object Schema:
    export SurfaceConfigSchemaKeys.*
    export SurfaceConfigSchemaParser.*
