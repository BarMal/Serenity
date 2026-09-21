package com.serenity.config

import com.serenity.animation.*
import com.serenity.animation.sprite.CompanionSpriteConfig
import com.serenity.keystroke.Modifier
import com.serenity.keystroke.events.Event
import com.serenity.lsp.config.LspUserConfig
import com.serenity.state.models.SurfacePlacement
import com.serenity.ui.fonts.FontLoader.FontConfig

/** Global application configuration */
final case class AppConfig(
    editorConfig: EditorConfig = EditorConfig(),
    inputConfig: InputConfig = InputConfig(),
    surfaceConfig: SurfaceConfig = SurfaceConfig(),
    cursorConfig: CursorConfig = CursorConfig(),
    windowConfig: WindowConfig = WindowConfig(),
    companionSpriteConfig: CompanionSpriteConfig = CompanionSpriteConfig.default,
    visualFlairLevel: VisualFlairLevel = VisualFlairLevel.default,
    documentConfig: DocumentConfig = DocumentConfig(),
    interfaceConfig: InterfaceConfig = InterfaceConfig(),
    languageToolsConfig: LanguageToolsConfig = LanguageToolsConfig(),
    appModeConfig: AppModeConfig = AppModeConfig(),
    statusLine: StatusLineConfig = StatusLineConfig.default
):

  def withEditorConfig(config: EditorConfig): AppConfig =
    copy(editorConfig = config.normalized)

  def withLanguageToolsConfig(config: LanguageToolsConfig): AppConfig =
    copy(languageToolsConfig = config.normalized)

  def withInputConfig(config: InputConfig): AppConfig =
    copy(inputConfig = config)

  def withSurfaceConfig(config: SurfaceConfig): AppConfig =
    copy(surfaceConfig = config.normalized)

  def windowChromeMode: WindowChromeMode =
    windowConfig.chromeMode

  def preferredWindowSize: Option[PreferredWindowSize] =
    windowConfig.preferredSize

  def markdownViewMode: MarkdownViewMode =
    documentConfig.markdownViewMode

  def defaultDocumentMode: DefaultDocumentMode =
    documentConfig.defaultMode

  def appMode: AppMode =
    appModeConfig.mode

  def showAllSettingsRegardlessOfMode: Boolean =
    appModeConfig.showAllSettingsRegardlessOfMode

  def interfaceDensity: InterfaceDensity =
    interfaceConfig.density

  def uiElementGap: Option[Double] =
    interfaceConfig.elementGap

  def uiCornerRadiusPx: Int =
    interfaceConfig.cornerRadiusPx

  def uiOutlineThicknessPx: Int =
    interfaceConfig.outlineThicknessPx

  def uiChromeScale: Double =
    editorConfig.fontConfig.uiChromeScale

  /** [[uiCornerRadiusPx]] scaled by [[uiChromeScale]] -- what renderers should actually draw with, so a panel's corner
    * radius stays proportionate to the panel as the UI font size changes (issue #1542) instead of always painting the
    * configured pixel value regardless of font size.
    */
  def scaledUiCornerRadiusPx: Int =
    math.round(uiCornerRadiusPx * uiChromeScale).toInt.max(0)

  /** [[uiOutlineThicknessPx]] scaled the same way as [[scaledUiCornerRadiusPx]]; floored above zero since `BasicStroke`
    * requires a positive width.
    */
  def scaledUiOutlineThicknessPx: Float =
    (uiOutlineThicknessPx * uiChromeScale).toFloat.max(0.5f)

  /** Create a new config with syntax highlighting toggled */
  def withSyntaxHighlighting(enabled: Boolean): AppConfig =
    withLanguageToolsConfig(languageToolsConfig.copy(syntaxHighlightingEnabled = enabled))

  def withHotkeyConfig(config: HotkeyConfig): AppConfig =
    withInputConfig(inputConfig.copy(hotkeyConfig = config))

  def withHotkeyOverride(action: HotkeyAction, binding: String): AppConfig =
    withInputConfig(inputConfig.copy(hotkeyConfig = inputConfig.hotkeyConfig.withBinding(action, binding)))

  def withHotkeyOverrideUnbindingConflicts(action: HotkeyAction, binding: String): AppConfig =
    withInputConfig(
      inputConfig.copy(hotkeyConfig = inputConfig.hotkeyConfig.withBindingUnbindingConflicts(action, binding))
    )

  def resetHotkeyOverride(action: HotkeyAction): AppConfig =
    withInputConfig(inputConfig.copy(hotkeyConfig = inputConfig.hotkeyConfig.resetBinding(action)))

  def withFocusedKeymapConfig(config: FocusedKeymapConfig): AppConfig =
    withInputConfig(inputConfig.copy(focusedKeymapConfig = config))

  def withKeymapBinding[A <: KeymapEventAction[E], E <: Event](
    group: KeymapGroup[A, E]
  )(action: A, binding: String): AppConfig =
    withInputConfig(
      inputConfig.copy(focusedKeymapConfig = inputConfig.focusedKeymapConfig.withBinding(group)(action, binding))
    )

  def withKeymapBindingUnbindingConflicts[A <: KeymapEventAction[E], E <: Event](
    group: KeymapGroup[A, E]
  )(action: A, binding: String): AppConfig =
    withInputConfig(
      inputConfig.copy(focusedKeymapConfig =
        inputConfig.focusedKeymapConfig.withBindingUnbindingConflicts(group)(action, binding)
      )
    )

  def resetKeymapBinding[A <: KeymapEventAction[E], E <: Event](group: KeymapGroup[A, E])(action: A): AppConfig =
    withInputConfig(
      inputConfig.copy(focusedKeymapConfig = inputConfig.focusedKeymapConfig.resetBinding(group)(action))
    )

  /** Create a new config with font configuration */
  def withFontConfig(config: FontConfig): AppConfig =
    withEditorConfig(editorConfig.copy(fontConfig = config))

  /** Create a new config with minimum pane width setting */
  def withMinimumPaneWidth(width: Int): AppConfig =
    withEditorConfig(editorConfig.copy(minimumPaneWidth = width))

  /** Create a new config with line numbers toggled */
  def withLineNumbers(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(showLineNumbers = enabled))

  /** Placement and spacing of the line-number counter (independent of interface density). Clamps margins/padding. */
  def withLineNumberLayout(layout: LineNumberLayout): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(lineNumberLayout = layout.normalized))

  def lineNumberLayout: LineNumberLayout =
    surfaceConfig.lineNumberLayout

  /** Show or hide the per-pane identity strip above editor content. */
  def withPaneHeaders(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(showPaneHeaders = enabled))

  /** Selects how document comments become visible: floating on-demand lens or persistent margin (#1222). */
  def withCommentDisplayMode(mode: CommentDisplayMode): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commentDisplayMode = mode))

  /** Create a new config with word wrapping toggled */
  def withWordWrap(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(wordWrapEnabled = enabled))

  def withVisualLineCursorNavigation(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(visualLineCursorNavigation = enabled))

  def withTypewriterScrolling(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(typewriterScrollingEnabled = enabled))

  def withColumnMode(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(columnModeEnabled = enabled))

  def withColumnTargetWidth(cells: Int): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(columnTargetWidthCells = cells))

  def withColumnGap(cells: Int): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(columnGap = cells))

  def withFocusedTextBody(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(focusedTextBodyEnabled = enabled))

  /** Show or hide the command runner's persistent key-hint footer row (issue #931, Stage 3). */
  def withCommandRunnerShowKeyHints(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commandRunnerShowKeyHints = enabled))

  /** Enable or disable the experimental cursor-peek prototype (off by default). */
  def withCommandRunnerCursorPeekEnabled(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commandRunnerCursorPeekEnabled = enabled))

  def withCommandRunnerCursorPeekModifier(modifier: Modifier): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commandRunnerCursorPeekModifier = modifier))

  def withCommandRunnerCursorPeekTapWindowMillis(millis: Long): AppConfig =
    withSurfaceConfig(
      surfaceConfig.copy(commandRunnerCursorPeekTapWindowMillis =
        AppConfig.clampCommandRunnerCursorPeekTapWindowMillis(millis)
      )
    )

  def withCommandRunnerCursorPeekPlacement(placement: SurfacePlacement): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commandRunnerCursorPeekPlacement = placement))

  def withContextualToolbarEnabled(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(contextualToolbarEnabled = enabled))

  def withContextualToolbarDisplayMode(mode: ToolbarDisplayMode): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(contextualToolbarDisplayMode = mode))

  def withRendererFrameStateCacheCapacity(capacity: Int): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(rendererFrameStateCacheCapacity = capacity))

  def withDiagnosticHighlightBlendWeight(weight: Double): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(diagnosticHighlightBlendWeight = weight))

  // #1316: re-assigning a setting its own current value is not customising it -- flip the preset only when the value
  // actually changes, so putting back what was already there is a no-op.
  def withBlurRadius(r: Float): AppConfig =
    if surfaceConfig.blurRadius == r then this
    else withSurfaceConfig(surfaceConfig.copy(blurRadius = r, materialPreset = MaterialPreset.Custom))

  def withBackgroundStyle(style: BackgroundStyle): AppConfig =
    if surfaceConfig.backgroundStyle == style then this
    else withSurfaceConfig(surfaceConfig.copy(backgroundStyle = style, materialPreset = MaterialPreset.Custom))

  def withMaterialPreset(preset: MaterialPreset): AppConfig =
    preset match
      case MaterialPreset.Custom =>
        withSurfaceConfig(surfaceConfig.copy(materialPreset = MaterialPreset.Custom))
      case _ =>
        withSurfaceConfig(
          surfaceConfig.copy(
            materialPreset = preset,
            backgroundStyle = preset.backgroundStyle,
            blurRadius = preset.blurRadius
          )
        )

  def withPostProcessingEffect(effect: PostProcessingEffect): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(postProcessingEffect = effect))

  def withUiShadowsEnabled(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(uiShadowsEnabled = enabled))

  def cursorMode: CursorMode =
    cursorConfig.mode

  def cursorColors: CursorColorConfig =
    cursorConfig.colors

  def withCursorConfig(config: CursorConfig): AppConfig =
    copy(cursorConfig = config)

  def withCursorMode(mode: CursorMode): AppConfig =
    withCursorConfig(cursorConfig.copy(mode = mode))

  def withCursorColors(colors: CursorColorConfig): AppConfig =
    withCursorConfig(cursorConfig.copy(colors = colors))

  def withStatusLine(config: StatusLineConfig): AppConfig =
    copy(statusLine =
      config.copy(colors =
        config.colors.copy(backgroundAlpha = config.colors.backgroundAlpha.map(StatusLineConfig.clampBackgroundAlpha))
      )
    )

  def withStatusLineSegments(segments: List[StatusSegment]): AppConfig =
    withStatusLine(statusLine.copy(segments = segments))

  def withStatusLinePlacement(placement: StatusLinePlacement): AppConfig =
    withStatusLine(statusLine.copy(placement = placement))

  def withoutStatusLine: AppConfig =
    withStatusLinePlacement(StatusLinePlacement.Off)

  def withStatusLineColors(colors: StatusLineColors): AppConfig =
    withStatusLine(statusLine.copy(colors = colors))

  def withWindowConfig(config: WindowConfig): AppConfig =
    copy(windowConfig = config.normalized)

  def withWindowChromeMode(mode: WindowChromeMode): AppConfig =
    withWindowConfig(windowConfig.copy(chromeMode = mode))

  def withDocumentConfig(config: DocumentConfig): AppConfig =
    copy(documentConfig = config)

  def withMarkdownViewMode(mode: MarkdownViewMode): AppConfig =
    withDocumentConfig(documentConfig.copy(markdownViewMode = mode))

  /** Create a new config with the default mode used for new empty buffers. */
  def withDefaultDocumentMode(mode: DefaultDocumentMode): AppConfig =
    withDocumentConfig(documentConfig.copy(defaultMode = mode))

  def withAppModeConfig(config: AppModeConfig): AppConfig =
    copy(appModeConfig = config)

  def withAppMode(mode: AppMode): AppConfig =
    withAppModeConfig(appModeConfig.copy(mode = mode))

  def withShowAllSettingsRegardlessOfMode(value: Boolean): AppConfig =
    withAppModeConfig(appModeConfig.copy(showAllSettingsRegardlessOfMode = value))

  def withInterfaceConfig(config: InterfaceConfig): AppConfig =
    copy(interfaceConfig = config.normalized)

  def withInterfaceDensity(density: InterfaceDensity): AppConfig =
    withInterfaceConfig(interfaceConfig.copy(density = density))

  def withUiElementGap(gap: Option[Double]): AppConfig =
    withInterfaceConfig(interfaceConfig.copy(elementGap = gap))

  def withUiCornerRadiusPx(radius: Int): AppConfig =
    withInterfaceConfig(interfaceConfig.copy(cornerRadiusPx = radius))

  def withUiOutlineThicknessPx(thickness: Int): AppConfig =
    withInterfaceConfig(interfaceConfig.copy(outlineThicknessPx = thickness))

  def withTextAreaInsets(insets: TextAreaInsets): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(textAreaInsets = insets))

  def withTextAreaLeftInset(value: Double): AppConfig =
    withTextAreaInsets(surfaceConfig.textAreaInsets.copy(left = value))

  def withTextAreaRightInset(value: Double): AppConfig =
    withTextAreaInsets(surfaceConfig.textAreaInsets.copy(right = value))

  def withTextAreaTopInset(value: Double): AppConfig =
    withTextAreaInsets(surfaceConfig.textAreaInsets.copy(top = value))

  def withTextAreaBottomInset(value: Double): AppConfig =
    withTextAreaInsets(surfaceConfig.textAreaInsets.copy(bottom = value))

  def withViewportSizing(sizing: ViewportSizing): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(viewportSizing = sizing))

  def withViewportWidthSizing(sizing: ViewportAxisSizing): AppConfig =
    withViewportSizing(surfaceConfig.viewportSizing.copy(width = sizing))

  def withViewportHeightSizing(sizing: ViewportAxisSizing): AppConfig =
    withViewportSizing(surfaceConfig.viewportSizing.copy(height = sizing))

  def withPreferredWindowSize(size: PreferredWindowSize): AppConfig =
    withWindowConfig(windowConfig.copy(preferredSize = Some(size.normalized)))

  def withCompanionSpriteConfig(config: CompanionSpriteConfig): AppConfig =
    copy(companionSpriteConfig = config.normalized)

  def withVisualFlairLevel(level: VisualFlairLevel): AppConfig =
    copy(visualFlairLevel = level)

  def withLspUserConfig(config: LspUserConfig): AppConfig =
    withLanguageToolsConfig(languageToolsConfig.copy(lspUserConfig = config))

  def withSpellCheck(config: SpellCheckConfig): AppConfig =
    withLanguageToolsConfig(languageToolsConfig.copy(spellCheck = config))

object AppConfig:

  val MinElementTransitionSpeedScale: Double    = 0.0
  val MaxElementTransitionSpeedScale: Double    = 4.0
  val MinUiElementGap: Double                   = 0.0
  val MaxUiElementGap: Double                   = 8.0
  val MinUiCornerRadiusPx: Int                  = 0
  val MaxUiCornerRadiusPx: Int                  = 32
  val MinUiOutlineThicknessPx: Int              = 1
  val MaxUiOutlineThicknessPx: Int              = 8
  val MinCommandRunnerVisibleRows: Int          = 1
  val MaxCommandRunnerVisibleRows: Int          = 20
  val MinCommandRunnerItemGapRows: Double       = 0.0
  val MaxCommandRunnerItemGapRows: Double       = 8.0
  val MinCommandRunnerCursorGapRows: Double     = 0.0
  val MaxCommandRunnerCursorGapRows: Double     = 8.0
  val MinDiagnosticHighlightBlendWeight: Double = 0.0
  val MaxDiagnosticHighlightBlendWeight: Double = 1.0
  // Wide enough to allow a deliberately slow "hold" feel while still rejecting nonsensical (near-zero or
  // multi-second) values; 200 (the default, matching `ModifierTapDetector.WindowMillis`) sits well inside it.
  val MinCommandRunnerCursorPeekTapWindowMillis: Long = 50L
  val MaxCommandRunnerCursorPeekTapWindowMillis: Long = 2000L
  // Lower bound keeps RendererFrameState's caches usefully sized even at their floor; upper bound is a sanity
  // ceiling against a fat-fingered config value, not a meaningful capacity anyone would actually want (see #1433).
  val MinRendererFrameStateCacheCapacity: Int = 8
  val MaxRendererFrameStateCacheCapacity: Int = 4096

  def clampElementTransitionSpeedScale(scale: Double): Double =
    scale.max(MinElementTransitionSpeedScale).min(MaxElementTransitionSpeedScale)

  def clampUiElementGap(gap: Double): Double =
    if gap.isFinite then gap.max(MinUiElementGap).min(MaxUiElementGap) else MinUiElementGap

  def clampUiCornerRadiusPx(radius: Int): Int =
    radius.max(MinUiCornerRadiusPx).min(MaxUiCornerRadiusPx)

  def clampUiOutlineThicknessPx(thickness: Int): Int =
    thickness.max(MinUiOutlineThicknessPx).min(MaxUiOutlineThicknessPx)

  def clampWheelScrollLines(lines: Int): Int =
    lines.max(1).min(50)

  def clampCommandRunnerVisibleRows(rows: Int): Int =
    rows.max(MinCommandRunnerVisibleRows).min(MaxCommandRunnerVisibleRows)

  def clampCommandRunnerItemGapRows(rows: Double): Double =
    if rows.isFinite then rows.max(MinCommandRunnerItemGapRows).min(MaxCommandRunnerItemGapRows)
    else MinCommandRunnerItemGapRows

  def clampCommandRunnerCursorGapRows(rows: Double): Double =
    if rows.isFinite then rows.max(MinCommandRunnerCursorGapRows).min(MaxCommandRunnerCursorGapRows)
    else MinCommandRunnerCursorGapRows

  def clampCommandRunnerCursorPeekTapWindowMillis(millis: Long): Long =
    millis.max(MinCommandRunnerCursorPeekTapWindowMillis).min(MaxCommandRunnerCursorPeekTapWindowMillis)

  def clampRendererFrameStateCacheCapacity(capacity: Int): Int =
    capacity.max(MinRendererFrameStateCacheCapacity).min(MaxRendererFrameStateCacheCapacity)

  def clampDiagnosticHighlightBlendWeight(weight: Double): Double =
    if weight.isFinite then weight.max(MinDiagnosticHighlightBlendWeight).min(MaxDiagnosticHighlightBlendWeight)
    else MinDiagnosticHighlightBlendWeight

  def scaledAnimation(animation: Option[AnimationConfig], speedScale: Double): Option[AnimationConfig] =
    animation.flatMap(_.scaledBy(clampElementTransitionSpeedScale(speedScale)))

  /** Default configuration keeps text entry immediate and uses restrained frosted surfaces. */
  /** What the app ships with.
    *
    * Only the settings that differ from their own field's default belong here. Restating one that already matches hides
    * which of the two is the real answer -- four of these used to, and telling them apart meant reading both.
    * `ConfigRegistry.defaults` lists every setting's default, and `docs/default-config.conf` is generated from it.
    */
  val default: AppConfig = AppConfig(
    surfaceConfig = SurfaceConfig(motionPreset = MotionPreset.Smooth)
  )

  /** Test configuration with visible animations enabled */
  val withTestAnimations: AppConfig = AppConfig(
    editorConfig = EditorConfig(characterAnimation = AnimationConfig.quick),
    surfaceConfig = SurfaceConfig(
      uiAnimation = AnimationConfig.quick,
      motionPreset = MotionPreset.Expressive
    )
  )

  /** Quick fade-in animation configuration */
  val withQuickAnimation: AppConfig = AppConfig(
    editorConfig = EditorConfig(characterAnimation = AnimationConfig.quick),
    surfaceConfig = SurfaceConfig(
      uiAnimation = AnimationConfig.quick,
      motionPreset = MotionPreset.Expressive
    )
  )

  /** Smooth fade-in animation configuration */
  val withSmoothAnimation: AppConfig = AppConfig(
    editorConfig = EditorConfig(characterAnimation = AnimationConfig.smooth),
    surfaceConfig = SurfaceConfig(
      uiAnimation = AnimationConfig.smooth,
      motionPreset = MotionPreset.Smooth
    )
  )

  /** Subtle fade-in animation configuration */
  val withSubtleAnimation: AppConfig = AppConfig(
    editorConfig = EditorConfig(characterAnimation = AnimationConfig.subtle),
    surfaceConfig = SurfaceConfig(
      uiAnimation = AnimationConfig.subtle,
      motionPreset = MotionPreset.Subtle
    )
  )
