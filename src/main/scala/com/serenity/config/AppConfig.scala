package com.serenity.config

import java.awt.Color

import com.serenity.animation.*
import com.serenity.lsp.config.LspUserConfig
import com.serenity.ui.fonts.FontLoader.FontConfig

enum BackgroundStyle:
  case Solid
  case Transparent
  case Frosted
  case GlassLike

enum MaterialPreset(val configKey: String):
  case Solid   extends MaterialPreset("solid")
  case Clear   extends MaterialPreset("clear")
  case Frosted extends MaterialPreset("frosted")
  case Crystal extends MaterialPreset("crystal")
  case Custom  extends MaterialPreset("custom")

  def backgroundStyle: BackgroundStyle =
    this match
      case Solid   => BackgroundStyle.Solid
      case Clear   => BackgroundStyle.Transparent
      case Frosted => BackgroundStyle.Frosted
      case Crystal => BackgroundStyle.GlassLike
      case Custom  => BackgroundStyle.Frosted

  def blurRadius: Float =
    this match
      case Solid | Clear => 0.0f
      case Frosted       => 0.3f
      case Crystal       => 0.65f
      case Custom        => 0.3f

enum MotionPreset(val configKey: String):
  case Reduced    extends MotionPreset("reduced")
  case Subtle     extends MotionPreset("subtle")
  case Smooth     extends MotionPreset("smooth")
  case Expressive extends MotionPreset("expressive")
  case Custom     extends MotionPreset("custom")

  def animationConfig: Option[AnimationConfig] =
    this match
      case Reduced    => AnimationConfig.none
      case Subtle     => AnimationConfig.subtle
      case Smooth     => AnimationConfig.smooth
      case Expressive => AnimationConfig.quick
      case Custom     => AnimationConfig.smooth

  def elementTransitionSettings: ElementTransitionSettings =
    this match
      case Reduced    => ElementTransitionSettings.disabled
      case Subtle     => ElementTransitionSettings.subtle
      case Smooth     => ElementTransitionSettings.smooth
      case Expressive => ElementTransitionSettings.expressive
      case Custom     => ElementTransitionSettings.smooth

enum CursorMode:
  case Blink
  case Breathe

enum CursorInfoBarMode:
  case Off
  case Position
  case Detailed

  def configKey: String =
    this match
      case Off      => "off"
      case Position => "position"
      case Detailed => "detailed"

enum CursorInfoBarPlacement(val configKey: String):
  case Floating     extends CursorInfoBarPlacement("floating")
  case PinnedBottom extends CursorInfoBarPlacement("pinned-bottom")

enum WindowChromeMode:
  case Native
  case Custom

enum MarkdownViewMode:
  case Source
  case SplitPreview
  case InlineLens

/** Default document mode for newly-created empty buffers. */
enum DefaultDocumentMode(val configKey: String):
  case PlainText extends DefaultDocumentMode("plain-text")
  case Markdown  extends DefaultDocumentMode("markdown")
  case RichText  extends DefaultDocumentMode("rich-text")

enum InterfaceDensity:
  case Compact
  case Comfortable
  case Spacious

  def configKey: String =
    this match
      case Compact     => "compact"
      case Comfortable => "comfortable"
      case Spacious    => "spacious"

case class InterfaceDensityMetrics(
    editorSpacerPercentage: Double,
    gutterHeight: Int,
    overlayGapRows: Int,
    commandSurfaceMaxHeight: Int,
    commandSurfaceMinHeight: Int,
    commandSurfaceVerticalPadding: Int
)

object InterfaceDensityMetrics:

  def forDensity(density: InterfaceDensity): InterfaceDensityMetrics =
    density match
      case InterfaceDensity.Compact =>
        InterfaceDensityMetrics(
          editorSpacerPercentage = 0.08,
          gutterHeight = 1,
          overlayGapRows = 0,
          commandSurfaceMaxHeight = 6,
          commandSurfaceMinHeight = 3,
          commandSurfaceVerticalPadding = 2
        )
      case InterfaceDensity.Comfortable =>
        InterfaceDensityMetrics(
          editorSpacerPercentage = 0.15,
          gutterHeight = 1,
          overlayGapRows = 1,
          commandSurfaceMaxHeight = 8,
          commandSurfaceMinHeight = 4,
          commandSurfaceVerticalPadding = 3
        )
      case InterfaceDensity.Spacious =>
        InterfaceDensityMetrics(
          editorSpacerPercentage = 0.22,
          gutterHeight = 2,
          overlayGapRows = 2,
          commandSurfaceMaxHeight = 10,
          commandSurfaceMinHeight = 5,
          commandSurfaceVerticalPadding = 4
        )

case class SpellCheckConfig(
    enabled: Boolean = false,
    languages: List[String] = List("en"),
    additionalWords: List[String] = Nil
):

  def normalized: SpellCheckConfig =
    copy(
      languages = languages.map(_.trim.toLowerCase).filter(_.nonEmpty).distinct,
      additionalWords = additionalWords.map(_.trim.toLowerCase).filter(_.nonEmpty).distinct
    )

case class PreferredWindowSize(width: Int, height: Int):
  def normalized: PreferredWindowSize =
    PreferredWindowSize(width.max(400), height.max(300))

case class CursorColorConfig(
    active: Option[Color] = None,
    inactive: Option[Color] = None
):
  def activeOr(default: Color): Color =
    active.getOrElse(default)

  def inactiveOr(activeColor: Color): Color =
    inactive.getOrElse(activeColor)

case class TextAreaInsets(
    left: Double = TextAreaInsets.DefaultInset,
    right: Double = TextAreaInsets.DefaultInset
):

  def normalized: TextAreaInsets =
    val normalizedLeft  = TextAreaInsets.clamp(left)
    val normalizedRight = TextAreaInsets.clamp(right)
    val total           = normalizedLeft + normalizedRight
    if total <= TextAreaInsets.MaxCombinedInset then copy(left = normalizedLeft, right = normalizedRight)
    else
      val scale = TextAreaInsets.MaxCombinedInset / total
      copy(left = normalizedLeft * scale, right = normalizedRight * scale)

  def leftPercent: Double =
    left * 100.0

  def rightPercent: Double =
    right * 100.0

object TextAreaInsets:
  val DefaultInset: Double     = 0.0
  val MaxInset: Double         = 0.45
  val MaxCombinedInset: Double = 0.8
  val MinTextAreaWidth: Double = 1.0 - MaxCombinedInset

  def fromPercent(left: Double, right: Double): TextAreaInsets =
    TextAreaInsets(left / 100.0, right / 100.0).normalized

  def clamp(value: Double): Double =
    value.max(0.0).min(MaxInset)

/** Relative plus optional bounded sizing for one viewport axis. */
case class ViewportAxisSizing(
    percent: Double = 1.0,
    maxCells: Option[Int] = None
):

  def normalized: ViewportAxisSizing =
    copy(
      percent = ViewportAxisSizing.clampPercent(percent),
      maxCells = maxCells.map(_.max(1))
    )

  def percentValue: Double =
    normalized.percent * 100.0

  def resolve(availableCells: Int): Int =
    val relativeCells = math.max(1, (availableCells.max(1) * normalized.percent).toInt)
    normalized.maxCells.fold(relativeCells)(maxCells => math.min(relativeCells, maxCells))

object ViewportAxisSizing:
  val MinPercent: Double = 0.01
  val MaxPercent: Double = 1.0

  def fromPercent(percent: Double, maxCells: Option[Int] = None): ViewportAxisSizing =
    ViewportAxisSizing(percent / 100.0, maxCells).normalized

  def clampPercent(value: Double): Double =
    value.max(MinPercent).min(MaxPercent)

/** Configurable editor viewport sizing within each pane's available text area. */
case class ViewportSizing(
    width: ViewportAxisSizing = ViewportAxisSizing(),
    height: ViewportAxisSizing = ViewportAxisSizing()
):

  def normalized: ViewportSizing =
    copy(width = width.normalized, height = height.normalized)

/** Global application configuration */
case class AppConfig(
    characterAnimation: Option[AnimationConfig] = AnimationConfig.none,
    syntaxHighlightingEnabled: Boolean = false,
    hotkeyConfig: HotkeyConfig = HotkeyConfig(),
    focusedKeymapConfig: FocusedKeymapConfig = FocusedKeymapConfig(),
    fontConfig: FontConfig = FontConfig(),
    minimumPaneWidth: Int = 50,
    showLineNumbers: Boolean = true,
    showGutter: Boolean = true,
    wordWrapEnabled: Boolean = true,
    blurRadius: Float = 0.0f,
    backgroundStyle: BackgroundStyle = BackgroundStyle.Frosted,
    materialPreset: MaterialPreset = MaterialPreset.Frosted,
    motionPreset: MotionPreset = MotionPreset.Reduced,
    elementTransitionSpeedScale: Double = 1.0,
    commandRunnerAnimation: Option[AnimationConfig] = AnimationConfig.smooth,
    commandRunnerVisibleRows: Option[Int] = None,
    editorInsertionTransitionKind: TransitionKind = TransitionKind.Fade,
    cursorMode: CursorMode = CursorMode.Blink,
    cursorColors: CursorColorConfig = CursorColorConfig(),
    cursorInfoBarMode: CursorInfoBarMode = CursorInfoBarMode.Off,
    cursorInfoBarPlacement: CursorInfoBarPlacement = CursorInfoBarPlacement.Floating,
    windowChromeMode: WindowChromeMode = WindowChromeMode.Native,
    markdownViewMode: MarkdownViewMode = MarkdownViewMode.Source,
    defaultDocumentMode: DefaultDocumentMode = DefaultDocumentMode.PlainText,
    interfaceDensity: InterfaceDensity = InterfaceDensity.Comfortable,
    uiElementGap: Int = 0,
    uiCornerRadiusPx: Int = 8,
    textAreaInsets: TextAreaInsets = TextAreaInsets(),
    viewportSizing: ViewportSizing = ViewportSizing(),
    preferredWindowSize: Option[PreferredWindowSize] = None,
    lspUserConfig: LspUserConfig = LspUserConfig.empty,
    spellCheck: SpellCheckConfig = SpellCheckConfig()
):
  /** Create a new config with character animation enabled */
  def withCharacterAnimation(config: AnimationConfig): AppConfig =
    copy(characterAnimation = Some(config), motionPreset = MotionPreset.Custom)

  /** Create a new config with character animation disabled */
  def withoutCharacterAnimation: AppConfig =
    copy(characterAnimation = None, motionPreset = MotionPreset.Reduced, commandRunnerAnimation = None)

  /** Create a new config with syntax highlighting toggled */
  def withSyntaxHighlighting(enabled: Boolean): AppConfig =
    copy(syntaxHighlightingEnabled = enabled)

  def withHotkeyConfig(config: HotkeyConfig): AppConfig =
    copy(hotkeyConfig = config)

  def withHotkeyOverride(action: HotkeyAction, binding: String): AppConfig =
    copy(hotkeyConfig = hotkeyConfig.withBinding(action, binding))

  def resetHotkeyOverride(action: HotkeyAction): AppConfig =
    copy(hotkeyConfig = hotkeyConfig.resetBinding(action))

  def withFocusedKeymapConfig(config: FocusedKeymapConfig): AppConfig =
    copy(focusedKeymapConfig = config)

  def withEditorKeyOverride(action: EditorKeyAction, binding: String): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.withEditorBinding(action, binding))

  def resetEditorKeyOverride(action: EditorKeyAction): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.resetEditorBinding(action))

  def withCommandRunnerKeyOverride(action: CommandRunnerKeyAction, binding: String): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.withCommandRunnerBinding(action, binding))

  def resetCommandRunnerKeyOverride(action: CommandRunnerKeyAction): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.resetCommandRunnerBinding(action))

  def withModalKeyOverride(action: ModalKeyAction, binding: String): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.withModalBinding(action, binding))

  def resetModalKeyOverride(action: ModalKeyAction): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.resetModalBinding(action))

  def withPanelKeyOverride(action: PanelKeyAction, binding: String): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.withPanelBinding(action, binding))

  def resetPanelKeyOverride(action: PanelKeyAction): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.resetPanelBinding(action))

  def withPeekKeyOverride(action: PeekKeyAction, binding: String): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.withPeekBinding(action, binding))

  def resetPeekKeyOverride(action: PeekKeyAction): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.resetPeekBinding(action))

  /** Create a new config with font configuration */
  def withFontConfig(config: FontConfig): AppConfig =
    copy(fontConfig = config)

  /** Create a new config with minimum pane width setting */
  def withMinimumPaneWidth(width: Int): AppConfig =
    copy(minimumPaneWidth = math.max(1, width))

  /** Create a new config with line numbers toggled */
  def withLineNumbers(enabled: Boolean): AppConfig =
    copy(showLineNumbers = enabled)

  /** Create a new config with gutter toggled */
  def withGutter(enabled: Boolean): AppConfig =
    copy(showGutter = enabled)

  /** Create a new config with word wrapping toggled */
  def withWordWrap(enabled: Boolean): AppConfig =
    copy(wordWrapEnabled = enabled)

  def withBlurRadius(r: Float): AppConfig =
    copy(blurRadius = r.max(0.0f).min(1.0f), materialPreset = MaterialPreset.Custom)

  def withBackgroundStyle(style: BackgroundStyle): AppConfig =
    copy(backgroundStyle = style, materialPreset = MaterialPreset.Custom)

  def withMaterialPreset(preset: MaterialPreset): AppConfig =
    preset match
      case MaterialPreset.Custom =>
        copy(materialPreset = MaterialPreset.Custom)
      case _ =>
        copy(
          materialPreset = preset,
          backgroundStyle = preset.backgroundStyle,
          blurRadius = preset.blurRadius
        )

  def withMotionPreset(preset: MotionPreset): AppConfig =
    preset match
      case MotionPreset.Custom =>
        copy(motionPreset = MotionPreset.Custom)
      case _ =>
        copy(
          motionPreset = preset,
          characterAnimation = preset.animationConfig,
          commandRunnerAnimation = preset.animationConfig
        )

  /** Transition policy derived from the selected motion preset and global speed scale. */
  def elementTransitionSettings: ElementTransitionSettings =
    val baseSettings = motionPreset.elementTransitionSettings
    if !baseSettings.enabled then baseSettings
    else
      baseSettings.copy(
        speedScale = elementTransitionSpeedScale,
        overrides = baseSettings.overrides + (TransitionScope.EditorInsertion -> editorInsertionTransitionKind)
      )

  def withElementTransitionSpeedScale(scale: Double): AppConfig =
    copy(elementTransitionSpeedScale = AppConfig.clampElementTransitionSpeedScale(scale))

  def withCommandRunnerAnimation(animation: Option[AnimationConfig]): AppConfig =
    copy(commandRunnerAnimation = animation)

  def withCommandRunnerVisibleRows(rows: Option[Int]): AppConfig =
    copy(commandRunnerVisibleRows = rows.map(AppConfig.clampCommandRunnerVisibleRows))

  /** Character insertion animation after applying the global motion speed. */
  def scaledCharacterAnimation: Option[AnimationConfig] =
    AppConfig.scaledAnimation(characterAnimation, elementTransitionSpeedScale)

  /** Command runner animation after applying the global motion speed. */
  def scaledCommandRunnerAnimation: Option[AnimationConfig] =
    AppConfig.scaledAnimation(commandRunnerAnimation, elementTransitionSpeedScale)

  /** General UI animation after applying the global motion speed. */
  def scaledUiAnimation: Option[AnimationConfig] =
    AppConfig.scaledAnimation(characterAnimation, elementTransitionSpeedScale)

  def withEditorInsertionTransitionKind(kind: TransitionKind): AppConfig =
    copy(editorInsertionTransitionKind = kind)

  def withCursorMode(mode: CursorMode): AppConfig =
    copy(cursorMode = mode)

  def withCursorColors(colors: CursorColorConfig): AppConfig =
    copy(cursorColors = colors)

  def withCursorInfoBarMode(mode: CursorInfoBarMode): AppConfig =
    copy(cursorInfoBarMode = mode)

  def withCursorInfoBarPlacement(placement: CursorInfoBarPlacement): AppConfig =
    copy(cursorInfoBarPlacement = placement)

  def withWindowChromeMode(mode: WindowChromeMode): AppConfig =
    copy(windowChromeMode = mode)

  def withMarkdownViewMode(mode: MarkdownViewMode): AppConfig =
    copy(markdownViewMode = mode)

  /** Create a new config with the default mode used for new empty buffers. */
  def withDefaultDocumentMode(mode: DefaultDocumentMode): AppConfig =
    copy(defaultDocumentMode = mode)

  def withInterfaceDensity(density: InterfaceDensity): AppConfig =
    copy(interfaceDensity = density)

  def withUiElementGap(gap: Int): AppConfig =
    copy(uiElementGap = AppConfig.clampUiElementGap(gap))

  def withUiCornerRadiusPx(radius: Int): AppConfig =
    copy(uiCornerRadiusPx = AppConfig.clampUiCornerRadiusPx(radius))

  def withTextAreaInsets(insets: TextAreaInsets): AppConfig =
    copy(textAreaInsets = insets.normalized)

  def withTextAreaLeftInset(value: Double): AppConfig =
    withTextAreaInsets(textAreaInsets.copy(left = value))

  def withTextAreaRightInset(value: Double): AppConfig =
    withTextAreaInsets(textAreaInsets.copy(right = value))

  def withViewportSizing(sizing: ViewportSizing): AppConfig =
    copy(viewportSizing = sizing.normalized)

  def withViewportWidthSizing(sizing: ViewportAxisSizing): AppConfig =
    withViewportSizing(viewportSizing.copy(width = sizing))

  def withViewportHeightSizing(sizing: ViewportAxisSizing): AppConfig =
    withViewportSizing(viewportSizing.copy(height = sizing))

  def withPreferredWindowSize(size: PreferredWindowSize): AppConfig =
    copy(preferredWindowSize = Some(size.normalized))

  def withLspUserConfig(config: LspUserConfig): AppConfig =
    copy(lspUserConfig = config)

  def withSpellCheck(config: SpellCheckConfig): AppConfig =
    copy(spellCheck = config.normalized)

object AppConfig:

  val MinElementTransitionSpeedScale: Double = 0.0
  val MaxElementTransitionSpeedScale: Double = 4.0
  val MinUiElementGap: Int                   = 0
  val MaxUiElementGap: Int                   = 8
  val MinUiCornerRadiusPx: Int               = 0
  val MaxUiCornerRadiusPx: Int               = 32
  val MinCommandRunnerVisibleRows: Int       = 1
  val MaxCommandRunnerVisibleRows: Int       = 20

  def clampElementTransitionSpeedScale(scale: Double): Double =
    scale.max(MinElementTransitionSpeedScale).min(MaxElementTransitionSpeedScale)

  def clampUiElementGap(gap: Int): Int =
    gap.max(MinUiElementGap).min(MaxUiElementGap)

  def clampUiCornerRadiusPx(radius: Int): Int =
    radius.max(MinUiCornerRadiusPx).min(MaxUiCornerRadiusPx)

  def clampCommandRunnerVisibleRows(rows: Int): Int =
    rows.max(MinCommandRunnerVisibleRows).min(MaxCommandRunnerVisibleRows)

  def scaledAnimation(animation: Option[AnimationConfig], speedScale: Double): Option[AnimationConfig] =
    animation.flatMap(_.scaledBy(clampElementTransitionSpeedScale(speedScale)))

  /** Default configuration with smooth animations and syntax highlighting disabled */
  val default: AppConfig = AppConfig(
    characterAnimation = AnimationConfig.smooth,
    syntaxHighlightingEnabled = false,
    blurRadius = 0.3f,
    backgroundStyle = BackgroundStyle.Frosted,
    motionPreset = MotionPreset.Smooth
  )

  /** Test configuration with visible animations enabled */
  val withTestAnimations: AppConfig = AppConfig(
    characterAnimation = AnimationConfig.quick,
    syntaxHighlightingEnabled = false,
    motionPreset = MotionPreset.Expressive
  )

  /** Quick fade-in animation configuration */
  val withQuickAnimation: AppConfig = AppConfig(
    characterAnimation = AnimationConfig.quick,
    syntaxHighlightingEnabled = false,
    motionPreset = MotionPreset.Expressive
  )

  /** Smooth fade-in animation configuration */
  val withSmoothAnimation: AppConfig = AppConfig(
    characterAnimation = AnimationConfig.smooth,
    syntaxHighlightingEnabled = false,
    motionPreset = MotionPreset.Smooth
  )

  /** Subtle fade-in animation configuration */
  val withSubtleAnimation: AppConfig = AppConfig(
    characterAnimation = AnimationConfig.subtle,
    syntaxHighlightingEnabled = false,
    motionPreset = MotionPreset.Subtle
  )
