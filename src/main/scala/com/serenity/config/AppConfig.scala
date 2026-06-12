package com.serenity.config

import java.awt.Color

import com.serenity.animation.AnimationConfig
import com.serenity.lsp.config.LspUserConfig
import com.serenity.ui.fonts.FontLoader.FontConfig

enum BackgroundStyle:
  case Solid
  case Transparent
  case Frosted
  case GlassLike

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

enum WindowChromeMode:
  case Native
  case Custom

enum MarkdownViewMode:
  case Source
  case SplitPreview
  case InlineLens

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
    lineNumberTopInset: Int,
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
          lineNumberTopInset = 0,
          gutterHeight = 1,
          overlayGapRows = 0,
          commandSurfaceMaxHeight = 6,
          commandSurfaceMinHeight = 3,
          commandSurfaceVerticalPadding = 2
        )
      case InterfaceDensity.Comfortable =>
        InterfaceDensityMetrics(
          editorSpacerPercentage = 0.15,
          lineNumberTopInset = 1,
          gutterHeight = 1,
          overlayGapRows = 1,
          commandSurfaceMaxHeight = 8,
          commandSurfaceMinHeight = 4,
          commandSurfaceVerticalPadding = 3
        )
      case InterfaceDensity.Spacious =>
        InterfaceDensityMetrics(
          editorSpacerPercentage = 0.22,
          lineNumberTopInset = 2,
          gutterHeight = 2,
          overlayGapRows = 2,
          commandSurfaceMaxHeight = 10,
          commandSurfaceMinHeight = 5,
          commandSurfaceVerticalPadding = 4
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
  val DefaultInset: Double     = 0.15
  val MaxInset: Double         = 0.45
  val MaxCombinedInset: Double = 0.8
  val MinTextAreaWidth: Double = 1.0 - MaxCombinedInset

  def fromPercent(left: Double, right: Double): TextAreaInsets =
    TextAreaInsets(left / 100.0, right / 100.0).normalized

  def clamp(value: Double): Double =
    value.max(0.0).min(MaxInset)

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
    blurRadius: Float = 0.0f,
    backgroundStyle: BackgroundStyle = BackgroundStyle.Frosted,
    cursorMode: CursorMode = CursorMode.Blink,
    cursorColors: CursorColorConfig = CursorColorConfig(),
    cursorInfoBarMode: CursorInfoBarMode = CursorInfoBarMode.Off,
    windowChromeMode: WindowChromeMode = WindowChromeMode.Native,
    markdownViewMode: MarkdownViewMode = MarkdownViewMode.Source,
    interfaceDensity: InterfaceDensity = InterfaceDensity.Comfortable,
    textAreaInsets: TextAreaInsets = TextAreaInsets(),
    preferredWindowSize: Option[PreferredWindowSize] = None,
    lspUserConfig: LspUserConfig = LspUserConfig.empty
):
  /** Create a new config with character animation enabled */
  def withCharacterAnimation(config: AnimationConfig): AppConfig =
    copy(characterAnimation = Some(config))

  /** Create a new config with character animation disabled */
  def withoutCharacterAnimation: AppConfig =
    copy(characterAnimation = None)

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

  def withBlurRadius(r: Float): AppConfig =
    copy(blurRadius = r.max(0.0f).min(1.0f))

  def withBackgroundStyle(style: BackgroundStyle): AppConfig =
    copy(backgroundStyle = style)

  def withCursorMode(mode: CursorMode): AppConfig =
    copy(cursorMode = mode)

  def withCursorColors(colors: CursorColorConfig): AppConfig =
    copy(cursorColors = colors)

  def withCursorInfoBarMode(mode: CursorInfoBarMode): AppConfig =
    copy(cursorInfoBarMode = mode)

  def withWindowChromeMode(mode: WindowChromeMode): AppConfig =
    copy(windowChromeMode = mode)

  def withMarkdownViewMode(mode: MarkdownViewMode): AppConfig =
    copy(markdownViewMode = mode)

  def withInterfaceDensity(density: InterfaceDensity): AppConfig =
    copy(interfaceDensity = density)

  def withTextAreaInsets(insets: TextAreaInsets): AppConfig =
    copy(textAreaInsets = insets.normalized)

  def withTextAreaLeftInset(value: Double): AppConfig =
    withTextAreaInsets(textAreaInsets.copy(left = value))

  def withTextAreaRightInset(value: Double): AppConfig =
    withTextAreaInsets(textAreaInsets.copy(right = value))

  def withPreferredWindowSize(size: PreferredWindowSize): AppConfig =
    copy(preferredWindowSize = Some(size.normalized))

  def withLspUserConfig(config: LspUserConfig): AppConfig =
    copy(lspUserConfig = config)

object AppConfig:

  /** Default configuration with smooth animations and syntax highlighting disabled */
  val default: AppConfig = AppConfig(
    characterAnimation = AnimationConfig.smooth,
    syntaxHighlightingEnabled = false,
    blurRadius = 0.3f,
    backgroundStyle = BackgroundStyle.Frosted
  )

  /** Test configuration with visible animations enabled */
  val withTestAnimations: AppConfig = AppConfig(
    characterAnimation = AnimationConfig.quick,
    syntaxHighlightingEnabled = false
  )

  /** Quick fade-in animation configuration */
  val withQuickAnimation: AppConfig = AppConfig(
    characterAnimation = AnimationConfig.quick,
    syntaxHighlightingEnabled = false
  )

  /** Smooth fade-in animation configuration */
  val withSmoothAnimation: AppConfig = AppConfig(
    characterAnimation = AnimationConfig.smooth,
    syntaxHighlightingEnabled = false
  )

  /** Subtle fade-in animation configuration */
  val withSubtleAnimation: AppConfig = AppConfig(
    characterAnimation = AnimationConfig.subtle,
    syntaxHighlightingEnabled = false
  )
