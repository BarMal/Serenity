package com.serenity.config

import java.awt.Color

import com.serenity.animation.AnimationConfig
import com.serenity.ui.fonts.FontLoader.FontConfig

enum BackgroundStyle:
  case Solid
  case Transparent
  case Frosted
  case GlassLike

enum CursorMode:
  case Blink
  case Breathe

enum WindowChromeMode:
  case Native
  case Custom

enum MarkdownViewMode:
  case Source
  case SplitPreview
  case InlineLens

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
    windowChromeMode: WindowChromeMode = WindowChromeMode.Native,
    markdownViewMode: MarkdownViewMode = MarkdownViewMode.Source,
    preferredWindowSize: Option[PreferredWindowSize] = None
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

  def withWindowChromeMode(mode: WindowChromeMode): AppConfig =
    copy(windowChromeMode = mode)

  def withMarkdownViewMode(mode: MarkdownViewMode): AppConfig =
    copy(markdownViewMode = mode)

  def withPreferredWindowSize(size: PreferredWindowSize): AppConfig =
    copy(preferredWindowSize = Some(size.normalized))

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
