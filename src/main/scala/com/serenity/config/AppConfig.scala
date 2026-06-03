package com.serenity.config

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

/** Global application configuration */
case class AppConfig(
    characterAnimation: Option[AnimationConfig] = AnimationConfig.none,
    syntaxHighlightingEnabled: Boolean = false,
    fontConfig: FontConfig = FontConfig(),
    minimumPaneWidth: Int = 50,
    showLineNumbers: Boolean = true,
    showGutter: Boolean = true,
    blurRadius: Float = 0.0f,
    backgroundStyle: BackgroundStyle = BackgroundStyle.Frosted,
    cursorMode: CursorMode = CursorMode.Blink,
    windowChromeMode: WindowChromeMode = WindowChromeMode.Native
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

  def withWindowChromeMode(mode: WindowChromeMode): AppConfig =
    copy(windowChromeMode = mode)

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
