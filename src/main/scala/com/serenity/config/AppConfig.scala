package com.serenity.config

import com.serenity.animation.AnimationConfig

/** Global application configuration */
case class AppConfig(
  characterAnimation: Option[AnimationConfig] = AnimationConfig.none,
  syntaxHighlightingEnabled: Boolean = false
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

object AppConfig:
  /** Default configuration with no animations and syntax highlighting disabled */
  val default: AppConfig = AppConfig()
  
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