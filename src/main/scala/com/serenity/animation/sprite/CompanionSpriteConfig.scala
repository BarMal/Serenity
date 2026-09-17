package com.serenity.animation.sprite

import com.serenity.ui.layout.PanelPosition

/** Persisted controls for the companion sprite pane: an enabled toggle, the settings that shape it once on, and (issue
  * #934 v2) the typing-reactivity settings absorbed from the retired `com.serenity.animation.WindowSitterConfig` --
  * the companion sprite panel is now the one typing-reactive mascot, so there is no separate sitter config surface.
  */
final case class CompanionSpriteConfig(
    enabled: Boolean = false,
    character: CompanionCharacter = CompanionCharacter.default,
    position: PanelPosition = PanelPosition.Right,
    size: Int = CompanionSpriteConfig.DefaultSize,
    typingCycle: SpriteFrameCycle = SpriteFrameCycle.default,
    typingActiveTicks: Int = 8,
    typingFastActiveTicks: Int = 16,
    typingFastThresholdMs: Int = 150
):

  def normalized: CompanionSpriteConfig =
    copy(
      size = size.max(CompanionSpriteConfig.MinSize).min(CompanionSpriteConfig.MaxSize),
      typingActiveTicks = typingActiveTicks.max(1).min(120),
      typingFastActiveTicks = typingFastActiveTicks.max(1).min(240),
      typingFastThresholdMs = typingFastThresholdMs.max(1).min(5000)
    )

object CompanionSpriteConfig:
  val MinSize: Int     = 4
  val MaxSize: Int     = 40
  val DefaultSize: Int = 10

  val default: CompanionSpriteConfig = CompanionSpriteConfig()
