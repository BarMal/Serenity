package com.serenity.animation.sprite

import com.serenity.ui.layout.PanelPosition

/** Persisted controls for the companion sprite pane, mirroring `com.serenity.animation.WindowSitterConfig`'s shape: an
  * enabled toggle, plus the settings that shape it once on.
  */
final case class CompanionSpriteConfig(
    enabled: Boolean = false,
    character: CompanionCharacter = CompanionCharacter.default,
    position: PanelPosition = PanelPosition.Right,
    size: Int = CompanionSpriteConfig.DefaultSize
):

  def normalized: CompanionSpriteConfig =
    copy(size = size.max(CompanionSpriteConfig.MinSize).min(CompanionSpriteConfig.MaxSize))

object CompanionSpriteConfig:
  val MinSize: Int     = 4
  val MaxSize: Int     = 40
  val DefaultSize: Int = 10

  val default: CompanionSpriteConfig = CompanionSpriteConfig()
