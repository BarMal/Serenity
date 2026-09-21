package com.serenity.ui.renderer

import com.serenity.config.AppConfig
import com.serenity.ui.layout.{SpacingScale, SpacingStep}

/** How far a surface's text sits inside the content rect the frame laid out for it, beyond the whole cell
  * [[com.serenity.ui.layout.SurfaceFrameLayout]] already reserves for the border. A cell is the smallest inset a frame
  * can express and it is narrower than it is tall, so one cell alone leaves glyphs closer to the border horizontally
  * than vertically, and closest of all to the rounded corner the border is drawn with.
  *
  * Deliberately smaller than a cell: a row's text is measured and truncated against the full content width, so an inset
  * at or beyond a cell would push a full-width row's last glyph past the margin the border sits in.
  *
  * Shared by [[TextOverlayRenderer]] (floating surfaces) and [[PinnedPanelRenderer]] (docked panels) so the two
  * families of framed surfaces -- which every other piece of border/frame chrome already treats identically -- can't
  * drift into computing "the same" inset two different ways.
  */
object SurfaceTextInset:

  def px(config: AppConfig): Double =
    SpacingScale
      .forUi(config.editorConfig.fontConfig.scaledUiFontSize.toDouble, config.interfaceDensity)
      .px(SpacingStep.Sm)
      .toDouble
