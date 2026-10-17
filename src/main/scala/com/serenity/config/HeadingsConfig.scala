package com.serenity.config

import com.serenity.richtext.ParagraphAlignment
import com.serenity.ui.theme.{HeadingScale, HeadingSettings, HeadingStyle}

/** One heading level's overrides under `headings.hN.*`; `None` is `auto`, the level's own default. */
final case class HeadingLevelConfig(
    size: Option[Float] = None,
    bold: Option[Boolean] = None,
    italic: Option[Boolean] = None,
    family: Option[String] = None,
    align: Option[ParagraphAlignment] = None
):

  def resolved(level: Int): HeadingStyle =
    val base = HeadingStyle.default(level)
    base.copy(
      sizeScale = size.getOrElse(base.sizeScale),
      bold = bold.getOrElse(base.bold),
      italic = italic.getOrElse(base.italic),
      family = family.orElse(base.family),
      align = align.orElse(base.align)
    )

/** The `headings.*` settings: per-level overrides, and the damping that stops a large body producing enormous headings.
  */
final case class HeadingsConfig(
    levels: Vector[HeadingLevelConfig] = Vector.fill(HeadingStyle.LevelCount)(HeadingLevelConfig()),
    damping: Boolean = true,
    dampingThresholdPt: Float = HeadingScale.DefaultDampingThresholdPt
):

  def withLevel(level: Int)(update: HeadingLevelConfig => HeadingLevelConfig): HeadingsConfig =
    if level < 1 || level > HeadingStyle.LevelCount then this
    else copy(levels = levels.updated(level - 1, update(levels(level - 1))))

  def styles: Vector[HeadingStyle] =
    levels.zipWithIndex.map((config, index) => config.resolved(index + 1))

  def settings(logicalBodyPt: Float): HeadingSettings =
    val damps = damping && logicalBodyPt > dampingThresholdPt
    // Settings that damp nothing are spelled one way, so changing them does not look like a change to layout.
    val built =
      if damps then HeadingSettings(styles, logicalBodyPt, dampingThresholdPt, dampingOn = true)
      else
        HeadingSettings(
          styles,
          logicalBodyPt,
          HeadingScale.DefaultDampingThresholdPt,
          dampingOn = logicalBodyPt <= HeadingScale.DefaultDampingThresholdPt
        )
    if built == HeadingSettings.default then HeadingSettings.default else built

object HeadingsConfig:
  val SizeRange: (Float, Float)      = (1.0f, 8.0f)
  val ThresholdRange: (Float, Float) = (8.0f, 144.0f)
