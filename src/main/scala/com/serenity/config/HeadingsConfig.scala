package com.serenity.config

import com.serenity.richtext.ParagraphAlignment
import com.serenity.ui.theme.{HeadingColour, HeadingScale, HeadingSettings, HeadingStyle}

/** One heading level's overrides under `headings.hN.*`; `None` is `auto`, the level's own default. */
final case class HeadingLevelConfig(
    size: Option[Float] = None,
    bold: Option[Boolean] = None,
    italic: Option[Boolean] = None,
    family: Option[String] = None,
    colour: Option[HeadingColour] = None,
    align: Option[ParagraphAlignment] = None,
    spaceBefore: Option[Float] = None,
    spaceAfter: Option[Float] = None,
    underline: Option[Boolean] = None,
    rule: Option[Boolean] = None
):

  /** The level's style. A terminal's auto is its ladder, a pixel surface's is today's look; a rule is only ever drawn in
    * cells, and is given the row of space below the heading it needs unless `space_after` says otherwise.
    */
  def resolved(level: Int, terminal: Boolean = false): HeadingStyle =
    val base      = if terminal then HeadingStyle.terminalDefault(level) else HeadingStyle.default(level)
    val ruled     = terminal && rule.getOrElse(false)
    val spaceRule = if ruled then base.spaceAfterEm.max(HeadingsConfig.TerminalRuleSpaceEm) else base.spaceAfterEm
    base.copy(
      sizeScale = size.getOrElse(base.sizeScale),
      bold = bold.getOrElse(base.bold),
      italic = italic.getOrElse(base.italic),
      family = family.orElse(base.family),
      colour = colour.orElse(base.colour),
      align = align.orElse(base.align),
      spaceBeforeEm = spaceBefore.getOrElse(base.spaceBeforeEm),
      spaceAfterEm = spaceAfter.getOrElse(spaceRule),
      underline = underline.getOrElse(base.underline),
      rule = ruled
    )

/** The `headings.*` settings: per-level overrides, and the damping that stops a large body producing enormous headings.
  */
final case class HeadingsConfig(
    levels: Vector[HeadingLevelConfig] = Vector.fill(HeadingStyle.LevelCount)(HeadingLevelConfig()),
    colour: Option[HeadingColour] = None,
    damping: Boolean = true,
    dampingThresholdPt: Float = HeadingScale.DefaultDampingThresholdPt
):

  def withLevel(level: Int)(update: HeadingLevelConfig => HeadingLevelConfig): HeadingsConfig =
    if level < 1 || level > HeadingStyle.LevelCount then this
    else copy(levels = levels.updated(level - 1, update(levels(level - 1))))

  def styles: Vector[HeadingStyle] = stylesFor(terminal = false)

  def stylesFor(terminal: Boolean): Vector[HeadingStyle] =
    levels.zipWithIndex.map((config, index) => config.resolved(index + 1, terminal))

  def settings(logicalBodyPt: Float, terminal: Boolean = false): HeadingSettings =
    val styles = stylesFor(terminal)
    val damps = damping && logicalBodyPt > dampingThresholdPt
    // Settings that damp nothing are spelled one way, so changing them does not look like a change to layout.
    val built =
      if damps then HeadingSettings(styles, logicalBodyPt, dampingThresholdPt, dampingOn = true, globalColour = colour)
      else
        HeadingSettings(
          styles,
          logicalBodyPt,
          HeadingScale.DefaultDampingThresholdPt,
          dampingOn = logicalBodyPt <= HeadingScale.DefaultDampingThresholdPt,
          globalColour = colour
        )
    if built == HeadingSettings.default then HeadingSettings.default else built

object HeadingsConfig:
  /** The room under a heading a terminal gives its rule when `space_after` is left auto: one row. */
  val TerminalRuleSpaceEm: Float     = 1.0f
  val SizeRange: (Float, Float)      = (1.0f, 8.0f)
  val SpaceRange: (Float, Float)     = (0.0f, 8.0f)
  val ThresholdRange: (Float, Float) = (8.0f, 144.0f)
