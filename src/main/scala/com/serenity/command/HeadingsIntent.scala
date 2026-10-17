package com.serenity.command

import com.serenity.config.{HeadingLevelConfig, HeadingsConfig}
import com.serenity.richtext.ParagraphAlignment

/** One attribute of one heading level; `None` is auto, the level's own default. */
enum HeadingLevelChange:
  case Size(scale: Option[Float])
  case Bold(enabled: Option[Boolean])
  case Italic(enabled: Option[Boolean])
  case Family(family: Option[String])
  case Align(alignment: Option[ParagraphAlignment])

/** How headings are styled. Global only: a UI preset neither holds nor changes these. */
enum HeadingsIntent:
  case SetLevel(level: Int, change: HeadingLevelChange)
  case SetDamping(enabled: Boolean)
  case SetDampingThreshold(pointSize: Float)

/** What a heading intent does to a [[HeadingsConfig]]. */
object HeadingsConfigEdit:

  def apply(intent: HeadingsIntent): HeadingsConfig => HeadingsConfig =
    intent match
      case HeadingsIntent.SetLevel(level, change) => _.withLevel(level)(levelEdit(change))
      case HeadingsIntent.SetDamping(enabled)     => _.copy(damping = enabled)
      case HeadingsIntent.SetDampingThreshold(pt) => _.copy(dampingThresholdPt = pt)

  private def levelEdit(change: HeadingLevelChange): HeadingLevelConfig => HeadingLevelConfig =
    change match
      case HeadingLevelChange.Size(scale)      => _.copy(size = scale)
      case HeadingLevelChange.Bold(enabled)    => _.copy(bold = enabled)
      case HeadingLevelChange.Italic(enabled)  => _.copy(italic = enabled)
      case HeadingLevelChange.Family(family)   => _.copy(family = family)
      case HeadingLevelChange.Align(alignment) => _.copy(align = alignment)
