package com.serenity.ui.theme

import com.serenity.richtext.ParagraphAlignment
import com.serenity.ui.color.RenderColor

/** How one heading level looks. `sizeScale` is a multiple of the body size. */
final case class HeadingStyle(
    sizeScale: Float,
    bold: Boolean = true,
    italic: Boolean = false,
    family: Option[String] = None,
    colour: Option[RenderColor] = None,
    align: Option[ParagraphAlignment] = None,
    spaceBeforeEm: Float = 0.0f,
    spaceAfterEm: Float = 0.0f
)

object HeadingStyle:

  val LevelCount: Int = 6

  private val BaselinePt = RichTextStyling.ProseZoomBaselinePx

  /** Today's rich-text look: the 22/18/16/14pt sizes the editor has always used, as multiples of the 12pt baseline. */
  def default(level: Int): HeadingStyle =
    level match
      case 1 => HeadingStyle(22.0f / BaselinePt)
      case 2 => HeadingStyle(18.0f / BaselinePt)
      case 3 => HeadingStyle(16.0f / BaselinePt)
      case _ => HeadingStyle(14.0f / BaselinePt)

  val defaults: Vector[HeadingStyle] = Vector.tabulate(LevelCount)(index => default(index + 1))
