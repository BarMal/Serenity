package com.serenity.ui.theme

import com.serenity.richtext.ParagraphAlignment

/** How one heading level looks. `sizeScale` is a multiple of the body size. `ladderColour` is what the level is painted
  * in when neither it nor the global setting names a colour; `rule` is drawn only where there are cells to draw it in.
  */
final case class HeadingStyle(
    sizeScale: Float,
    bold: Boolean = true,
    italic: Boolean = false,
    family: Option[String] = None,
    colour: Option[HeadingColour] = None,
    align: Option[ParagraphAlignment] = None,
    spaceBeforeEm: Float = 0.0f,
    spaceAfterEm: Float = 0.0f,
    underline: Boolean = false,
    rule: Boolean = false,
    ladderColour: Option[HeadingColour] = None
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

  /** What the terminal makes of a level left on auto. A terminal has no sizes, so the levels climb a ladder of weight,
    * slant and underline, and the three smallest take a theme colour each: the accent, the heading colour and the quiet
    * text colour, all of which a theme already keeps readable on its background.
    */
  def terminalDefault(level: Int): HeadingStyle =
    val base = default(level)
    level match
      case 1 => base.copy(underline = true)
      case 3 => base.copy(italic = true)
      case 4 => base.copy(ladderColour = Some(HeadingColour.Accent))
      case 5 => base.copy(ladderColour = Some(HeadingColour.Heading))
      case 6 => base.copy(ladderColour = Some(HeadingColour.Muted))
      case _ => base
