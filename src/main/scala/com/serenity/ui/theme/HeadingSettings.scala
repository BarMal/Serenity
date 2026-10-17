package com.serenity.ui.theme

import com.serenity.richtext.ParagraphAlignment

/** The heading look a layout resolves its paragraphs with. The per-level text styles are resolved once here, so styling
  * a heading run is a lookup.
  */
final case class HeadingSettings(
    styles: Vector[HeadingStyle] = HeadingStyle.defaults,
    logicalBodyPt: Float = RichTextStyling.ProseZoomBaselinePx,
    dampingThresholdPt: Float = HeadingScale.DefaultDampingThresholdPt,
    dampingOn: Boolean = true,
    globalColour: Option[HeadingColour] = None
):

  private val multiples: Vector[Float] =
    styles.map(style => HeadingScale.effective(style.sizeScale, logicalBodyPt, dampingThresholdPt, dampingOn))

  private val textStyles: Vector[TextStyle] =
    styles.zip(multiples).map { (style, multiple) =>
      TextStyle(
        isBold = style.bold,
        isItalic = style.italic,
        fontFamily = style.family,
        fontSize = Some(multiple * RichTextStyling.ProseZoomBaselinePx)
      )
    }

  /** A level outside 1 to 6 takes the smallest heading's, as it always has. */
  private def indexOf(level: Int): Int =
    if level >= 1 && level <= HeadingStyle.LevelCount then level - 1 else HeadingStyle.LevelCount - 1

  def textStyle(level: Int): TextStyle = textStyles(indexOf(level))

  /** How many times the body size `level` is set at, after damping. */
  def sizeMultiple(level: Int): Float = multiples(indexOf(level))

  /** The colour `level` is set to on its own; none follows the global setting. */
  def colour(level: Int): Option[HeadingColour] = styles(indexOf(level)).colour

  /** What `level` is painted in: its own colour, else the global one, else what a document of that kind has always had
    * -- the theme's heading colour in Markdown, the text colour in rich text.
    */
  def colourFor(level: Int, markdown: Boolean): HeadingColour =
    colour(level).orElse(globalColour).getOrElse(if markdown then HeadingColour.Heading else HeadingColour.Text)

  def alignment(level: Int): Option[ParagraphAlignment] =
    styles(math.max(1, math.min(level, HeadingStyle.LevelCount)) - 1).align

object HeadingSettings:

  val default: HeadingSettings = HeadingSettings()
