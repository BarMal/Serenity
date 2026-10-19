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
        isUnderlined = style.underline,
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

  /** What `level` is painted in: its own colour, else the global one, else the level's ladder colour, else what a
    * document of that kind has always had -- the theme's heading colour in Markdown, the text colour in rich text.
    */
  def colourFor(level: Int, markdown: Boolean): HeadingColour =
    colour(level)
      .orElse(globalColour)
      .orElse(styles(indexOf(level)).ladderColour)
      .getOrElse(if markdown then HeadingColour.Heading else HeadingColour.Text)

  /** Whether a rule is drawn under a `level` heading, in the row its space below gives it. */
  def rule(level: Int): Boolean = styles(indexOf(level)).rule

  def alignment(level: Int): Option[ParagraphAlignment] =
    styles(indexOf(level)).align

  /** The space above and below a `level` heading, in em of that heading's font size. */
  def spacing(level: Int): (Float, Float) =
    val style = styles(indexOf(level))
    (style.spaceBeforeEm, style.spaceAfterEm)

object HeadingSettings:

  val default: HeadingSettings = HeadingSettings()
