package com.serenity.ui.theme

import com.serenity.richtext.ParagraphAlignment

/** The heading look a layout resolves its paragraphs with. The per-level text styles are resolved once here, so styling
  * a heading run is a lookup.
  */
final case class HeadingSettings(
    styles: Vector[HeadingStyle] = HeadingStyle.defaults,
    logicalBodyPt: Float = RichTextStyling.ProseZoomBaselinePx,
    dampingThresholdPt: Float = HeadingScale.DefaultDampingThresholdPt,
    dampingOn: Boolean = true
):

  private val textStyles: Vector[TextStyle] =
    styles.map { style =>
      val multiple = HeadingScale.effective(style.sizeScale, logicalBodyPt, dampingThresholdPt, dampingOn)
      TextStyle(
        isBold = style.bold,
        isItalic = style.italic,
        fontFamily = style.family,
        fontSize = Some(multiple * RichTextStyling.ProseZoomBaselinePx)
      )
    }

  /** The style for `level`; a level outside 1 to 6 takes the smallest heading's, as it always has. */
  def textStyle(level: Int): TextStyle =
    if level >= 1 && level <= HeadingStyle.LevelCount then textStyles(level - 1)
    else textStyles(HeadingStyle.LevelCount - 1)

  def alignment(level: Int): Option[ParagraphAlignment] =
    styles(math.max(1, math.min(level, HeadingStyle.LevelCount)) - 1).align

object HeadingSettings:

  val default: HeadingSettings = HeadingSettings()
