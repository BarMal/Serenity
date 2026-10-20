package com.serenity.ui.theme

import java.awt.Font
import java.awt.font.TextAttribute

import scala.jdk.CollectionConverters.*

import com.serenity.ui.color.RenderColor

final case class TextStyle(
    isBold: Boolean = false,
    isItalic: Boolean = false,
    isUnderlined: Boolean = false,
    fontFamily: Option[String] = None,
    fontSize: Option[Float] = None,
    isStrikethrough: Boolean = false
):

  def combine(other: TextStyle): TextStyle =
    TextStyle(
      isBold = other.isBold || this.isBold,
      isItalic = other.isItalic || this.isItalic,
      isUnderlined = other.isUnderlined || this.isUnderlined,
      fontFamily = other.fontFamily.orElse(this.fontFamily),
      fontSize = other.fontSize.orElse(this.fontSize),
      isStrikethrough = other.isStrikethrough || this.isStrikethrough
    )

  def fontMode: Int =
    (if isBold then Font.BOLD else 0) |
      (if isItalic then Font.ITALIC else 0)

object TextStyle:
  def normal: TextStyle     = TextStyle()
  def bold: TextStyle       = TextStyle(isBold = true)
  def italic: TextStyle     = TextStyle(isItalic = true)
  def underlined: TextStyle = TextStyle(isUnderlined = true)
  def boldItalic: TextStyle = TextStyle(isBold = true, isItalic = true)

  /** Derive the concrete AWT font a style paints (and is measured) with, over a base font. This is the single source
    * both `Java2DRenderSurface.enableStyle` (drawing) and the measured-layout caret measurement use, so a run's glyph
    * advances always match the glyphs actually drawn.
    *
    * An underline is not part of the font: the surface draws it as a rule under the run, so an underlined run shares
    * the font of its plain counterpart. Strikethrough is still a font attribute.
    */
  def styledFont(base: Font, style: TextStyle): Font =
    val fontMode = style.fontMode
    val size     = style.fontSize.getOrElse(base.getSize2D).max(1.0f)
    val styled = style.fontFamily match
      case Some(family) => Font(family, fontMode, size.round.max(1)).deriveFont(fontMode, size)
      case None         => base.deriveFont(fontMode, size)
    if style.isStrikethrough then
      val strike: Map[TextAttribute, AnyRef] = Map(TextAttribute.STRIKETHROUGH -> TextAttribute.STRIKETHROUGH_ON)
      styled.deriveFont(strike.asJava)
    else styled

final case class StyledText(
    content: String,
    style: TextStyle = TextStyle.normal,
    foregroundColor: RenderColor = RenderColor.White,
    backgroundColor: RenderColor = RenderColor.Black
)

object StyledText:

  /** Whether the segments' contents, concatenated, are exactly `text` -- without building the concatenation. */
  def spell(segments: List[StyledText], text: String): Boolean =
    @annotation.tailrec
    def loop(remaining: List[StyledText], offset: Int): Boolean =
      remaining match
        case Nil          => offset == text.length
        case head :: tail => text.startsWith(head.content, offset) && loop(tail, offset + head.content.length)
    loop(segments, 0)
