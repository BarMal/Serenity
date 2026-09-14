package com.serenity.ui.theme

import java.awt.font.TextAttribute
import java.awt.{Color, Font}

import scala.jdk.CollectionConverters.*

final case class TextStyle(
    isBold: Boolean = false,
    isItalic: Boolean = false,
    isUnderlined: Boolean = false,
    fontFamily: Option[String] = None,
    fontSize: Option[Float] = None
):

  def combine(other: TextStyle): TextStyle =
    TextStyle(
      isBold = other.isBold || this.isBold,
      isItalic = other.isItalic || this.isItalic,
      isUnderlined = other.isUnderlined || this.isUnderlined,
      fontFamily = other.fontFamily.orElse(this.fontFamily),
      fontSize = other.fontSize.orElse(this.fontSize)
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
    */
  def styledFont(base: Font, style: TextStyle): Font =
    val fontMode = style.fontMode
    val size     = style.fontSize.getOrElse(base.getSize2D).max(1.0f)
    val styled = style.fontFamily match
      case Some(family) => Font(family, fontMode, size.round.max(1)).deriveFont(fontMode, size)
      case None         => base.deriveFont(fontMode, size)
    if style.isUnderlined then styled.deriveFont(Map(TextAttribute.UNDERLINE -> TextAttribute.UNDERLINE_ON).asJava)
    else styled

final case class StyledText(
    content: String,
    style: TextStyle = TextStyle.normal,
    foregroundColor: Color = Color.WHITE,
    backgroundColor: Color = Color.BLACK
)
