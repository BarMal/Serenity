package com.serenity.ui.theme

import java.awt.{Color, Font}

case class TextStyle(
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

case class StyledText(
    content: String,
    style: TextStyle = TextStyle.normal,
    foregroundColor: Color = Color.WHITE,
    backgroundColor: Color = Color.BLACK
)
