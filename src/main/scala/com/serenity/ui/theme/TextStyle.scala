package com.serenity.ui.theme

import java.awt.Color

case class TextStyle(
    isBold: Boolean = false,
    isItalic: Boolean = false,
    isUnderlined: Boolean = false
):

  def combine(other: TextStyle): TextStyle =
    TextStyle(
      isBold = other.isBold || this.isBold,
      isItalic = other.isItalic || this.isItalic,
      isUnderlined = other.isUnderlined || this.isUnderlined
    )

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
