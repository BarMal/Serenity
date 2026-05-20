package com.serenity.ui.theme

import com.googlecode.lanterna.TextColor

case class TextStyle(
    isBold: Boolean = false,
    isItalic: Boolean = false,
    isUnderlined: Boolean = false
):
  /** Combine this style with another, with the other style taking precedence for conflicting attributes */
  def combine(other: TextStyle): TextStyle =
    TextStyle(
      isBold = other.isBold || this.isBold,
      isItalic = other.isItalic || this.isItalic,
      isUnderlined = other.isUnderlined || this.isUnderlined
    )

object TextStyle:
  def normal: TextStyle = TextStyle()
  def bold: TextStyle = TextStyle(isBold = true)
  def italic: TextStyle = TextStyle(isItalic = true)
  def underlined: TextStyle = TextStyle(isUnderlined = true)
  def boldItalic: TextStyle = TextStyle(isBold = true, isItalic = true)

case class StyledText(
    content: String,
    style: TextStyle = TextStyle.normal,
    foregroundColor: TextColor = TextColor.ANSI.WHITE,
    backgroundColor: TextColor = TextColor.ANSI.BLACK
)