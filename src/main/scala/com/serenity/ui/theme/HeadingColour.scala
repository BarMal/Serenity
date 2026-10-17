package com.serenity.ui.theme

import com.serenity.ui.color.RenderColor

/** What a heading is painted in when it is not set by the run itself. */
enum HeadingColour:
  /** The normal foreground colour. */
  case Text

  /** The theme's heading colour, which is its keyword colour unless the theme sets one. */
  case Heading
  case Fixed(colour: RenderColor)

  def resolve(theme: Theme): RenderColor =
    this match
      case Text         => theme.foreground
      case Heading      => theme.colorFor(SyntaxElement.Heading).foreground
      case Fixed(color) => color
