package com.serenity.ui.theme

import com.serenity.ui.color.RenderColor

/** What a heading is painted in when it is not set by the run itself. */
enum HeadingColour:
  /** The normal foreground colour. */
  case Text

  /** The theme's heading colour, which is its keyword colour unless the theme sets one. */
  case Heading

  /** The theme's accent, which is its caret colour. */
  case Accent

  /** The theme's string colour, its green, which neither its accent nor its heading colour is near. */
  case Secondary

  /** The theme's quiet text colour. */
  case Muted
  case Fixed(colour: RenderColor)

  def resolve(theme: Theme): RenderColor =
    this match
      case Text         => theme.foreground
      case Heading      => theme.colorFor(SyntaxElement.Heading).foreground
      case Accent       => theme.accent
      case Secondary    => theme.colorFor(SyntaxElement.String).foreground
      case Muted        => theme.muted
      case Fixed(color) => color
