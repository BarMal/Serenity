package com.serenity.ui.theme

import java.util.Locale

import com.serenity.ui.color.RenderColor

/** What a drop cap's glyph is painted in; it never affects layout. */
enum DropCapColour:
  /** The normal foreground colour. */
  case Text

  /** What `headings.colour` makes of a level 1 heading, in rich text, where drop caps are drawn. */
  case Heading

  /** The theme's highlighted foreground, which the terminal's one-cell fallback has always used. */
  case Accent
  case Fixed(colour: RenderColor)

  def resolve(theme: Theme, headings: HeadingSettings): RenderColor =
    this match
      case Text         => theme.foreground
      case Heading      => headings.colourFor(1, markdown = false).resolve(theme)
      case Accent       => theme.highlighted.foreground
      case Fixed(color) => color

object DropCapColour:

  private val HexColour = "#([0-9a-f]{6}|[0-9a-f]{8})".r

  /** How the config file and the session spell a colour: `text`, `heading`, `accent` or `#RRGGBB`. */
  def configKey(colour: DropCapColour): String =
    colour match
      case Text         => "text"
      case Heading      => "heading"
      case Accent       => "accent"
      case Fixed(color) => ColorFormat.toHex(color, withAlpha = true)

  def fromConfigKey(text: String): Option[DropCapColour] =
    text.trim.toLowerCase(Locale.ROOT) match
      case "text"            => Some(Text)
      case "heading"         => Some(Heading)
      case "accent"          => Some(Accent)
      case HexColour(digits) => Some(Fixed(fromHex(digits)))
      case _                 => None

  private def fromHex(digits: String): RenderColor =
    val rgb   = Integer.parseInt(digits.take(6), 16)
    val alpha = if digits.length == 8 then Integer.parseInt(digits.drop(6), 16) else 255
    RenderColor.fromRgba((rgb >> 16) & 0xff, (rgb >> 8) & 0xff, rgb & 0xff, alpha)
