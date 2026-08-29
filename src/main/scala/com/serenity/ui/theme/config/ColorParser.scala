package com.serenity.ui.theme.config

import java.awt.Color

import scala.util.Try

import com.serenity.ui.theme.ColorFormat.withAlpha

object ColorParser:

  /** `color` with alpha zeroed out, otherwise identical -- the starting/ending point for every fade-in/fade-out
    * animation that needs a transparent variant of a theme color.
    */
  def transparent(color: Color): Color =
    color.withAlpha(0)

  def parseColor(colorStr: String): Either[String, Color] =
    colorStr.trim match
      case hex if hex.startsWith("#")    => parseHexColor(hex)
      case rgb if rgb.startsWith("rgb(") => parseRgbColor(rgb)
      case unknown                       => Left(s"Unknown color: $unknown")

  private def parseHexColor(hex: String): Either[String, Color] =
    val cleanHex = hex.substring(1)
    // Invalid hex digits are a genuinely malformed color string, not our own control-flow choice --
    // Integer.parseInt's NumberFormatException is caught by Try and reported through the Either, same
    // as before. The length check below is our own validation and is expressed directly as Left/Right
    // rather than throw-then-catch.
    cleanHex.length match
      case 3 =>
        Try {
          val r = Integer.parseInt(cleanHex.substring(0, 1), 16) * 17
          val g = Integer.parseInt(cleanHex.substring(1, 2), 16) * 17
          val b = Integer.parseInt(cleanHex.substring(2, 3), 16) * 17
          new Color(r, g, b)
        }.toEither.left.map(_.getMessage)
      case 6 =>
        Try {
          val r = Integer.parseInt(cleanHex.substring(0, 2), 16)
          val g = Integer.parseInt(cleanHex.substring(2, 4), 16)
          val b = Integer.parseInt(cleanHex.substring(4, 6), 16)
          new Color(r, g, b)
        }.toEither.left.map(_.getMessage)
      case _ => Left(s"Invalid hex color format: $hex")

  private def parseRgbColor(rgb: String): Either[String, Color] =
    val content = rgb.substring(4, rgb.length - 1)
    Try(content.split(",").map(_.trim.toInt)).toEither.left
      .map(_.getMessage)
      .flatMap { parts =>
        if parts.length != 3 then Left(s"RGB color must have 3 components: $rgb")
        else
          val Array(r, g, b) = parts: @unchecked
          if r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255 then Left(s"RGB values must be 0-255: $rgb")
          else Right(new Color(r, g, b))
      }
