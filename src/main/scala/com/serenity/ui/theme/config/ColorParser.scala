package com.serenity.ui.theme.config

import scala.util.Try

import com.googlecode.lanterna.TextColor

object ColorParser:

  /** Parse a color string to Lanterna TextColor */
  def parseColor(colorStr: String): Either[String, TextColor] =
    colorStr.trim match
      case hex if hex.startsWith("#") => parseHexColor(hex)
      case rgb if rgb.startsWith("rgb(") => parseRgbColor(rgb)
      case unknown => Left(s"Unknown color: $unknown")

  /** Parse hex color (#RRGGBB or #RGB) */
  private def parseHexColor(hex: String): Either[String, TextColor] =
    Try {
      val cleanHex = hex.substring(1) // Remove #
      val (r, g, b) = cleanHex.length match
        case 3 => // #RGB -> #RRGGBB
          val r = Integer.parseInt(cleanHex.substring(0, 1), 16) * 17
          val g = Integer.parseInt(cleanHex.substring(1, 2), 16) * 17
          val b = Integer.parseInt(cleanHex.substring(2, 3), 16) * 17
          (r, g, b)
        case 6 => // #RRGGBB
          val r = Integer.parseInt(cleanHex.substring(0, 2), 16)
          val g = Integer.parseInt(cleanHex.substring(2, 4), 16)
          val b = Integer.parseInt(cleanHex.substring(4, 6), 16)
          (r, g, b)
        case _ => throw new IllegalArgumentException(s"Invalid hex color format: $hex")

      new TextColor.RGB(r, g, b)
    }.toEither.left.map(_.getMessage)

  /** Parse RGB color (rgb(r,g,b)) */
  private def parseRgbColor(rgb: String): Either[String, TextColor] =
    Try {
      val content = rgb.substring(4, rgb.length - 1) // Remove "rgb(" and ")"
      val parts   = content.split(",").map(_.trim.toInt)
      if parts.length != 3 then throw new IllegalArgumentException(s"RGB color must have 3 components: $rgb")

      val Array(r, g, b) = parts
      if r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255 then
        throw new IllegalArgumentException(s"RGB values must be 0-255: $rgb")

      new TextColor.RGB(r, g, b)
    }.toEither.left.map(_.getMessage)
