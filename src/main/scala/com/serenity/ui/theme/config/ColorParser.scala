package com.serenity.ui.theme.config

import java.awt.Color
import scala.util.Try

object ColorParser:

  def parseColor(colorStr: String): Either[String, Color] =
    colorStr.trim match
      case hex if hex.startsWith("#") => parseHexColor(hex)
      case rgb if rgb.startsWith("rgb(") => parseRgbColor(rgb)
      case unknown => Left(s"Unknown color: $unknown")

  private def parseHexColor(hex: String): Either[String, Color] =
    Try {
      val cleanHex = hex.substring(1)
      val (r, g, b) = cleanHex.length match
        case 3 =>
          val r = Integer.parseInt(cleanHex.substring(0, 1), 16) * 17
          val g = Integer.parseInt(cleanHex.substring(1, 2), 16) * 17
          val b = Integer.parseInt(cleanHex.substring(2, 3), 16) * 17
          (r, g, b)
        case 6 =>
          val r = Integer.parseInt(cleanHex.substring(0, 2), 16)
          val g = Integer.parseInt(cleanHex.substring(2, 4), 16)
          val b = Integer.parseInt(cleanHex.substring(4, 6), 16)
          (r, g, b)
        case _ => throw new IllegalArgumentException(s"Invalid hex color format: $hex")

      new Color(r, g, b)
    }.toEither.left.map(_.getMessage)

  private def parseRgbColor(rgb: String): Either[String, Color] =
    Try {
      val content = rgb.substring(4, rgb.length - 1)
      val parts   = content.split(",").map(_.trim.toInt)
      if parts.length != 3 then throw new IllegalArgumentException(s"RGB color must have 3 components: $rgb")

      val Array(r, g, b) = parts
      if r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255 then
        throw new IllegalArgumentException(s"RGB values must be 0-255: $rgb")

      new Color(r, g, b)
    }.toEither.left.map(_.getMessage)
