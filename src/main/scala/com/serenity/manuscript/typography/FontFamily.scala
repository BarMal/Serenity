package com.serenity.manuscript.typography

enum FaceStyle:
  case Regular, Italic, Bold, BoldItalic

/** A font family bundled under `src/main/resources/fonts`, with one file per [[FaceStyle]].
  *
  * Adding a family is adding a case here and bundling its files; presets, the measurer and the config pick it up
  * without an API change.
  */
enum FontFamily(val key: String, val displayName: String, private val filePrefix: String):
  case CourierPrime extends FontFamily("courier-prime", "Courier Prime", "CourierPrime")

  def resource(style: FaceStyle): String =
    val suffix = style match
      case FaceStyle.Regular    => "Regular"
      case FaceStyle.Italic     => "Italic"
      case FaceStyle.Bold       => "Bold"
      case FaceStyle.BoldItalic => "BoldItalic"
    s"/fonts/$filePrefix-$suffix.ttf"

final case class FontSpec(family: FontFamily, style: FaceStyle, sizePoints: Float)

/** Vertical metrics of a font at a size, all in points and positive. */
final case class LineMetrics(ascent: Float, descent: Float, lineGap: Float):
  def height: Float = ascent + descent + lineGap
