package com.serenity.manuscript.typography

enum MeasureError:
  case MissingGlyph(codePoint: Int, family: FontFamily, style: FaceStyle)
  case FaceUnavailable(family: FontFamily, style: FaceStyle)

  def message: String = this match
    case MissingGlyph(codePoint, family, style) =>
      f"${family.displayName} $style has no glyph for U+$codePoint%04X"
    case FaceUnavailable(family, style) =>
      s"${family.displayName} $style is not loaded"

/** Measures text in points, from the same font file the PDF will embed, so what the paginator lays out is what the
  * painter draws.
  */
trait TextMeasurer:

  /** One advance per Unicode code point of `text`. A code point the face has no glyph for is an error, never zero. */
  def advances(text: String, font: FontSpec): Either[MeasureError, IArray[Float]]

  def lineMetrics(font: FontSpec): Either[MeasureError, LineMetrics]
