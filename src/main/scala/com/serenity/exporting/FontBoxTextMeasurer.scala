package com.serenity.exporting

import java.io.IOException

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.serenity.manuscript.typography.*
import org.apache.fontbox.ttf.{CmapLookup, TrueTypeFont}

/** Measures with FontBox, from the bundled TrueType files the PDF will embed: an advance is the glyph's `hmtx` width
  * times size over `unitsPerEm`. No AWT is involved, so measurement agrees with painting exactly.
  */
final class FontBoxTextMeasurer private (faces: Map[(FontFamily, FaceStyle), FontBoxTextMeasurer.Face])
    extends TextMeasurer:

  import FontBoxTextMeasurer.Face

  def advances(text: String, font: FontSpec): Either[MeasureError, IArray[Float]] =
    face(font).flatMap(loaded =>
      text.codePoints.toArray.toList
        .traverse(codePoint => loaded.advanceUnits(codePoint).map(_.toFloat * font.sizePoints / loaded.unitsPerEm))
        .map(IArray.from(_))
        .leftMap(codePoint => MeasureError.MissingGlyph(codePoint, font.family, font.style))
    )

  def lineMetrics(font: FontSpec): Either[MeasureError, LineMetrics] =
    face(font).map { loaded =>
      val scale = font.sizePoints / loaded.unitsPerEm
      LineMetrics(loaded.ascender * scale, -loaded.descender * scale, loaded.lineGap * scale)
    }

  private def face(font: FontSpec): Either[MeasureError, Face] =
    faces.get((font.family, font.style)).toRight(MeasureError.FaceUnavailable(font.family, font.style))

object FontBoxTextMeasurer:

  /** One parsed face. `advanceUnits` is `Left(codePoint)` for a code point the face has no glyph for. */
  final private[exporting] class Face(
      font: TrueTypeFont,
      lookup: CmapLookup,
      val unitsPerEm: Int,
      val ascender: Int,
      val descender: Int,
      val lineGap: Int
  ):

    def advanceUnits(codePoint: Int): Either[Int, Int] =
      Some(lookup.getGlyphId(codePoint))
        .filter(_ > 0)
        .toRight(codePoint)
        .flatMap(glyph => Either.catchOnly[IOException](font.getAdvanceWidth(glyph)).leftMap(_ => codePoint))

    def close(): Unit = font.close()

  /** Every bundled face of the given families, parsed once and closed with the resource. */
  def resource(families: List[FontFamily] = FontFamily.values.toList): Resource[IO, FontBoxTextMeasurer] =
    val wanted = for family <- families; style <- FaceStyle.values.toList yield (family, style)
    wanted
      .traverse(key => faceResource(key._1, key._2).map(key -> _))
      .map(loaded => FontBoxTextMeasurer(loaded.toMap))

  private def faceResource(family: FontFamily, style: FaceStyle): Resource[IO, Face] =
    Resource.make(loadFace(family.resource(style)))(face => IO.blocking(face.close()))

  private def loadFace(resource: String): IO[Face] =
    BundledFonts
      .readResource(resource)
      .flatMap(bytes => IO.blocking(BundledFonts.parse(resource, bytes).flatMap(faceOf(resource, _))))
      .flatMap(IO.fromEither)

  private def faceOf(resource: String, font: TrueTypeFont): Either[FontLoadError, Face] =
    Either
      .catchOnly[IOException] {
        val header = font.getHorizontalHeader
        Face(
          font,
          font.getUnicodeCmapLookup,
          font.getUnitsPerEm,
          header.getAscender.toInt,
          header.getDescender.toInt,
          header.getLineGap.toInt
        )
      }
      .leftMap(failure => FontLoadError(resource, Option(failure.getMessage).getOrElse("unreadable metrics")))
