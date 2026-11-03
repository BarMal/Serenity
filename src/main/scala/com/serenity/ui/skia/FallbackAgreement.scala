package com.serenity.ui.skia

import org.jetbrains.skia.{EncodedImageFormat, Font, Paint, Surface}

/** How many strings of the allowlisted symbols the fallback runs shape exactly as Skiko's `TextLine` does, on the fonts
  * of one machine: the benchmark reports it, so a run on a machine whose fonts the specs never saw shows whether the
  * two agree there. `differing` names the strings that do not, as "font name U+XXXX".
  */
final case class FallbackAgreement(checked: Int, identical: Int, differing: Vector[String])

object FallbackAgreement:

  /** One string for each allowlisted code point and font: the symbol with two spaces each side. A string `TextLine`
    * still shapes (no match call, no font has the character) is its own result, so it counts as identical.
    */
  def check(fonts: Vector[(String, Font)], fallbacks: => FallbackFonts = FallbackFonts.system): FallbackAgreement =
    val distinct = fonts.distinctBy((_, font) => (font.getTypeface.getUniqueId, font.getSize))
    val results = distinct.flatMap { (name, font) =>
      val routed    = new SkiaShapedTextCache(fallbacks = fallbacks)
      val reference = new SkiaShapedTextCache()
      FallbackRuns.codePoints.map { codePoint =>
        val text = s"  ${new String(Character.toChars(codePoint))}  "
        (f"$name U+$codePoint%04X", agree(routed.stretchOf(text, font), reference.shapeLine(text, font), text.length))
      }
    }
    FallbackAgreement(results.size, results.count(_._2), results.filterNot(_._2).map(_._1))

  /** The same check for AWT fonts, which is what the benchmark has. */
  def checkAwt(fonts: Vector[(String, java.awt.Font)]): FallbackAgreement =
    check(fonts.map((name, font) => name -> SkiaTypefaces.fontFor(font)))

  private def agree(routed: SkiaShapedStretch, reference: SkiaShapedStretch, length: Int): Boolean =
    (-1 to length + 1).forall(i => routed.leftEdgeOf(i) == reference.leftEdgeOf(i)) &&
      pixels(routed, length) == pixels(reference, length)

  private def pixels(stretch: SkiaShapedStretch, length: Int): Vector[Byte] =
    val surface = Surface.Companion.makeRasterN32Premul(200, 44)
    val canvas  = surface.getCanvas
    val _       = canvas.clear(0xffffffff)
    val paint   = new Paint()
    paint.setColor(0xff000000)
    stretch.draw(canvas, 0, length, 12f, 30f, paint)
    surface.makeImageSnapshot().encodeToData(EncodedImageFormat.PNG, 100, 0).getBytes.toVector
