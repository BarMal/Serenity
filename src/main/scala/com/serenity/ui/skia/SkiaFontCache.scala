package com.serenity.ui.skia

import java.awt.font.{FontRenderContext, TextAttribute}
import java.awt.geom.AffineTransform
import java.util.concurrent.ConcurrentHashMap

import com.serenity.ui.renderer.{FontFirstUse, StyledFontCache}
import org.jetbrains.skia.{Data, Font, FontEdging, FontHinting, FontMgr, FontStyleSet, FontVariation, Typeface}

/** What painting with one AWT font needs from Skia: the Skia font, and the strikethrough the AWT font asks for, which
  * Skia draws as a rule rather than a font attribute.
  */
final case class SkiaFace(font: Font, strikethrough: Option[StyledFontCache.Underline])

/** The Skia font that stands in for each AWT font layout measures with.
  *
  * Layout measures with `java.awt.Font`, so Skia has to draw the same glyphs at the same advances: the bundled fonts
  * are loaded from the very files AWT loads them from, and a system family is looked up by name. Metrics are unhinted
  * and linear, the same fractional advances AWT reports, so a run starting at a layout-given x ends where layout says.
  *
  * A face is built once per distinct AWT font, under one lock, and warmed before it is handed out: the first use of a
  * typeface (table loading, the glyph cache) is where `FontFirstUse` found AWT disagreeing with itself across threads,
  * and Skia's font manager is not documented as safe for two threads' first lookups.
  */
final class SkiaFontCache(maxFaces: Int = SkiaFontCache.DefaultMaxFaces):
  import SkiaFontCache.*

  private val faces = new ConcurrentHashMap[java.awt.Font, SkiaFace]()
  private val lock  = new Object

  def faceFor(font: java.awt.Font): SkiaFace =
    Option(faces.get(font)).getOrElse {
      lock.synchronized {
        Option(faces.get(font)).getOrElse {
          if faces.size >= maxFaces then faces.clear()
          val face = build(font)
          val _    = faces.put(font, face)
          face
        }
      }
    }

  def size: Int = faces.size

  private def build(font: java.awt.Font): SkiaFace =
    FontFirstUse.settle(font, StrikeContext)
    val skia = SkiaTypefaces.fontFor(font)
    val _    = skia.measureTextWidth(WarmUpText)
    SkiaFace(skia, strikethroughOf(font))

  private def strikethroughOf(font: java.awt.Font): Option[StyledFontCache.Underline] =
    Option.when(font.getAttributes.get(TextAttribute.STRIKETHROUGH) == TextAttribute.STRIKETHROUGH_ON) {
      val metrics = font.getLineMetrics("", StrikeContext)
      StyledFontCache.Underline(metrics.getStrikethroughOffset, metrics.getStrikethroughThickness)
    }

object SkiaFontCache:

  val DefaultMaxFaces: Int = 256

  private val WarmUpText = "Hoxi AVATAR fi To"

  /** The strikethrough rule belongs to the font, not to the surface that paints it. */
  private val StrikeContext = FontRenderContext(AffineTransform(), true, true)

/** Finds the Skia typeface for an AWT font's family and style, and sets a Skia font up the way layout measures. */
private[skia] object SkiaTypefaces:

  private val Bold = 700

  /** AWT's logical families, which fontconfig and CoreText do not know, by the generic family each resolves to. */
  private val LogicalFamilies = Map(
    "monospaced"  -> "monospace",
    "dialoginput" -> "monospace",
    "serif"       -> "serif",
    "sansserif"   -> "sans-serif",
    "dialog"      -> "sans-serif"
  )

  private val BundledResources = List(
    "/fonts/MonaspaceNeonVarVF[wght,wdth,slnt].ttf",
    "/fonts/MonaspaceNeon-Regular.otf",
    "/fonts/MonaspaceNeon-Bold.otf",
    "/fonts/MaterialIconsRound-Regular.otf",
    "/fonts/CourierPrime-Regular.ttf",
    "/fonts/CourierPrime-Bold.ttf",
    "/fonts/CourierPrime-Italic.ttf",
    "/fonts/CourierPrime-BoldItalic.ttf",
    "/fonts/NotoEmoji-Regular.ttf",
    "/fonts/NotoEmoji-Bold.ttf"
  )

  /** Whether a bundled face carries `family`, whatever the system holds. */
  def hasBundled(family: String): Boolean = Bundled.faces.exists(_.getFamilyName.equalsIgnoreCase(family))

  def fontFor(awt: java.awt.Font): Font =
    val bold     = awt.isBold
    val italic   = awt.isItalic
    val resolved = typefaceFor(awt.getFamily, bold, italic)
    val font     = new Font(resolved, awt.getSize2D.max(1.0f))
    font.setSubpixel(true)
    font.setLinearMetrics(true)
    font.setHinting(FontHinting.NONE)
    font.setEdging(FontEdging.ANTI_ALIAS)
    font.setEmboldened(bold && !resolved.isBold)
    if italic && !resolved.isItalic then font.setSkewX(ObliqueSkew)
    font

  /** Skia's own oblique slant, applied when the typeface has no italic of its own. */
  private val ObliqueSkew = -0.25f

  private def typefaceFor(family: String, bold: Boolean, italic: Boolean): Typeface =
    systemTypeface(family, bold, italic)
      .orElse(bundledTypeface(family, bold, italic))
      .orElse(systemTypeface(LogicalFamilies.getOrElse(family.toLowerCase, family), bold, italic))
      .orElse(systemTypeface("sans-serif", bold, italic))
      .orElse(anySystemTypeface(bold, italic))
      .getOrElse(Typeface.Companion.makeEmpty())

  /** The installed face of `family` closest to the requested style. `FontStyle` is a Kotlin value class as of Skiko
    * 0.153, so its factory methods carry compiler-mangled names that Scala cannot call stably; the style set's faces
    * report `isBold` and `isItalic` plainly.
    */
  private def systemTypeface(family: String, bold: Boolean, italic: Boolean): Option[Typeface] =
    Option(FontMgr.Companion.getDefault().matchFamily(family)).flatMap(closest(_, bold, italic))

  private def anySystemTypeface(bold: Boolean, italic: Boolean): Option[Typeface] =
    val manager = FontMgr.Companion.getDefault()
    (0 until manager.getFamiliesCount).iterator
      .flatMap(index => Option(manager.makeStyleSet(index)).flatMap(closest(_, bold, italic)))
      .nextOption()

  private def closest(set: FontStyleSet, bold: Boolean, italic: Boolean): Option[Typeface] =
    val faces = (0 until set.count()).flatMap(index => Option(set.getTypeface(index)))
    faces.sortBy(face => (if face.isBold == bold then 0 else 1) + (if face.isItalic == italic then 0 else 1)).headOption

  private def bundledTypeface(family: String, bold: Boolean, italic: Boolean): Option[Typeface] =
    val sameFamily = Bundled.faces.filter(_.getFamilyName.equalsIgnoreCase(family))
    val exact      = sameFamily.find(face => face.isBold == bold && face.isItalic == italic)
    exact.orElse(sameFamily.headOption).map(withWeight(_, bold))

  /** A variable font is asked for its bold instance rather than emboldened by smearing. */
  private def withWeight(face: Typeface, bold: Boolean): Typeface =
    val hasWeightAxis = Option(face.getVariationAxes).exists(_.exists(_.getTag == WeightTag))
    if bold && !face.isBold && hasWeightAxis then face.makeClone(FontVariation("wght", Bold.toFloat))
    else face

  private val WeightTag = FontVariation("wght", 0f).getTag

  /** Read once, on the first family the system does not know. */
  private object Bundled:

    lazy val faces: List[Typeface] = BundledResources.flatMap(load)

    private def load(resource: String): Option[Typeface] =
      Option(getClass.getResourceAsStream(resource)).flatMap { stream =>
        try
          val bytes = stream.readAllBytes()
          Option(FontMgr.Companion.getDefault().makeFromData(Data.Companion.makeFromBytes(bytes, 0, bytes.length), 0))
        catch case _: Exception => None
        finally stream.close()
      }
