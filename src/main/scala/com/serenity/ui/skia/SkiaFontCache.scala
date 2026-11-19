package com.serenity.ui.skia

import java.lang.invoke.{MethodHandle, MethodHandles, MethodType}
import java.util.concurrent.ConcurrentHashMap

import scala.util.Try

import com.serenity.ui.fonts.LogicalFontFamilies
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{FontSpec, StyledFontCache}
import org.jetbrains.skia.{Data, Font, FontEdging, FontHinting, FontMgr, FontStyleSet, FontVariation, Typeface}

/** What painting with one AWT font needs from Skia: the Skia font, and the strikethrough the AWT font asks for, which
  * Skia draws as a rule rather than a font attribute.
  */
final case class SkiaFace(font: Font, strikethrough: Option[StyledFontCache.Underline])

/** The Skia font that stands in for each font spec layout measures with.
  *
  * Layout still measures through AWT, so Skia has to draw the same glyphs at the same advances: the bundled fonts are
  * loaded from the very files AWT loads them from, and a system family is looked up by name. Metrics are unhinted and
  * linear, the same fractional advances AWT reports, so a run starting at a layout-given x ends where layout says.
  *
  * A face is built once per distinct font spec, under one lock, and warmed before it is handed out: the first use of a
  * typeface (table loading, the glyph cache) is where `FontFirstUse` found AWT disagreeing with itself across threads,
  * and Skia's font manager is not documented as safe for two threads' first lookups.
  */
final class SkiaFontCache(
    maxFaces: Int = SkiaFontCache.DefaultMaxFaces,
    warmup: WarmupStats = new WarmupStats(),
    resolveTypeface: SkiaTypefaces.TypefaceKey => Typeface = new SharedTypefaces().typefaceFor
):
  import SkiaFontCache.*

  private val faces     = new ConcurrentHashMap[FontSpec, SkiaFace]()
  private val ligatures = new ConcurrentHashMap[Font, Boolean]()
  private val typefaces = new ConcurrentHashMap[SkiaTypefaces.TypefaceKey, Typeface]()
  private val lock      = new Object

  def faceFor(font: FontSpec): SkiaFace =
    Option(faces.get(font)).getOrElse {
      lock.synchronized {
        Option(faces.get(font)).getOrElse {
          if faces.size >= maxFaces then
            faces.clear()
            ligatures.clear()
            typefaces.clear()
          val face = build(font)
          val _    = ligatures.put(face.font, font.ligatures)
          val _    = faces.put(font, face)
          face
        }
      }
    }

  def size: Int = faces.size

  /** The grid a font is laid out on: the width of its `M` rounded up, its ascent rounded up and, under the baseline,
    * its descent and leading rounded up, so the line is the two summed and a descender always has the room Skia says it
    * needs. Rounding the sum instead can leave a pixel too few under the baseline.
    */
  def cellMetrics(font: FontSpec): CellMetrics =
    val skia                       = faceFor(font).font
    val (ascent, descent, leading) = SkiaVerticalMetrics.of(skia)
    val aboveBaseline              = math.ceil((-ascent).toDouble).toInt
    val belowBaseline              = math.ceil((descent + leading).toDouble).toInt
    CellMetrics(
      math.max(1, math.ceil(skia.measureTextWidth("M").toDouble).toInt),
      math.max(1, aboveBaseline + belowBaseline),
      math.max(1, aboveBaseline),
      displayWidthAware = true
    )

  /** Whether text is shaped with `font`'s ligatures on: what the spec it was built for says, and on for any other font.
    */
  def ligaturesOf(font: Font): Boolean = Option(ligatures.get(font)).forall(identity)

  private def build(font: FontSpec): SkiaFace =
    val started = System.nanoTime()
    val skia    = SkiaTypefaces.fontFor(font, typefaceOf)
    val _       = skia.measureTextWidth(WarmUpText)
    val face    = SkiaFace(skia, AwtFontReconciliation.strikethroughOf(font))
    warmup.faceBuilt(System.nanoTime() - started)
    face

  /** Fonts that choose a face from the same inputs share one lookup; called only from `build`, under `lock`. */
  private def typefaceOf(key: SkiaTypefaces.TypefaceKey): Typeface =
    Option(typefaces.get(key)).getOrElse {
      val typeface = resolveTypeface(key)
      val _        = typefaces.put(key, typeface)
      typeface
    }

object SkiaFontCache:

  val DefaultMaxFaces: Int = 256

  private val WarmUpText = "Hoxi AVATAR fi To"

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

  private[skia] def bundledTypefaces: List[Typeface] = Bundled.faces

  /** Whether a bundled face carries `family`, whatever the system holds. */
  def hasBundled(family: String): Boolean = Bundled.faces.exists(_.getFamilyName.equalsIgnoreCase(family))

  /** Everything the choice of a face reads from a font: two fonts with the same key get the same typeface, at whatever
    * size each is drawn.
    */
  final case class TypefaceKey(family: String, bold: Boolean, italic: Boolean)

  def keyOf(spec: FontSpec): TypefaceKey = TypefaceKey(spec.family, spec.bold, spec.italic)

  def fontFor(spec: FontSpec): Font = fontFor(spec, typefaceFor)

  def fontFor(spec: FontSpec, typefaceOf: TypefaceKey => Typeface): Font =
    val key      = keyOf(spec)
    val bold     = key.bold
    val italic   = key.italic
    val resolved = typefaceOf(key)
    val font     = new Font(resolved, spec.sizePt.max(1.0f))
    font.setSubpixel(true)
    font.setLinearMetrics(true)
    font.setHinting(FontHinting.NONE)
    font.setEdging(FontEdging.ANTI_ALIAS)
    font.setEmboldened(bold && !resolved.isBold)
    if italic && !resolved.isItalic then font.setSkewX(ObliqueSkew)
    font

  /** Skia's own oblique slant, applied when the typeface has no italic of its own. */
  private val ObliqueSkew = -0.25f

  /** The family AWT draws a logical font in on `osName` where the JDK documents one; layout measures with it, and a
    * system font manager does not know the logical names.
    */
  def physicalFamilyFor(logical: String, osName: String): Option[String] =
    LogicalFontFamilies.documentedFamily(logical, osName)

  def typefaceFor(key: TypefaceKey): Typeface =
    typefaceFor(key.family, key.bold, key.italic)

  private def typefaceFor(family: String, bold: Boolean, italic: Boolean): Typeface =
    systemTypeface(family, bold, italic)
      .orElse(physicalFamilyFor(family, System.getProperty("os.name", "")).flatMap(systemTypeface(_, bold, italic)))
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
    val candidates = (0 until set.count()).iterator
      .flatMap(index => Option(set.getTypeface(index)))
      .map(face => FaceChoice.Candidate(face, SkiaFontWeight.of(face), face.isItalic))
      .toVector
    FaceChoice.choose(candidates, bold, italic)

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

/** A face's weight, which Skiko hands out as part of a Kotlin value class under a name Scala cannot call stably. The
  * weight is the low half of the packed style; without the accessor a face is regular or bold by its flag.
  */
private[skia] object SkiaFontWeight:

  private val StyleOf: Option[MethodHandle] =
    classOf[Typeface].getMethods
      .find(m => m.getName.startsWith("getFontStyle-") && m.getParameterCount == 0 && m.getReturnType == classOf[Int])
      .flatMap(method => Try(MethodHandles.publicLookup().unreflect(method)).toOption)

  def of(face: Typeface): Int =
    StyleOf
      .flatMap { handle =>
        val packed: Object = handle.invoke(face)
        Option(packed).collect { case value: Integer => value.intValue & 0xffff }
      }
      .getOrElse(if face.isBold then FaceChoice.BoldWeight else FaceChoice.RegularWeight)

/** Skia's vertical metrics of a font. Skiko hands its `FontMetrics` value class out as a float array under a name Scala
  * cannot call, in the order top, ascent, descent, bottom, leading.
  */
private[skia] object SkiaVerticalMetrics:

  private val MetricsOf: Option[MethodHandle] =
    Try(
      MethodHandles
        .publicLookup()
        .findVirtual(classOf[Font], "getMetrics-l-OL_C8", MethodType.methodType(classOf[Array[Float]]))
    ).toOption

  def available: Boolean = MetricsOf.isDefined

  /** Ascent (negative above the baseline), descent and leading; without the Skiko accessor, the proportions of a
    * typical face, which the measurer's spec catches before anyone measures with them.
    */
  def of(font: Font): (Float, Float, Float) =
    MetricsOf
      .map { handle =>
        val answer: Object = handle.invoke(font)
        answer
      }
      .collect { case metrics: Array[Float] => (metrics(1), metrics(2), metrics(4)) }
      .getOrElse((-0.9f * font.getSize, 0.25f * font.getSize, 0.0f))
