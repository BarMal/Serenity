package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Callable, Executors}

import com.serenity.ui.fonts.FontLoader
import org.jetbrains.skia.{EncodedImageFormat, Font, Paint, Surface, Typeface}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Text with characters the base font lacks is shaped by supplying our own font runs instead of Skiko's `TextLine`,
  * which costs 4 to 8 ms a call whatever the text. Every claim here is a comparison against `TextLine` itself, for
  * every code point of the allowlist, so the two cannot drift apart unnoticed.
  */
final class FallbackRunsSpec extends AnyFlatSpec with Matchers:

  /** The ranges the owner approved, before the controls are taken out. */
  private val ApprovedRanges = Vector(0x2500 to 0x257f, 0x2190 to 0x21ff, 0x2000 to 0x206f)

  /** Bidi and joiner controls: they reorder or join, which explicit runs do not model. */
  private val Controls: Set[Int] = Set(0x200c, 0x200d, 0x200e, 0x200f) ++ (0x202a to 0x202e) ++ (0x2066 to 0x2069)

  private val Contexts: Vector[(String, String => String)] = Vector(
    "alone"             -> (c => c),
    "spaces both sides" -> (c => s"  $c  "),
    "letters around"    -> (c => s"ab$c cd"),
    "two in a row"      -> (c => s"$c$c x")
  )

  private val Witnesses = Vector(0x2502, 0x2192, 0x2014)

  private def text(cp: Int, wrap: String => String): String = wrap(new String(Character.toChars(cp)))

  private def baseFonts: Vector[(String, Font)] =
    val config = FontLoader.FontConfig()
    val awt = Vector(
      "text"       -> FontLoader.previewTextFont(config),
      "code"       -> FontLoader.previewCodeFont(config),
      "ui"         -> FontLoader.previewUiFont(config),
      "Serif"      -> new java.awt.Font(java.awt.Font.SERIF, java.awt.Font.PLAIN, 12),
      "SansSerif"  -> new java.awt.Font(java.awt.Font.SANS_SERIF, java.awt.Font.PLAIN, 12),
      "Monospaced" -> new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
    ).map((name, font) => s"$name (${font.getFamily})" -> SkiaTypefaces.fontFor(font))
    val bundled = SkiaTypefaces.bundledTypefaces.map(t => s"bundled ${t.getFamilyName}" -> new Font(t, 12f))
    (awt ++ bundled).distinctBy((_, font) =>
      (font.getTypeface.getFamilyName, font.getTypeface.isBold, font.getTypeface.isItalic, font.getSize)
    )

  private def pixels(stretch: SkiaShapedStretch, from: Int, until: Int): Vector[Byte] =
    val surface = Surface.Companion.makeRasterN32Premul(260, 44)
    val canvas  = surface.getCanvas
    val _       = canvas.clear(0xffffffff)
    val paint   = new Paint()
    paint.setColor(0xff000000)
    stretch.draw(canvas, from, until, 12f, 30f, paint)
    surface.makeImageSnapshot().encodeToData(EncodedImageFormat.PNG, 100, 0).getBytes.toVector

  private def ranges(length: Int): Vector[(Int, Int)] =
    (Vector((0, length)) ++ (0 until length).map(i => (i, i + 1)) ++ Vector((1, length - 1)))
      .filter((a, b) => a < b)
      .distinct

  private def differences(routed: SkiaShapedStretch, reference: SkiaShapedStretch, length: Int): Vector[String] =
    val edges =
      (-1 to length + 1).filter(i => routed.leftEdgeOf(i) != reference.leftEdgeOf(i)).map(i => s"leftEdgeOf($i)")
    val draws =
      ranges(length).filter((a, b) => pixels(routed, a, b) != pixels(reference, a, b)).map((a, b) => s"pixels[$a,$b)")
    (edges ++ draws).toVector

  /** Four workers: each font's comparisons are independent, and the reference side, `TextLine`, is the slow part. */
  private val Workers = 4

  private def differencesOn(fontName: String, font: Font): Vector[String] =
    for
      cp              <- ApprovedRanges.flatten.filter(FallbackRuns.isAllowlisted).toVector
      (context, wrap) <- Contexts
      string = text(cp, wrap)
      found <-
        val routed    = new SkiaShapedTextCache().stretchOf(string, font)
        val reference = new SkiaShapedTextCache().shapeLine(string, font)
        differences(routed, reference, string.length).map(what => f"$fontName / U+$cp%04X / $context: $what")
    yield found

  "The allowlist" should "be the approved ranges without the bidi and joiner controls and without undefined code points" in {
    val approved = ApprovedRanges.flatten.filterNot(Controls).filter(Character.isDefined)
    val listed   = ApprovedRanges.flatten.filter(FallbackRuns.isAllowlisted)

    listed shouldBe approved
    Controls.exists(FallbackRuns.isAllowlisted) shouldBe false
    FallbackRuns.isAllowlisted(0x2065) shouldBe false
    FallbackRuns.isAllowlisted('a'.toInt) shouldBe false
    FallbackRuns.isAllowlisted(0x2fff) shouldBe false
  }

  "Text shaped with our own runs" should "equal TextLine for every allowlisted code point, in four contexts, on every base font" in {
    val pool = Executors.newFixedThreadPool(Workers)
    val failures =
      try
        val perFont = baseFonts.map { (fontName, font) =>
          pool.submit(
            new Callable[Vector[String]]:
              def call(): Vector[String] = differencesOn(fontName, font)
          )
        }
        perFont.flatMap(_.get())
      finally pool.shutdown()

    withClue(s"${failures.size} differences, first: ${failures.take(15).mkString("; ")}\n") {
      failures shouldBe empty
    }
  }

  it should "not use TextLine for the cold frame's box bar, or any witness, on any base font" in {
    val used = for
      (fontName, font) <- baseFonts
      cp               <- Witnesses
      (context, wrap)  <- Contexts
      cache = new SkiaShapedTextCache()
      _     = cache.stretchOf(text(cp, wrap), font)
      if cache.lineShapings != 0L
    yield f"$fontName / U+$cp%04X / $context"

    withClue(s"TextLine still used for: ${used.take(10).mkString("; ")}\n") {
      used shouldBe empty
    }
  }

  it should "keep text outside the allowlist on TextLine" in {
    val outside = Vector(
      "combining mark"     -> "é x",
      "Arabic"             -> "ال x",
      "joiner in a row"    -> "a‍b",
      "bidi isolate"       -> "a⁦b⁩",
      "CJK"                -> "中文 x",
      "emoji with variant" -> "❤️ x"
    )
    val (_, font) = baseFonts.head
    outside.foreach { (label, string) =>
      withClue(label) {
        val cache = new SkiaShapedTextCache()
        cache.stretchOf(string, font)
        cache.lineShapings shouldBe 1L
      }
    }
  }

  it should "fall back to TextLine, with the same result, when the character-match call is unavailable" in {
    val (_, font) = baseFonts.head
    val string    = "  │  "
    val cache     = new SkiaShapedTextCache(fallbacks = FallbackFonts.unavailable)

    val routed = cache.stretchOf(string, font)

    cache.lineShapings shouldBe 1L
    differences(routed, new SkiaShapedTextCache().shapeLine(string, font), string.length) shouldBe empty
  }

  it should "fall back to TextLine when no installed font has the character" in {
    val (_, font) = baseFonts.head
    val string    = "  │  "
    val cache     = new SkiaShapedTextCache(fallbacks = FallbackFonts.matching((_, _) => None))

    cache.stretchOf(string, font)

    cache.lineShapings shouldBe 1L
  }

  it should "ask the font manager once for a character and base font, however often the character is shaped" in {
    val (_, font) = baseFonts.head
    val asked     = new AtomicInteger(0)
    val cache = new SkiaShapedTextCache(fallbacks = FallbackFonts.matching { (base, cp) =>
      val _ = asked.incrementAndGet()
      FallbackFonts.system.typefaceFor(base, cp)
    })

    List("│", "a│b", "  │  ", "││").foreach(string => cache.stretchOf(string, font))

    asked.get shouldBe 1
    cache.lineShapings shouldBe 0L
  }

  it should "give a typeface for a character the base font lacks, from the system's font manager" in {
    val (_, font)               = baseFonts.head
    val found: Option[Typeface] = FallbackFonts.system.typefaceFor(font.getTypeface, 0x2502)

    found.map(_.getUTF32Glyph(0x2502)).exists(_ != 0) shouldBe true
  }
