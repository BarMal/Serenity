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

  /** Bidi and joiner controls, and the line and paragraph separators, which differed from `TextLine` on macOS. */
  private val Controls: Set[Int] =
    Set(0x200c, 0x200d, 0x200e, 0x200f, 0x2028, 0x2029) ++ (0x202a to 0x202e) ++ (0x2066 to 0x2069)

  private val Contexts: Vector[(String, String => String)] = Vector(
    "alone"             -> (c => c),
    "spaces both sides" -> (c => s"  $c  "),
    "letters around"    -> (c => s"ab$c cd"),
    "two in a row"      -> (c => s"$c$c x")
  )

  private val Witnesses = Vector(0x2502, 0x2192, 0x2014)

  /** A base font without a box-drawing glyph on every machine: the bundled Monaspace Neon, whose file is the same
    * everywhere, unlike the system fonts, one of which may well have the glyph.
    */
  private def lacking: Font =
    SkiaTypefaces.bundledTypefaces
      .find(t => t.getFamilyName == "Monaspace Neon" && !t.isBold)
      .map(new Font(_, 12f))
      .getOrElse(fail("the bundled Monaspace Neon is missing"))

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

  /** The differences grouped by font and context, with the code points, so a CI log shows the pattern at a glance. */
  private def summary(failures: Vector[String]): String =
    val grouped = failures
      .flatMap { line =>
        val parts = line.split(" / ", 3)
        if parts.length == 3 then Some((parts(0), parts(1), parts(2).takeWhile(_ != ':'))) else None
      }
      .groupBy((font, _, context) => (font, context))
    grouped.toVector
      .sortBy(_._1)
      .map {
        case ((font, context), items) =>
          val codePoints = items.map(_._2).distinct.sorted
          s"FALLBACK-RUNS DIFFERENCES [$font / $context]: ${codePoints.size} code points: ${codePoints.take(40).mkString(" ")}"
      }
      .mkString("\n")

  "The allowlist" should "be the approved ranges without the bidi and joiner controls, the separators and undefined code points" in {
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

    if failures.nonEmpty then println(summary(failures))
    withClue(s"${failures.size} differences, first: ${failures.take(15).mkString("; ")}\n") {
      failures shouldBe empty
    }
  }

  it should "not use TextLine for the cold frame's box bar, or any witness, wherever a font has the characters" in {
    val used = for
      (fontName, font) <- baseFonts :+ ("bundled Monaspace Neon" -> lacking)
      cp               <- Witnesses
      (context, wrap)  <- Contexts
      string = text(cp, wrap)
      if FallbackRuns.runsFor(string, font, FallbackFonts.system).isDefined
      cache = new SkiaShapedTextCache()
      _     = cache.stretchOf(string, font)
      if cache.lineShapings != 0L
    yield f"$fontName / U+$cp%04X / $context"

    withClue(s"TextLine still used for: ${used.take(10).mkString("; ")}\n") {
      used shouldBe empty
    }
  }

  it should "not use TextLine for the box bar on a base font that lacks it, when any installed font has it" in {
    val font = lacking
    font.getTypeface.getUTF32Glyph(0x2502) shouldBe 0
    val installed = FallbackFonts.system.typefaceFor(font.getTypeface, 0x2502).exists(_.getUTF32Glyph(0x2502) != 0)
    assume(installed, "no installed font has U+2502 on this machine")

    val cache = new SkiaShapedTextCache()
    cache.stretchOf("  \u2502  ", font)

    cache.lineShapings shouldBe 0L
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
    val font = lacking
    outside.foreach { (label, string) =>
      withClue(label) {
        val cache = new SkiaShapedTextCache()
        cache.stretchOf(string, font)
        cache.lineShapings shouldBe 1L
      }
    }
  }

  it should "fall back to TextLine, with the same result, when the character-match call is unavailable" in {
    val font   = lacking
    val string = "  │  "
    val cache  = new SkiaShapedTextCache(fallbacks = FallbackFonts.unavailable)

    val routed = cache.stretchOf(string, font)

    cache.lineShapings shouldBe 1L
    differences(routed, new SkiaShapedTextCache().shapeLine(string, font), string.length) shouldBe empty
  }

  it should "fall back to TextLine when no installed font has the character" in {
    val font   = lacking
    val string = "  │  "
    val cache  = new SkiaShapedTextCache(fallbacks = FallbackFonts.matching((_, _) => None))

    cache.stretchOf(string, font)

    cache.lineShapings shouldBe 1L
  }

  it should "ask the font manager once for a character and base font, however often the character is shaped" in {
    val font  = lacking
    val asked = new AtomicInteger(0)
    val cache = new SkiaShapedTextCache(fallbacks = FallbackFonts.matching { (base, cp) =>
      val _ = asked.incrementAndGet()
      FallbackFonts.system.typefaceFor(base, cp)
    })

    List("│", "a│b", "  │  ", "││").foreach(string => cache.stretchOf(string, font))

    asked.get shouldBe 1
    cache.lineShapings shouldBe 0L
  }

  it should "give a typeface for a character the base font lacks, from the system's font manager" in {
    val font                    = lacking
    val found: Option[Typeface] = FallbackFonts.system.typefaceFor(font.getTypeface, 0x2502)

    found.map(_.getUTF32Glyph(0x2502)).exists(_ != 0) shouldBe true
  }
