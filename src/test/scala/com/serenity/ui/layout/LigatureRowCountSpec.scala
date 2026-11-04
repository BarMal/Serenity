package com.serenity.ui.layout

import java.awt.Font
import java.util.concurrent.Executor

import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.ui.fonts.FontLoader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Counting the rows of a source file in a code font set with ligatures, the default (#2447 B2): once the font's
  * certificate is built, a line it puts on one row is counted without wrapping it, and a row the real layout paints
  * differently revokes the certificate and has every line recounted by the wrap. The unguarded wrap is the oracle.
  */
class LigatureRowCountSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val frc              = TextLayoutSnapshot.defaultFontRenderContext()
  private val config           = FontLoader.FontConfig()
  private val bufferId         = BufferId(1)
  private val inline: Executor = _.run()

  private def ligatureFont(sizePt: Float = 12.0f): Font =
    FontLoader.previewCodeFont(config.copy(fontSize = sizePt, enableLigatures = true))

  private def cacheWithCertificate(font: Font): WrappedLineCache.Bounded =
    val cache = WrappedLineCache.bounded(layoutMemos = LayoutMemos(inline))
    val key   = NominalAdvances.Key(font, frc)
    cache.layoutMemos.rowCertificates.of(key)
    cache.layoutMemos.rowCertificates.of(key) shouldBe defined
    cache

  private val source = Vector.tabulate(40) { line =>
    if line % 3 == 0 then s"  val f$line = (a: Int) => a -> $line // line $line"
    else if line % 3 == 1 then s"    case Some(x) if x >= $line && x != 1 => x |> g >>= h"
    else s"  def step$line(value: Int): Int = value * 2 + $line"
  }

  private def wrapped(text: String, widthPx: Int, font: Font): Int =
    TextLayoutSnapshot
      .boundedVisualLinesForText(
        text,
        0,
        widthPx,
        font,
        cellMetricsOverride = Some(CellMetrics.fromFont(font)),
        wrapCache = WrappedLineCache.bounded()
      )
      .length
      .max(1)

  private def counted(
    buffer: Buffer,
    widthPx: Int,
    font: Font,
    cache: WrappedLineCache,
    painted: Map[Int, Int] = Map.empty
  ): Vector[Int] =
    val rows = VisualRowCounts.forBuffer(
      buffer,
      widthPx,
      font,
      Some(CellMetrics.fromFont(font)),
      false,
      cache,
      known = painted.get,
      painted = painted
    )
    Vector.tabulate(buffer.document.content.lineCount)(rows.rowsIn)

  private def wraps(cache: WrappedLineCache.Bounded): Long =
    cache.wrapStats.coldWraps + cache.wrapStats.incrementalWraps

  "A source file in a font set with ligatures" should "count every line as the wrap does, without wrapping the lines that fit" in
    List(11.0f, 12.0f, 14.0f, 16.0f).foreach { size =>
      val font   = ligatureFont(size)
      val buffer = Buffer.fromString(bufferId, source.mkString("\n"))
      val cache  = cacheWithCertificate(font)

      withClue(s"$size pt: ") {
        counted(buffer, 1472, font, cache) shouldBe source.map(wrapped(_, 1472, font))
        wraps(cache) shouldBe 0L
      }
    }

  it should "count lines either side of the certificate's decision as the wrap does" in {
    val font   = ligatureFont()
    val text   = source(0)
    val total  = font.createGlyphVector(frc, text).getGlyphPosition(text.length).getX.toFloat
    val buffer = Buffer.fromString(bufferId, text)
    List(-2, -1, 0, 1, 2).foreach { offset =>
      val width = math.max(1, math.round(total) + offset)
      withClue(s"width $width: ")(
        counted(buffer, width, font, cacheWithCertificate(font)) shouldBe Vector(wrapped(text, width, font))
      )
    }
  }

  it should "wrap every line while the certificate is not yet built" in {
    val font   = ligatureFont()
    val buffer = Buffer.fromString(bufferId, source.mkString("\n"))
    val cache  = WrappedLineCache.bounded(layoutMemos = LayoutMemos((_: Runnable) => ()))

    counted(buffer, 1472, font, cache) shouldBe source.map(wrapped(_, 1472, font))

    wraps(cache) shouldBe source.size.toLong
  }

  it should "wrap a line that is not printable ASCII, and only those" in {
    val font   = ligatureFont()
    val lines  = Vector("val a = 1", "\tval b = 2", "val c = \"é\"", "val d = a -> b")
    val buffer = Buffer.fromString(bufferId, lines.mkString("\n"))
    val cache  = cacheWithCertificate(font)

    counted(buffer, 1472, font, cache)

    wraps(cache) shouldBe 2L
  }

  "A painted row count that differs from the certificate's" should "revoke the certificate and recount every other line by the wrap" in {
    val font   = ligatureFont()
    val buffer = Buffer.fromString(bufferId, source.mkString("\n"))
    val cache  = cacheWithCertificate(font)
    val key    = NominalAdvances.Key(font, frc)

    val rows = counted(buffer, 1472, font, cache, painted = Map(3 -> 2))

    cache.layoutMemos.rowCertificates.of(key) shouldBe None
    rows(3) shouldBe 2
    rows.patch(3, Nil, 1) shouldBe source.patch(3, Nil, 1).map(wrapped(_, 1472, font))
    wraps(cache) shouldBe (source.size - 1).toLong
  }

  it should "leave the certificate alone when every painted line agrees with it" in {
    val font   = ligatureFont()
    val buffer = Buffer.fromString(bufferId, source.mkString("\n"))
    val cache  = cacheWithCertificate(font)

    counted(buffer, 1472, font, cache, painted = Map(3 -> 1, 4 -> 1))

    cache.layoutMemos.rowCertificates.of(NominalAdvances.Key(font, frc)) shouldBe defined
  }

  it should "not count a line the certificate did not call one row as a miscount" in {
    val font   = ligatureFont()
    val buffer = Buffer.fromString(bufferId, "x" * 600)
    val cache  = cacheWithCertificate(font)

    counted(buffer, 1472, font, cache, painted = Map(0 -> 3))

    cache.layoutMemos.rowCertificates.of(NominalAdvances.Key(font, frc)) shouldBe defined
  }

  it should "also revoke the certificate of a font set without ligatures" in {
    val font   = FontLoader.previewCodeFont(config.copy(enableLigatures = false))
    val buffer = Buffer.fromString(bufferId, source.mkString("\n"))
    val cache  = WrappedLineCache.bounded(layoutMemos = LayoutMemos(inline))

    counted(buffer, 1472, font, cache, painted = Map(3 -> 2))

    cache.layoutMemos.rowCertificates.of(NominalAdvances.Key(font, frc)) shouldBe None
  }

  it should "drop the row counts already kept, so lines counted by the old certificate are counted again" in {
    val font   = ligatureFont()
    val buffer = Buffer.fromString(bufferId, source.mkString("\n"))
    val cache  = cacheWithCertificate(font)
    counted(buffer, 1472, font, cache)

    counted(buffer, 1472, font, cache, painted = Map(3 -> 2))

    wraps(cache) shouldBe (source.size - 1).toLong
  }
