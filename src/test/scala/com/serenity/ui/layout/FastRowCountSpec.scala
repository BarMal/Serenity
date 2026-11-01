package com.serenity.ui.layout

import java.awt.Font

import scala.util.Random

import com.serenity.richtext.{RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, RichTextState}
import com.serenity.ui.fonts.FontLoader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Counting the rows of a source file in a code font whose advances are fractional (#2447 B2): a line the font's own
  * advances put on one row is counted without wrapping it, every count equals the wrap's, and anything the font may
  * shape differently, or that is not printable ASCII, is wrapped as before. The unguarded wrap is the oracle.
  */
class FastRowCountSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val frc      = TextLayoutSnapshot.defaultFontRenderContext()
  private val config   = FontLoader.FontConfig()
  private val bufferId = BufferId(1)

  private def codeFont(sizePt: Float, ligatures: Boolean): Font =
    FontLoader.previewCodeFont(config.copy(fontSize = sizePt, enableLigatures = ligatures))

  private val sizes = List(11.0f, 12.0f, 13.0f, 14.0f, 15.0f, 16.0f, 18.0f)

  private val symbols = "=<>-!/*+&|:.~#_^%?@\\;,()[]{}\"'`"
  private val plain   = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Seq(' ', ' ', ' ')).toVector

  private val asciiLines: Vector[String] =
    val random = Random(7)
    Vector("", "x", "val x = 1", "  def step(value: Int): Int = value * 2 + 1") ++
      Vector.fill(40) {
        val length = List(5, 20, 40, 80, 120, 160, 200, 260)(random.nextInt(8))
        String(
          Array.fill(length)(
            if random.nextInt(10) < 4 then symbols(random.nextInt(symbols.length))
            else plain(random.nextInt(plain.size))
          )
        )
      }

  private def metricsOf(font: Font): CellMetrics = CellMetrics.fromFont(font)

  /** What the wrap itself says: the count `VisualRowCounts.forBuffer` takes for a line it does not shortcut. */
  private def wrapped(text: String, widthPx: Int, font: Font): Int =
    TextLayoutSnapshot
      .boundedVisualLinesForText(
        text,
        0,
        widthPx,
        font,
        cellMetricsOverride = Some(metricsOf(font)),
        wrapCache = WrappedLineCache.bounded()
      )
      .length
      .max(1)

  private def counted(buffer: Buffer, widthPx: Int, font: Font, cache: WrappedLineCache, forceCells: Boolean = false) =
    val rows = VisualRowCounts.forBuffer(buffer, widthPx, font, Some(metricsOf(font)), forceCells, cache)
    Vector.tabulate(buffer.document.content.lineCount)(rows.rowsIn)

  /** Widths either side of a line's own width, so the margin the wrap keeps from the edge is crossed both ways. */
  private def widthsAround(text: String, font: Font): List[Int] =
    val total = font.createGlyphVector(frc, text).getGlyphPosition(text.length).getX.toFloat
    List(math.floor(total).toInt - 1, math.floor(total).toInt, math.ceil(total).toInt, math.ceil(total).toInt + 1)
      .map(math.max(1, _))
      .distinct

  private def wraps(cache: WrappedLineCache.Bounded): Long =
    cache.wrapStats.coldWraps + cache.wrapStats.incrementalWraps

  "A code font without ligatures" should "count every line as the wrap does, at widths either side of the line's own" in
    sizes.foreach { size =>
      val font = codeFont(size, ligatures = false)
      asciiLines.foreach { text =>
        val buffer = Buffer.fromString(bufferId, text)
        widthsAround(text, font).foreach { width =>
          withClue(s"$size pt, width $width, '$text': ") {
            counted(buffer, width, font, WrappedLineCache.bounded()) shouldBe Vector(wrapped(text, width, font))
          }
        }
      }
    }

  it should "count lines that are not printable ASCII as the wrap does" in {
    val lines = Vector(
      "\tindented\tcode",
      "naïve café",
      "日本語のテキスト" * 12,
      "👩‍👩‍👧‍👦 family " * 8,
      "á combining " * 10,
      "x" * 600
    )
    sizes.foreach { size =>
      val font = codeFont(size, ligatures = false)
      lines.foreach { text =>
        List(200, 640, 1472).foreach { width =>
          withClue(s"$size pt, width $width, '${text.take(20)}': ") {
            counted(Buffer.fromString(bufferId, text), width, font, WrappedLineCache.bounded()) shouldBe
              Vector(wrapped(text, width, font))
          }
        }
      }
    }
  }

  it should "wrap none of the lines a panel holds on one row" in {
    val font   = codeFont(12.0f, ligatures = false)
    val buffer = Buffer.fromString(bufferId, Vector.tabulate(40)(line => s"  val value$line = $line").mkString("\n"))
    val cache  = WrappedLineCache.bounded()

    counted(buffer, 1472, font, cache) shouldBe Vector.fill(40)(1)

    wraps(cache) shouldBe 0L
  }

  it should "wrap a line wider than the panel, and only that line" in {
    val font   = codeFont(12.0f, ligatures = false)
    val buffer = Buffer.fromString(bufferId, Vector("val short = 1", "x" * 600, "val also = 2").mkString("\n"))
    val cache  = WrappedLineCache.bounded()

    val rows = counted(buffer, 1472, font, cache)

    rows(0) shouldBe 1
    rows(1) should be > 1
    rows(2) shouldBe 1
    wraps(cache) shouldBe 1L
  }

  it should "wrap the lines that are not printable ASCII, and only those" in {
    val font = codeFont(12.0f, ligatures = false)
    val buffer =
      Buffer.fromString(bufferId, Vector("val a = 1", "\tval b = 2", "val c = \"é\"", "val d = 4").mkString("\n"))
    val cache = WrappedLineCache.bounded()

    counted(buffer, 1472, font, cache)

    wraps(cache) shouldBe 2L
  }

  it should "leave a count already known to the caller as it was told" in {
    val font   = codeFont(12.0f, ligatures = false)
    val buffer = Buffer.fromString(bufferId, "x" * 600)
    val rows = VisualRowCounts.forBuffer(
      buffer,
      1472,
      font,
      Some(metricsOf(font)),
      false,
      WrappedLineCache.bounded(),
      known = _ => Some(7)
    )

    rows.rowsIn(0) shouldBe 7
  }

  "A code font with ligatures" should "wrap every line, since shaping may change what a character advances" in {
    val font   = codeFont(12.0f, ligatures = true)
    val buffer = Buffer.fromString(bufferId, Vector.tabulate(40)(line => s"  val value$line = $line").mkString("\n"))
    val cache  = WrappedLineCache.bounded()

    counted(buffer, 1472, font, cache) shouldBe Vector.fill(40)(1)

    wraps(cache) shouldBe 40L
  }

  "The proportional text font" should "wrap every line" in {
    val font   = FontLoader.previewTextFont(config)
    val buffer = Buffer.fromString(bufferId, Vector.tabulate(40)(line => s"short line $line").mkString("\n"))
    val cache  = WrappedLineCache.bounded()

    counted(buffer, 1472, font, cache)

    wraps(cache) shouldBe 40L
  }

  "A buffer with a rich-text document" should "wrap every line" in {
    val font     = codeFont(12.0f, ligatures = false)
    val document = RichTextDocument(List.tabulate(40)(line => RichTextParagraph.plain(s"short line $line")))
    val buffer = Buffer
      .fromString(bufferId, document.plainText)
      .copy(richText = RichTextState(richTextDocument = Some(document)))
    val cache = WrappedLineCache.bounded()

    counted(buffer, 1472, font, cache)

    wraps(cache) shouldBe 40L
  }

  "A cell layout forced by the caller" should "wrap every line, since its rows follow the cell grid and not the advances" in {
    val font   = codeFont(12.0f, ligatures = false)
    val buffer = Buffer.fromString(bufferId, Vector.tabulate(40)(line => s"  val value$line = $line").mkString("\n"))
    val cache  = WrappedLineCache.bounded()

    counted(buffer, 1472, font, cache, forceCells = true)

    wraps(cache) shouldBe 40L
  }
