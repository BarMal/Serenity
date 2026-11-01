package com.serenity.ui.layout

import java.awt.Font

import com.serenity.richtext.{ParagraphAlignment, ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, RichTextState}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Counting a document's rows after the panel grew (#2447 B4): the count of each line is what a cold count at the new
  * width gives, and only the lines that were not one row are wrapped again. A cold count (no cache) is the oracle.
  */
class ResizeRowCountSeedingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val serif     = Font(Font.SERIF, Font.PLAIN, 14)
  private val monospace = Font(Font.MONOSPACED, Font.PLAIN, 14)
  private val bufferId  = BufferId(1)

  private val prose = "lorem ipsum dolor sit amet " * 12

  private val corpus = Vector(
    "",
    "short",
    "a short line of code()",
    prose,
    "x" * 600,
    "\tindented\tcode with\ttabs",
    "日本語のテキスト" * 15,
    "👩‍👩‍👧‍👦 family " * 12,
    "ＡＢＣ" * 40,
    "word " * 3,
    "  leading spaces " + "word " * 40
  )

  private val plainBuffer = Buffer.fromString(bufferId, corpus.mkString("\n"))

  private val richDocument = RichTextDocument(
    List(
      RichTextParagraph.plain("A heading that is long enough to wrap once the panel is narrow", role = ParagraphRole.Heading(1)),
      RichTextParagraph.plain("A shorter heading", role = ParagraphRole.Heading(2)),
      RichTextParagraph.plain(prose, ParagraphAlignment.Left, ParagraphRole.dropCap(3)),
      RichTextParagraph.plain(prose, ParagraphAlignment.Center),
      RichTextParagraph.plain(prose, ParagraphAlignment.Right),
      RichTextParagraph.plain("short centred", ParagraphAlignment.Center),
      RichTextParagraph.plain("Body text that wraps at some widths and not at others, " * 3),
      RichTextParagraph.plain("")
    )
  )

  private val richBuffer = Buffer
    .fromString(bufferId, richDocument.plainText)
    .copy(richText = RichTextState(richTextDocument = Some(richDocument)))

  private val widths = List(120, 157, 200, 263, 333, 480, 640, 900)

  private val growths: List[(Int, Int)] =
    for
      narrow <- widths
      wide   <- (narrow + 1) :: widths.filter(_ > narrow)
    yield narrow -> wide

  private def counts(buffer: Buffer, widthPx: Int, font: Font, cache: WrappedLineCache): Vector[Int] =
    val rows = VisualRowCounts.forBuffer(buffer, widthPx, font, None, false, cache)
    Vector.tabulate(buffer.document.content.lineCount)(rows.rowsIn)

  private def cold(buffer: Buffer, widthPx: Int, font: Font): Vector[Int] =
    counts(buffer, widthPx, font, WrappedLineCache.Uncached)

  private def wraps(cache: WrappedLineCache.Bounded): Long =
    cache.wrapStats.coldWraps + cache.wrapStats.incrementalWraps

  private val documents = List("plain text" -> plainBuffer, "rich text" -> richBuffer)
  private val fonts     = List("proportional" -> serif, "monospaced" -> monospace)

  for
    (documentName, buffer) <- documents
    (fontName, font)       <- fonts
  do
    s"The $documentName counts in a $fontName font after a grow" should "equal a cold count at the new width, line by line" in {
      growths.foreach { (narrow, wide) =>
        val cache = WrappedLineCache.bounded()
        counts(buffer, narrow, font, cache)

        withClue(s"$narrow px to $wide px: ")(counts(buffer, wide, font, cache) shouldBe cold(buffer, wide, font))
      }
    }

    it should "never need more rows for a line that was one row" in {
      growths.foreach { (narrow, wide) =>
        val before = cold(buffer, narrow, font)
        val after  = cold(buffer, wide, font)

        withClue(s"$narrow px to $wide px: ") {
          before.indices.filter(before(_) == 1).foreach(line => withClue(s"line $line: ")(after(line) shouldBe 1))
        }
      }
    }

  "Growing the panel" should "wrap only the lines that were not one row" in {
    val cache = WrappedLineCache.bounded()
    val atNarrow = counts(plainBuffer, 333, serif, cache)
    val before   = wraps(cache)

    counts(plainBuffer, 400, serif, cache)

    wraps(cache) - before shouldBe atNarrow.count(_ != 1).toLong
  }

  it should "wrap no line at all when every line was one row" in {
    val shortLines = Buffer.fromString(bufferId, Vector.fill(40)("a short line").mkString("\n"))
    val cache      = WrappedLineCache.bounded()
    counts(shortLines, 333, serif, cache)
    val before = wraps(cache)

    counts(shortLines, 400, serif, cache)

    wraps(cache) - before shouldBe 0L
  }

  "Shrinking the panel" should "wrap every line again" in {
    val cache = WrappedLineCache.bounded()
    counts(plainBuffer, 400, serif, cache)
    val before = wraps(cache)

    counts(plainBuffer, 333, serif, cache)

    wraps(cache) - before shouldBe corpus.size.toLong
  }

  "Growing the panel and changing the font" should "wrap every line again" in {
    val cache = WrappedLineCache.bounded()
    counts(plainBuffer, 333, serif, cache)
    val before = wraps(cache)

    counts(plainBuffer, 400, Font(Font.SERIF, Font.PLAIN, 15), cache)

    wraps(cache) - before shouldBe corpus.size.toLong
  }
