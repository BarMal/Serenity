package com.serenity.perf

import java.awt.Font

import com.serenity.richtext.{ParagraphAlignment, ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, RichTextState, Viewport}
import com.serenity.ui.layout.{LayoutMemos, TextLayoutSnapshot, WrappedLineCache}
import com.serenity.ui.renderer.ShapedTextCache

/** Snapshot builds over a warm wrap cache: the per-frame and per-navigation cost of laying out prose whose wraps are
  * already cached (#1908). Aligned paragraphs pay for placing caret stops, and a line inserted above moves every
  * paragraph to a new buffer line.
  */
private[perf] object LayoutSnapshotBenchmarks:

  given Balance = Balance.default

  private val proseFont  = Font(Font.SERIF, Font.PLAIN, 14)
  private val panelWidth = 900
  private val alignments = Vector(ParagraphAlignment.Center, ParagraphAlignment.Right, ParagraphAlignment.Left)

  private def buffer(document: RichTextDocument): Buffer =
    Buffer
      .fromString(BufferId(1), document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 120, visibleLines = 60)
      )

  private def document(paragraphs: Int, leadingBlankLines: Int): RichTextDocument =
    val leading = List.fill(leadingBlankLines)(RichTextParagraph.plain(""))
    val prose = (0 until paragraphs).map { index =>
      RichTextParagraph.plain(
        BenchmarkFixtures.longParagraph(240 + index % 7 * 30),
        alignment = alignments(index % alignments.size)
      )
    }
    RichTextDocument(leading ++ prose)

  private def headingDocument(paragraphs: Int): RichTextDocument =
    RichTextDocument((0 until paragraphs).toList.map { index =>
      if index % 3 == 0 then RichTextParagraph.plain(s"Heading $index", role = ParagraphRole.Heading(index % 6 + 1))
      else RichTextParagraph.plain(BenchmarkFixtures.longParagraph(240 + index % 7 * 30))
    })

  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    val cache = WrappedLineCache.bounded()
    val plain = buffer(document(paragraphs = 40, leadingBlankLines = 0))
    val moved = buffer(document(paragraphs = 40, leadingBlankLines = 1))
    def build(source: Buffer): TextLayoutSnapshot =
      TextLayoutSnapshot.fromBuffer(source, panelWidth, proseFont, wrapCache = cache)
    val expectedRows = build(plain).visualLines.size
    val headings     = buffer(headingDocument(paragraphs = 60))
    // Every wrap is measured afresh; what the owner has read off the font stays, as a process-wide cache once kept it.
    val measuring = WrappedLineCache.measuring(ShapedTextCache(), LayoutMemos())
    def buildUncached(source: Buffer): TextLayoutSnapshot =
      TextLayoutSnapshot.fromBuffer(source, panelWidth, proseFont, wrapCache = measuring)

    List(
      BenchmarkRunner.Benchmark(
        "layout.aligned_prose.cached_snapshot",
        BenchmarkIterationCounts.LayoutVisibleViewport,
        BenchmarkIterationCounts.LayoutVisibleViewport,
        () => assert(build(plain).visualLines.size == expectedRows),
        () => build(plain),
        settleJit = true
      ),
      BenchmarkRunner.Benchmark(
        "layout.aligned_prose.moved_lines",
        BenchmarkIterationCounts.LayoutVisibleViewport,
        BenchmarkIterationCounts.LayoutVisibleViewport,
        () => assert(build(moved).visualLines.exists(_.bufferLine > 0)),
        () => (build(plain), build(moved)),
        settleJit = true
      ),
      BenchmarkRunner.Benchmark(
        "layout.heading_prose.uncached_snapshot",
        BenchmarkIterationCounts.LayoutVisibleViewport,
        BenchmarkIterationCounts.LayoutVisibleViewport,
        () => assert(buildUncached(headings).visualLines.exists(_.bufferLine == 3)),
        () => buildUncached(headings),
        settleJit = true
      )
    )

end LayoutSnapshotBenchmarks
