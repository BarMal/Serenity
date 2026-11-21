package com.serenity.perf

import java.awt.Font

import com.serenity.richtext.{ParagraphAlignment, ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, RichTextState, Viewport}
import com.serenity.testkit.TestMeasure
import com.serenity.ui.layout.{TextLayoutSnapshot, WrappedLineCache}

/** Snapshot builds over a warm wrap cache: the per-frame and per-navigation cost of laying out prose whose wraps are
  * already cached (#1908). Aligned paragraphs pay for placing caret stops, and a line inserted above moves every
  * paragraph to a new buffer line. The `warm_snapshot` rows are plain documents of different line lengths, where the
  * cost left on a hit is the walk of the document's lines: reading the text of the lines the viewport uses, and keying
  * the cache by it.
  */
private[perf] object LayoutSnapshotBenchmarks:

  given Balance = Balance.default

  private val proseFont = Font(Font.SERIF, Font.PLAIN, 14)
  private val plainFont = Font(Font.MONOSPACED, Font.PLAIN, 14)
  private val words =
    Vector("alpha", "bravo", "charlie", "delta", "echo", "foxtrot", "golf", "hotel", "india", "juliet")
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

  private def line(index: Int, chars: Int): String =
    val cycle = words.indices.map(step => words((index + 7 * step) % words.length)).mkString(" ")
    Iterator.continually(cycle).take(chars / cycle.length + 1).mkString(" ").take(chars)

  private def lines(count: Int, chars: Int): String = (0 until count).map(line(_, chars)).mkString("\n")

  /** A snapshot of `visibleLines` rows over `text`, which `usedLines` of its logical lines fill. */
  private def warmSnapshot(
    name: String,
    text: => String,
    visibleLines: Int,
    usedLines: Int
  ): BenchmarkRunner.Benchmark =
    lazy val source = Buffer
      .fromString(BufferId(1), text)
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = visibleLines, visibleColumns = 100))
    lazy val cache = TestMeasure.wrapCache()
    def build(): TextLayoutSnapshot =
      TextLayoutSnapshot.fromBuffer(source, panelWidth, plainFont, wrapCache = cache)
    BenchmarkRunner.Benchmark.building((source, cache))(
      BenchmarkRunner.Benchmark(
        name,
        BenchmarkIterationCounts.LayoutVisibleViewport,
        BenchmarkIterationCounts.LayoutVisibleViewport,
        () => assert(build().visualLines.map(_.bufferLine).distinct.size == usedLines),
        () => build(),
        settleJit = true
      )
    )

  /** Registered apart from [[benchmarks]], after the rows whose order the warm-up budget pins. */
  def warmSnapshots(): List[BenchmarkRunner.Benchmark] =
    List(
      warmSnapshot("layout.warm_snapshot.prose_550", lines(800, 550), visibleLines = 40, usedLines = 7),
      warmSnapshot("layout.warm_snapshot.code_80", lines(5000, 80), visibleLines = 50, usedLines = 50),
      warmSnapshot("layout.warm_snapshot.long_lines_20k", lines(40, 20000), visibleLines = 40, usedLines = 1),
      warmSnapshot("layout.warm_snapshot.minified_200k", lines(1, 200000), visibleLines = 40, usedLines = 1)
    )

  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    lazy val cache = TestMeasure.wrapCache()
    lazy val plain = buffer(document(paragraphs = 40, leadingBlankLines = 0))
    lazy val moved = buffer(document(paragraphs = 40, leadingBlankLines = 1))
    def build(source: Buffer): TextLayoutSnapshot =
      TextLayoutSnapshot.fromBuffer(source, panelWidth, proseFont, wrapCache = cache)
    lazy val expectedRows = build(plain).visualLines.size
    lazy val headings     = buffer(headingDocument(paragraphs = 60))
    // Every wrap is measured afresh; what the owner has read off the font stays, as a process-wide cache once kept it.
    lazy val measuring = WrappedLineCache.measuring(TestMeasure.layoutMemos())
    def buildUncached(source: Buffer): TextLayoutSnapshot =
      TextLayoutSnapshot.fromBuffer(source, panelWidth, proseFont, wrapCache = measuring)

    BenchmarkRunner.Benchmark.sharing((cache, plain, moved, expectedRows, headings, measuring))(
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
    )

end LayoutSnapshotBenchmarks
