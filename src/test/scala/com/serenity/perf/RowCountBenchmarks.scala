package com.serenity.perf

import java.awt.Font

import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.ui.layout.{VisualRowCounts, WrappedLineCache}

/** Counting the visual rows of every line of a 1200-line document of prose and code, the document the render benchmark
  * uses (#2447 B4):
  *
  *   - `layout.row_count.cold`: a fresh cache counting at a 1472 px panel, every line wrapped;
  *   - `layout.row_count.after_grow`: the same, then the panel grows 20% to 1760 px on the same cache. The lines that
  *     were one row keep their count, so only the others are wrapped again; what the second count costs is this row
  *     less the cold one.
  *
  * Run just these, without a window: `sbt "Test/runMain com.serenity.perf.RowCountBenchmarks"`.
  */
private[perf] object RowCountBenchmarks:

  given Balance = Balance.default

  private val font      = Font(Font.SERIF, Font.PLAIN, 14)
  private val narrowPx  = 1472
  private val widePx    = 1760
  private val lineCount = 1200

  private val buffer = Buffer.fromString(
    BufferId(1),
    Vector
      .tabulate(lineCount) { index =>
        if index % 36 < 12 then s"Paragraph $index " + "lorem ipsum dolor sit amet " * (4 + index % 5 * 4)
        else s"  def step$index(value: Int): Int = value * ${index % 7 + 2} + ${index % 13}"
      }
      .mkString("\n")
  )

  private def totalRows(cache: WrappedLineCache, widthPx: Int): Int =
    val rows = VisualRowCounts.forBuffer(buffer, widthPx, font, None, false, cache)
    Vector.tabulate(lineCount)(rows.rowsIn).sum

  private def cold(): Int = totalRows(WrappedLineCache.bounded(), narrowPx)

  private def afterGrow(): Int =
    val cache = WrappedLineCache.bounded()
    val _     = totalRows(cache, narrowPx)
    totalRows(cache, widePx)

  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    val expectedCold = totalRows(WrappedLineCache.Uncached, narrowPx)
    val expectedWide = totalRows(WrappedLineCache.Uncached, widePx)
    List(
      BenchmarkRunner.Benchmark(
        "layout.row_count.cold",
        2,
        10,
        () => assert(cold() == expectedCold),
        () => cold(),
        settleJit = true
      ),
      BenchmarkRunner.Benchmark(
        "layout.row_count.after_grow",
        2,
        10,
        () => assert(afterGrow() == expectedWide),
        () => afterGrow(),
        settleJit = true
      )
    )

  def main(args: Array[String]): Unit =
    BenchmarkRunner.printResults(BenchmarkRunner.runMatching(args.toList, benchmarks()))

end RowCountBenchmarks
