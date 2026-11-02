package com.serenity.perf

import java.awt.Font

import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot, VisualRowCounts, WrappedLineCache}

/** Counting the visual rows of every line of a 1,200-line source file in the bundled code font, from a fresh cache and
  * at a 1472 px panel (#2447 B2):
  *
  *   - `layout.row_count.source_ligatures_off`: ligatures off, so a line the font's own advances put on one row is
  *     counted without wrapping it;
  *   - `layout.row_count.source_ligatures_on`: ligatures on (the default), where every line is wrapped.
  *
  * Both verify against the wrap of each line on its own. Run just these, without a window:
  * `sbt "Test/runMain com.serenity.perf.SourceRowCountBenchmarks"`.
  */
private[perf] object SourceRowCountBenchmarks:

  given Balance = Balance.default

  private val lineCount = 1200
  private val widthPx   = 1472

  private val lines: Vector[String] =
    Vector.tabulate(lineCount) { index =>
      index % 9 match
        case 0 => ""
        case 1 => s"  def step$index(value: Int): Int = value * ${index % 7 + 2} + ${index % 13}"
        case 2 => s"    val result$index = items.filter(_.size > ${index % 5}).map(_.name).mkString(\", \")"
        case 3 => s"  // step $index folds the parsed values into one total before the next stage reads it back"
        case 4 => "    case Some(value) if value >= 0 && value != 1 => value -> index"
        case 5 =>
          s"  private val table$index = Map(\"alpha\" -> 1, \"beta\" -> 2, \"gamma\" => 3, \"delta\" -> ${index % 11})"
        case 6 => "  end step"
        case 7 =>
          s"    require(index >= 0 && index < limit, s\"index $$index outside 0 until $$limit at stage $index\")"
        case _ => "}"
    }

  private val buffer = Buffer.fromString(BufferId(1), lines.mkString("\n"))

  private def codeFont(ligatures: Boolean): Font =
    FontLoader.previewCodeFont(FontLoader.FontConfig(enableLigatures = ligatures))

  private def totalRows(font: Font): Int =
    val rows = VisualRowCounts.forBuffer(
      buffer,
      widthPx,
      font,
      Some(CellMetrics.fromFont(font)),
      false,
      WrappedLineCache.bounded()
    )
    Vector.tabulate(lineCount)(rows.rowsIn).sum

  /** What wrapping each line on its own says, which no shortcut takes part in. */
  private def wrappedRows(font: Font): Int =
    val cache = WrappedLineCache.bounded()
    lines.zipWithIndex.map { (text, line) =>
      TextLayoutSnapshot
        .boundedVisualLinesForText(
          text,
          line,
          widthPx,
          font,
          cellMetricsOverride = Some(CellMetrics.fromFont(font)),
          wrapCache = cache
        )
        .length
        .max(1)
    }.sum

  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    List(("source_ligatures_off", false), ("source_ligatures_on", true)).map { (name, ligatures) =>
      val font     = codeFont(ligatures)
      val expected = wrappedRows(font)
      BenchmarkRunner.Benchmark(
        s"layout.row_count.$name",
        2,
        10,
        () => assert(totalRows(font) == expected),
        () => totalRows(font),
        settleJit = true
      )
    }

  def main(args: Array[String]): Unit =
    BenchmarkRunner.printResults(BenchmarkRunner.runMatching(args.toList, benchmarks()))

end SourceRowCountBenchmarks
