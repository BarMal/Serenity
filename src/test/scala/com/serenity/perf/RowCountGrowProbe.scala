package com.serenity.perf

import java.awt.Font

import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.ui.layout.{VisualRowCounts, WrappedLineCache}

/** Why `layout.row_count.after_grow` reads slower than `layout.row_count.cold` (#2447 B4, laptop result): the row is a
  * narrow count plus a wide one. This probe times the pieces on the benchmark's own document, alternating the arms each
  * round in one process. Test-only, no product code.
  */
object RowCountGrowProbe:

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

  private def wraps(cache: WrappedLineCache.Bounded): Long = cache.wrapStats.coldWraps + cache.wrapStats.incrementalWraps

  private def millis[A](body: => A): (A, Double) =
    val started = System.nanoTime()
    val result  = body
    (result, (System.nanoTime() - started) / 1e6)

  final private case class Round(
    cold: Double,
    coldWide: Double,
    row: Double,
    second: Double,
    secondUnseeded: Double,
    secondWraps: Long,
    unseededWraps: Long
  )

  private def round(): Round =
    val (_, cold)     = millis(totalRows(WrappedLineCache.bounded(), narrowPx))
    val (_, coldWide) = millis(totalRows(WrappedLineCache.bounded(), widePx))
    val (_, row) = millis {
      val cache = WrappedLineCache.bounded()
      totalRows(cache, narrowPx)
      totalRows(cache, widePx)
    }
    val seeded = WrappedLineCache.bounded()
    totalRows(seeded, narrowPx)
    val before          = wraps(seeded)
    val (_, second)     = millis(totalRows(seeded, widePx))
    val secondWraps     = wraps(seeded) - before
    val unseeded        = WrappedLineCache.sharing(seeded.shapedText)
    val (_, unseededMs) = millis(totalRows(unseeded, widePx))
    Round(cold, coldWide, row, second, unseededMs, secondWraps, wraps(unseeded))

  private def percentile(sorted: Vector[Double], p: Double): Double =
    sorted(math.min(sorted.length - 1, math.ceil(p * sorted.length).toInt - 1).max(0))

  def main(args: Array[String]): Unit =
    val warmups = args.headOption.map(_.toInt).getOrElse(15)
    val rounds  = args.lift(1).map(_.toInt).getOrElse(30)
    println(s"java ${System.getProperty("java.version")}, ${Runtime.getRuntime.availableProcessors} processors, ${System.getProperty("os.name")}")
    println(s"font ${font.getFontName}, document $lineCount lines, narrow $narrowPx px, wide $widePx px, $warmups warm-up rounds, $rounds measured, alternating in one process")
    (1 to warmups).foreach(_ => round())
    val measured = Vector.fill(rounds)(round())
    def line(label: String, pick: Round => Double): Unit =
      val sorted = measured.map(pick).sorted
      println(f"  $label%-62s p50 ${percentile(sorted, 0.5)}%7.2f ms  p95 ${percentile(sorted, 0.95)}%7.2f  min ${sorted.head}%7.2f")
    line("A cold count at 1472 px (the `cold` row)", _.cold)
    line("B cold count at 1760 px, fresh cache", _.coldWide)
    line("C narrow then wide on one cache (the `after_grow` row)", _.row)
    line("D the wide count alone, after a narrow count (seeded)", _.second)
    line("E the wide count alone, shaped text warm, no seeding", _.secondUnseeded)
    println(f"  C - A (what the row's second count costs, by subtraction)       p50 ${percentile(measured.map(r => r.row - r.cold).sorted, 0.5)}%7.2f ms")
    println(s"  lines wrapped by the seeded wide count: ${measured.map(_.secondWraps).distinct.sorted.mkString(",")}   by the unseeded one: ${measured.map(_.unseededWraps).distinct.sorted.mkString(",")}   (of $lineCount)")
