package com.serenity.ui.layout

import java.awt.Font

import com.serenity.rope.Balance
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.renderer.ShapedTextCache

/** Settles why counting a source file's rows cost 12.0 ms in one probe and 69 to 86 ms in another for the same font
  * (#2447 B2). Alternating arms in one process, same fonts and lines: fresh caches (every pass re-runs the font probes
  * a long-lived cache keeps) against a shared warm `ShapedTextCache`, the old probe's lines against the newer probe's,
  * ligatures off against on, and a table-sum "one row" guard that falls back to the wrap. Test-only, no product code.
  */
object WrapCostAbProbe:

  private val frc       = TextLayoutSnapshot.defaultFontRenderContext()
  private val guardPx   = 0.05f
  private val lineCount = 1200
  private val widthPx   = 1472

  /** `LineCostProbe`'s source file. */
  private val oldLines: Vector[String] =
    Vector.tabulate(lineCount) { index =>
      index % 9 match
        case 0 => ""
        case 1 => s"  def step$index(value: Int): Int = value * ${index % 7 + 2} + ${index % 13}"
        case 2 => s"    val result$index = items.filter(_.size > ${index % 5}).map(_.name).mkString(\", \")"
        case 3 => s"  // step $index folds the parsed values into one total before the next stage reads it back"
        case 4 => "    case Some(value) if value > 0 => value"
        case 5 => s"  private val table$index = Map(\"alpha\" -> 1, \"beta\" -> 2, \"gamma\" -> 3, \"delta\" -> ${index % 11})"
        case 6 => "  end step"
        case 7 => s"    require(index >= 0 && index < limit, s\"index $$index outside 0 until $$limit at stage $index\")"
        case _ => "}"
    }

  /** `AdvanceDriftProbe`'s source file: two of the nine line shapes differ. */
  private val newLines: Vector[String] =
    oldLines.zipWithIndex.map { (text, index) =>
      index % 9 match
        case 4 => "    case Some(value) if value >= 0 && value != 1 => value -> index"
        case 5 => s"  private val table$index = Map(\"alpha\" -> 1, \"beta\" -> 2, \"gamma\" => 3, \"delta\" -> ${index % 11})"
        case _ => text
    }

  private val printable = (0x20 to 0x7e).map(_.toChar)

  private def wrapCount(cache: WrappedLineCache, text: String, line: Int, font: Font): Int =
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

  private def table(font: Font, shapedText: ShapedTextCache): Map[Char, Float] =
    val resolver = TextCaretMeasurement.singleFontResolver(font)
    printable.map(char => char -> GlyphAdvances.measure(char.toString, 0, 1, 0, resolver, frc, shapedText).caretsFrom(0, 1)(1)).toMap

  private def timedCounts(body: => Vector[Int]): (Vector[Int], Double) =
    val started = System.nanoTime()
    val counts  = body
    (counts, (System.nanoTime() - started) / 1e6)

  private def wrapAll(cache: WrappedLineCache, lines: Vector[String], font: Font): Vector[Int] =
    lines.zipWithIndex.map((text, line) => wrapCount(cache, text, line, font))

  private def guarded(cache: WrappedLineCache, lines: Vector[String], font: Font, advances: Map[Char, Float]): Vector[Int] =
    lines.zipWithIndex.map { (text, line) =>
      val total = if text.forall(advances.contains) then Some(text.foldLeft(0.0f)((sum, char) => sum + advances(char))) else None
      if total.exists(_ + guardPx < widthPx.toFloat) then 1 else wrapCount(cache, text, line, font)
    }

  private def percentile(sorted: Vector[Double], q: Double): Double = sorted(((sorted.size - 1) * q).toInt)

  def main(args: Array[String]): Unit =
    given Balance = Balance.default
    val rounds    = args.lift(0).flatMap(_.toIntOption).getOrElse(20)
    val config    = FontLoader.FontConfig()
    val off       = FontLoader.previewCodeFont(config.copy(enableLigatures = false))
    val on        = FontLoader.previewCodeFont(config.copy(enableLigatures = true))
    // Shared shaped-text caches whose font probes are already settled, as a long-lived cache's are.
    val warmOff = ShapedTextCache()
    val warmOn  = ShapedTextCache()
    val warmOffAdvances = table(off, warmOff)
    val warmOffCache    = WrappedLineCache.sharing(warmOff)
    val _               = wrapAll(warmOffCache, newLines, off)
    val _               = wrapAll(WrappedLineCache.sharing(warmOn), newLines, on)
    println(s"java ${System.getProperty("java.version")}, ${java.lang.Runtime.getRuntime.availableProcessors} processors, ${System.getProperty("os.name")} ${System.getProperty("os.arch")}")
    println(s"$lineCount lines, panel $widthPx px, $rounds alternating rounds after 5 warm-up rounds, one process; 12 pt bundled code font")

    final case class Arm(label: String, run: () => (Vector[Int], Double))
    val arms = List(
      Arm("1 OFF wrap, old lines, fresh cache", () => timedCounts(wrapAll(WrappedLineCache.bounded(maxLines = 4000), oldLines, off))),
      Arm("2 OFF wrap, new lines, fresh cache", () => timedCounts(wrapAll(WrappedLineCache.bounded(maxLines = 4000), newLines, off))),
      Arm("3 OFF wrap, old lines, warm shared shapedText", () => timedCounts(wrapAll(WrappedLineCache.sharing(warmOff), oldLines, off))),
      Arm("4 OFF wrap, new lines, warm shared shapedText", () => timedCounts(wrapAll(WrappedLineCache.sharing(warmOff), newLines, off))),
      Arm("5 ON  wrap, new lines, fresh cache", () => timedCounts(wrapAll(WrappedLineCache.bounded(maxLines = 4000), newLines, on))),
      Arm("6 ON  wrap, new lines, warm shared shapedText", () => timedCounts(wrapAll(WrappedLineCache.sharing(warmOn), newLines, on))),
      Arm("7 OFF guard + fallback, new lines, fresh cache (builds the table)", () =>
        val shaped = ShapedTextCache()
        timedCounts(guarded(WrappedLineCache.sharing(shaped), newLines, off, table(off, shaped)))
      ),
      Arm("8 OFF guard + fallback, new lines, warm cache and table", () => timedCounts(guarded(warmOffCache, newLines, off, warmOffAdvances))),
      Arm("9 OFF wrap of ONE short line, fresh cache (per-cache constant)", () => timedCounts(wrapAll(WrappedLineCache.bounded(), Vector("val x = 1"), off))),
      Arm("10 OFF wrap of ONE short line, warm shared shapedText", () => timedCounts(wrapAll(WrappedLineCache.sharing(warmOff), Vector("val x = 1"), off)))
    )
    (0 until 5).foreach(_ => arms.foreach(_.run()))
    val results = (0 until rounds).toVector.map(_ => arms.map(arm => arm -> arm.run()))
    val reference = results.head.find(_._1.label.startsWith("2 ")).get._2._1
    arms.foreach { arm =>
      val samples = results.map(round => round.find(_._1 eq arm).get._2._2).sorted
      val counts  = results.head.find(_._1 eq arm).get._2._1
      val agrees  = if arm.label.startsWith("7") || arm.label.startsWith("8") then s"  counts equal arm 2: ${counts == reference}" else ""
      println(f"  ${arm.label}%-66s p50 ${percentile(samples, 0.5)}%8.2f ms  p95 ${percentile(samples, 0.95)}%8.2f ms  min ${samples.head}%8.2f ms$agrees")
    }
