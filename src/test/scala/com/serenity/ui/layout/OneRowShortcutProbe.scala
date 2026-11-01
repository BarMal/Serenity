package com.serenity.ui.layout

import java.awt.Font

import com.serenity.rope.Balance
import com.serenity.ui.fonts.FontLoader

/** Whether a line of proportional text can be called one row from its summed glyph advances (#2447, owner decision c):
  * if every character is context-free and the row's widest trailing edge is under the panel width by more than the
  * wrap's own float guard, the line is one row, and nothing else need be measured. Every other line falls back to the
  * full wrap. Lives in the layout package because `GlyphAdvances` is package-private; test-only, no product code.
  *
  * Times whole passes on a fresh cache (as `LineCostProbe` does) and checks the shortcut's "one row" against the full
  * wrap at many widths. Usage: `Test/runMain com.serenity.ui.layout.OneRowShortcutProbe [passes] [warm-up passes]`.
  */
object OneRowShortcutProbe:

  private val lineCount    = 1200
  private val guardPx      = 0.05f
  private val prose        = "lorem ipsum dolor sit amet " * 12
  private val widthPx      = 1472
  private val frc          = TextLayoutSnapshot.defaultFontRenderContext()

  private val benchmarkLines: Vector[String] =
    Vector.tabulate(lineCount) { index =>
      if index % 36 < 12 then s"Paragraph $index " + "lorem ipsum dolor sit amet " * (4 + index % 5 * 4)
      else s"  def step$index(value: Int): Int = value * ${index % 7 + 2} + ${index % 13}"
    }

  private val mixedCorpus = Vector(
    "", "short", "a short line of code()", prose, "x" * 600, "\tindented\tcode with\ttabs", "日本語のテキスト" * 15,
    "👩‍👩‍👧‍👦 family " * 12, "ＡＢＣ" * 40, "word " * 3, "  leading spaces " + "word " * 40, "fi fl ffi office affinity " * 6,
    "AV To Ty We Yo " * 8, "trailing spaces      ", "é combining ́ marks " * 10
  )

  private def wrapCount(cache: WrappedLineCache.Bounded, text: String, line: Int, width: Int, font: Font): Int =
    TextLayoutSnapshot
      .boundedVisualLinesForText(
        text,
        line,
        width,
        font,
        cellMetricsOverride = Some(CellMetrics.fromFont(font)),
        wrapCache = cache
      )
      .length
      .max(1)

  private def fitsOneRow(cache: WrappedLineCache.Bounded, text: String, width: Int, font: Font): Boolean =
    text.isEmpty || {
      val advances =
        GlyphAdvances.measure(text, 0, text.length, 0, TextCaretMeasurement.singleFontResolver(font), frc, cache.shapedText)
      advances.isContextFree(0, text.length) &&
      advances.trailingEdgesFrom(0, text.length).foldLeft(0.0f)((widest, edge) => math.max(widest, edge)) + guardPx <
        width.toFloat
    }

  private def timed[A](body: => A): (A, Double) =
    val started = System.nanoTime()
    val result  = body
    (result, (System.nanoTime() - started) / 1e6)

  private def fullPass(lines: Vector[String], font: Font): Double =
    val cache = WrappedLineCache.bounded(maxLines = lineCount * 2)
    timed(lines.zipWithIndex.map((text, line) => wrapCount(cache, text, line, widthPx, font)))._2

  private def decideOnlyPass(lines: Vector[String], font: Font): Double =
    val cache = WrappedLineCache.bounded(maxLines = lineCount * 2)
    timed(lines.map(text => fitsOneRow(cache, text, widthPx, font)))._2

  private def shortcutPass(lines: Vector[String], font: Font): (Double, Int) =
    val cache = WrappedLineCache.bounded(maxLines = lineCount * 2)
    val (decided, millis) = timed(lines.zipWithIndex.map { (text, line) =>
      if fitsOneRow(cache, text, widthPx, font) then true
      else
        val _ = wrapCount(cache, text, line, widthPx, font)
        false
    })
    (millis, decided.count(identity))

  private def percentile(sorted: Vector[Double], q: Double): Double = sorted(((sorted.size - 1) * q).toInt)

  private def report(label: String, passes: Vector[Double], lines: Int): Unit =
    val sorted = passes.sorted
    val p50    = percentile(sorted, 0.5)
    println(
      f"$label%-52s n=${sorted.size}%3d  p50 $p50%8.2f ms  p95 ${percentile(sorted, 0.95)}%8.2f ms  per line ${p50 * 1000 / lines}%7.1f us"
    )

  /** At each width, every line the shortcut calls one row must be one row to the full wrap. */
  private def exactness(label: String, lines: Vector[String], font: Font, widths: Seq[Int]): Unit =
    val checked = widths.map { width =>
      val cache = WrappedLineCache.bounded(maxLines = lineCount * 2)
      val verdicts = lines.zipWithIndex.map { (text, line) =>
        val decided = fitsOneRow(cache, text, width, font)
        (decided, decided && wrapCount(cache, text, line, width, font) != 1)
      }
      (verdicts.count(_._1), verdicts.count(_._2))
    }
    val decided = checked.map(_._1).sum
    val wrong   = checked.map(_._2).sum
    println(
      f"  $label%-40s widths ${widths.size}%4d  lines decided one row ${decided}%8d  disagreeing with the wrap ${wrong}%4d"
    )

  def main(args: Array[String]): Unit =
    given Balance = Balance.default
    val passes    = args.lift(0).flatMap(_.toIntOption).getOrElse(20)
    val warmUp    = args.lift(1).flatMap(_.toIntOption).getOrElse(5)
    val font      = FontLoader.previewTextFont(FontLoader.FontConfig())
    println(s"One-row shortcut for proportional text: text font '${font.getFontName}', panel $widthPx px, $lineCount lines.")
    println(
      s"java ${System.getProperty("java.version")}, ${java.lang.Runtime.getRuntime.availableProcessors} processors, ${System.getProperty("os.name")} ${System.getProperty("os.arch")}"
    )
    (0 until warmUp).foreach { _ =>
      fullPass(benchmarkLines, font)
      shortcutPass(benchmarkLines, font)
    }
    val rounds = (0 until passes).toVector.map { _ =>
      val full            = fullPass(benchmarkLines, font)
      val decide          = decideOnlyPass(benchmarkLines, font)
      val (shortcut, hit) = shortcutPass(benchmarkLines, font)
      (full, decide, shortcut, hit)
    }
    report("full wrap of every line", rounds.map(_._1), lineCount)
    report("advance sum of every line, nothing else", rounds.map(_._2), lineCount)
    report("shortcut: advance sum, full wrap of the rest", rounds.map(_._3), lineCount)
    println(s"  lines the shortcut called one row: ${rounds.head._4} of $lineCount")

    println()
    println("Exactness: a line called one row by the shortcut is one row to the full wrap.")
    exactness("benchmark lines, step 7 px", benchmarkLines, font, 200 to 1800 by 7)
    exactness("benchmark lines, step 1 px, 8 lines", benchmarkLines.slice(100, 108), font, 100 to 1800)
    exactness("mixed corpus, step 3 px", mixedCorpus, font, 60 to 1200 by 3)
