package com.serenity.ui.layout

import java.awt.Font
import java.awt.image.BufferedImage

import scala.util.Random

import com.serenity.rope.Balance
import com.serenity.ui.fonts.FontLoader

/** Phase 1 of a fractional-advance-tolerant fast row count (#2447 B2): where a code font's drift from the cell grid
  * comes from, how it accumulates over a line, why `isMonospaced` flips at some sizes, what the measured path spends on
  * a source line, and whether a table-sum "one row" decision agrees with the wrap. Test-only, no product code, and it
  * lives in the layout package because `GlyphAdvances` is package-private.
  */
object AdvanceDriftProbe:

  private val frc      = TextLayoutSnapshot.defaultFontRenderContext()
  private val printable = (0x20 to 0x7e).map(_.toChar).toVector
  private val guardPx   = 0.05f

  private val sourceLines: Vector[String] =
    Vector.tabulate(1200) { index =>
      index % 9 match
        case 0 => ""
        case 1 => s"  def step$index(value: Int): Int = value * ${index % 7 + 2} + ${index % 13}"
        case 2 => s"    val result$index = items.filter(_.size > ${index % 5}).map(_.name).mkString(\", \")"
        case 3 => s"  // step $index folds the parsed values into one total before the next stage reads it back"
        case 4 => "    case Some(value) if value >= 0 && value != 1 => value -> index"
        case 5 => s"  private val table$index = Map(\"alpha\" -> 1, \"beta\" -> 2, \"gamma\" => 3, \"delta\" -> ${index % 11})"
        case 6 => "  end step"
        case 7 => s"    require(index >= 0 && index < limit, s\"index $$index outside 0 until $$limit at stage $index\")"
        case _ => "}"
    }

  /** Printable ASCII with a code-like bias towards the characters that start ligatures. */
  private def randomLines(count: Int): Vector[String] =
    val random  = Random(42)
    val symbols = "=<>-!/*+&|:.~#_^%?@\\;,()[]{}\"'`"
    val plain   = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Seq(' ', ' ', ' ')).toVector
    Vector.fill(count) {
      val length = random.nextInt(261)
      String(Array.fill(length)(if random.nextInt(10) < 4 then symbols(random.nextInt(symbols.length)) else plain(random.nextInt(plain.size))))
    }

  private def fontMetricsWidths(font: Font): String =
    val image    = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
    val graphics = image.createGraphics()
    try
      val metrics = graphics.getFontMetrics(font)
      "iWmlM".map(char => s"$char=${metrics.charWidth(char)}").mkString(" ")
    finally graphics.dispose()

  final private class Table(font: Font, cache: WrappedLineCache.Bounded):
    private val resolver = TextCaretMeasurement.singleFontResolver(font)
    val advance: Map[Char, Float] = printable.map { char =>
      char -> GlyphAdvances.measure(char.toString, 0, 1, 0, resolver, frc, cache.shapedText).caretsFrom(0, 1)(1)
    }.toMap

    def lineAdvance(text: String): Option[Float] =
      if text.forall(char => advance.contains(char)) then Some(text.foldLeft(0.0f)((sum, char) => sum + advance(char)))
      else None

    def measured(text: String): GlyphAdvances =
      GlyphAdvances.measure(text, 0, text.length, 0, resolver, frc, cache.shapedText)

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

  private def timed[A](body: => A): Double =
    val started = System.nanoTime()
    val _       = body
    (System.nanoTime() - started) / 1e6

  private def p50(samples: Vector[Double]): Double = samples.sorted.apply(samples.size / 2)

  private def describe(label: String, font: Font): Unit =
    val cache   = WrappedLineCache.bounded()
    val probes  = cache.shapedText.probes
    val cell    = CellMetrics.fromFont(font).charWidth
    val table   = Table(font, cache)
    val widths  = table.advance.values.toVector
    val spread  = widths.max - widths.min
    val iAdv    = table.advance('i')
    val perChar = iAdv - cell
    println(f"  $label%-34s cell $cell  glyph advance min ${widths.min}%.4f max ${widths.max}%.4f spread $spread%.4f  'i' - cell = $perChar%+.4f px")
    val pad = " " * 36
    println(
      f"$pad per-line error vs cell grid: 80 chars ${80 * perChar}%+.1f px, 120 chars ${120 * perChar}%+.1f px, 200 chars ${200 * perChar}%+.1f px"
    )
    println(
      pad + s" FontMetrics ints: ${fontMetricsWidths(font)}; isMonospaced=${FontLoader.isMonospacedFont(font)}; " +
        s"uniform=${GlyphAdvances.advancesAreUniform(font, frc, probes)} layoutMatchesSum=${GlyphAdvances.layoutAdvanceMatchesSum(font, frc, probes)} " +
        s"shapingMaySubstitute=${GlyphAdvances.shapingMaySubstitute(font, frc, probes)} hasLayoutAttributes=${font.hasLayoutAttributes}"
    )

  private def costs(label: String, font: Font, passes: Int): Unit =
    val resolver = TextCaretMeasurement.singleFontResolver(font)
    def measureAll(): Double =
      val cache = WrappedLineCache.bounded(maxLines = 4000)
      timed(sourceLines.foreach(text => if text.nonEmpty then GlyphAdvances.measure(text, 0, text.length, 0, resolver, frc, cache.shapedText)))
    def wrapAll(): Double =
      val cache = WrappedLineCache.bounded(maxLines = 4000)
      timed(sourceLines.zipWithIndex.foreach((text, line) => wrapCount(cache, text, line, 1472, font)))
    def tableSum(): Double =
      val cache = WrappedLineCache.bounded(maxLines = 4000)
      val table = Table(font, cache)
      timed(sourceLines.foreach(text => table.lineAdvance(text)))
    val cache = WrappedLineCache.bounded(maxLines = 4000)
    val table = Table(font, cache)
    val free  = sourceLines.filter(_.nonEmpty).count(text => table.measured(text).isContextFree(0, text.length))
    (0 until 3).foreach { _ => measureAll(); wrapAll(); tableSum() }
    val m = p50(Vector.fill(passes)(measureAll()))
    val w = p50(Vector.fill(passes)(wrapAll()))
    val t = p50(Vector.fill(passes)(tableSum()))
    println(
      f"  $label%-34s GlyphAdvances.measure ${m}%7.2f ms  full wrap ${w}%7.2f ms  table sum ${t}%6.3f ms  (1200 lines)  context-free lines ${free}%d of ${sourceLines.count(_.nonEmpty)}%d"
    )

  /** The naive table-sum decision: a line whose summed advance plus the wrap's float guard is under the width is one row. */
  private def agreement(label: String, font: Font, lines: Vector[String], widths: Seq[Int]): Unit =
    val setup   = WrappedLineCache.bounded()
    val table   = Table(font, setup)
    val totals  = lines.map(table.lineAdvance)
    var decided = 0L
    var wrong   = 0L
    var nearGuard = 0L
    widths.foreach { width =>
      val cache = WrappedLineCache.bounded(maxLines = lines.size * 2)
      lines.indices.foreach { line =>
        totals(line).foreach { total =>
          if math.abs(total - width.toFloat) < guardPx then nearGuard += 1
          else if total + guardPx < width.toFloat then
            decided += 1
            if wrapCount(cache, lines(line), line, width, font) != 1 then wrong += 1
        }
      }
    }
    println(f"  $label%-34s widths ${widths.size}%4d  lines decided one row ${decided}%9d  disagree with the wrap ${wrong}%5d  within the guard (fall back) $nearGuard%d")

  def main(args: Array[String]): Unit =
    given Balance = Balance.default
    val passes    = args.lift(0).flatMap(_.toIntOption).getOrElse(10)
    val defaults  = FontLoader.FontConfig()
    println(s"java ${System.getProperty("java.version")}, ${java.lang.Runtime.getRuntime.availableProcessors} processors, ${System.getProperty("os.name")} ${System.getProperty("os.arch")}")
    println(s"font render context: fractionalMetrics=${frc.usesFractionalMetrics} antiAliased=${frc.isAntiAliased} transform=${frc.getTransform}")
    val sizes = List(11.0f, 12.0f, 13.0f, 14.0f, 15.0f, 16.0f, 18.0f)
    val fonts = for
      size <- sizes
      lig  <- List(false, true)
    yield (f"code ${size}%4.1f pt ligatures ${if lig then "ON " else "OFF"}", FontLoader.previewCodeFont(defaults.copy(fontSize = size, enableLigatures = lig)))
    println("\nA. Drift against the cell grid, and why isMonospaced flips")
    fonts.foreach((label, font) => describe(label, font))
    println(s"\nB. Cost of a 1200-line source file at 1472 px, n=$passes (container; measure = GlyphAdvances.measure of every line)")
    fonts.filter((label, _) => label.contains(" 12.0 ") || label.contains(" 14.0 ") || label.contains(" 13.0 ")).foreach((label, font) => costs(label, font, passes))
    println("\nC. A table-sum one-row decision against the wrap (source lines + 4000 random printable-ASCII lines)")
    val lines  = sourceLines ++ randomLines(4000)
    val widths = 200 to 1800 by 17
    fonts.filter((label, _) => label.contains(" 12.0 ") || label.contains(" 14.0 ") || label.contains(" 13.0 ")).foreach((label, font) => agreement(label, font, lines, widths))
