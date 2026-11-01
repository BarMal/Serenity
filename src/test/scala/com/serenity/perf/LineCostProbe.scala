package com.serenity.perf

import java.awt.Font

import com.serenity.rope.Balance
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot, WrappedLineCache}

/** What counting the visual rows of a document's lines costs per line, by layout path (#2447 B2 and B4): which lines
  * take the measured path and which the cell path, what the cell path still costs next to plain arithmetic, and how many
  * lines are one row so a wider window could keep their count. Every pass uses a fresh cache and counts each line as
  * `VisualRowCounts.forBuffer` does (plain rich-text context, the font's own cell metrics).
  *
  * Times whole passes of `boundedVisualLinesForText`, not a frame. Usage:
  * `java -cp serenity-perf.jar com.serenity.perf.LineCostProbe [passes per arm] [warm-up passes]`.
  */
object LineCostProbe:

  private val lineCount = 1200

  /** The 1200-line document of prose and code the render benchmark uses. */
  private val benchmarkLines: Vector[String] =
    Vector.tabulate(lineCount) { index =>
      if index % 36 < 12 then s"Paragraph $index " + "lorem ipsum dolor sit amet " * (4 + index % 5 * 4)
      else s"  def step$index(value: Int): Int = value * ${index % 7 + 2} + ${index % 13}"
    }

  /** A source file: short code lines of varied length, blank lines and the odd long line. */
  private val sourceLines: Vector[String] =
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

  private final case class Arm(label: String, lines: Vector[String], font: Font, widthPx: Int, forceCells: Boolean)

  private def counts(arm: Arm, only: Int => Boolean): (Vector[Int], Double) =
    val cache   = WrappedLineCache.bounded(maxLines = lineCount * 2)
    val metrics = Some(CellMetrics.fromFont(arm.font))
    val started = System.nanoTime()
    val rows = arm.lines.zipWithIndex.map { (text, line) =>
      if only(line) then
        TextLayoutSnapshot
          .boundedVisualLinesForText(
            text,
            line,
            arm.widthPx,
            arm.font,
            cellMetricsOverride = metrics,
            forceCellLayout = arm.forceCells,
            wrapCache = cache
          )
          .length
          .max(1)
      else 1
    }
    (rows, (System.nanoTime() - started) / 1e6)

  private def arithmetic(arm: Arm): (Vector[Int], Double) =
    val columns = math.max(1, arm.widthPx / math.max(1, CellMetrics.fromFont(arm.font).charWidth))
    val started = System.nanoTime()
    val rows    = arm.lines.map(text => math.max(1, (text.length + columns - 1) / columns))
    (rows, (System.nanoTime() - started) / 1e6)

  /** The first two of `shouldUseMeasuredLayout`'s three conditions; its fractional-advance-drift check is private to the
    * layout package and is not included, so a true here is exact and a false is exact unless that check holds.
    */
  private def measuredPath(arm: Arm): Boolean =
    !arm.forceCells && (!FontLoader.isMonospacedFont(arm.font) || FontLoader.ligaturesEnabled(arm.font))

  private def percentile(sorted: Vector[Double], q: Double): Double = sorted(((sorted.size - 1) * q).toInt)

  private def report(label: String, passes: Vector[Double], lines: Int): Unit =
    val sorted = passes.sorted
    val p50    = percentile(sorted, 0.5)
    println(
      f"$label%-58s n=${sorted.size}%3d  p50 ${p50}%8.2f ms  p95 ${percentile(sorted, 0.95)}%8.2f ms  per line ${p50 * 1000 / lines}%7.1f us"
    )

  def main(args: Array[String]): Unit =
    given Balance = Balance.default
    val passes    = args.lift(0).flatMap(_.toIntOption).getOrElse(20)
    val warmUp    = args.lift(1).flatMap(_.toIntOption).getOrElse(5)
    val defaults  = FontLoader.FontConfig()
    val noLig     = FontLoader.FontConfig(enableLigatures = false, textLigatures = false)
    val text      = FontLoader.previewTextFont(defaults)
    val codeLig   = FontLoader.previewCodeFont(defaults)
    val codePlain = FontLoader.previewCodeFont(noLig)
    val width     = 1472
    val wider     = 1760
    val arms = List(
      Arm("A  benchmark doc, text font (as the app lays it out)", benchmarkLines, text, width, false),
      Arm("B  source file, code font, ligatures ON (default)", sourceLines, codeLig, width, false),
      Arm("C  source file, code font, ligatures OFF", sourceLines, codePlain, width, false),
      Arm("D  source file, code font, ligatures OFF, cells forced", sourceLines, codePlain, width, true)
    )
    println(s"Cost of counting the rows of ${lineCount} lines, fresh cache per pass, panel $width px (wider $wider px).")
    println(
      s"java ${System.getProperty("java.version")}, ${java.lang.Runtime.getRuntime.availableProcessors} processors, ${System.getProperty("os.name")} ${System.getProperty("os.arch")}"
    )
    println(s"text font '${text.getFontName}' monospaced=${FontLoader.isMonospacedFont(text)}")
    println(s"code font '${codeLig.getFontName}' monospaced=${FontLoader.isMonospacedFont(codeLig)}, ligatures default ON=${FontLoader.ligaturesEnabled(codeLig)}, OFF=${FontLoader.ligaturesEnabled(codePlain)}")
    arms.foreach(arm => println(s"  ${arm.label}: measured layout path = ${measuredPath(arm)}"))
    val all: Int => Boolean = _ => true
    (0 until warmUp).foreach(_ => arms.foreach(arm => counts(arm, all)))
    val timed = (0 until passes).toVector.map(_ => arms.map(arm => arm -> counts(arm, all)._2))
    arms.foreach(arm => report(arm.label, timed.map(_.find(_._1 == arm).get._2), lineCount))
    val floorArm = arms(2)
    report("E  arithmetic floor (ceil(length / columns)), source file", Vector.fill(passes)(arithmetic(floorArm)._2), lineCount)
    val exact  = counts(floorArm, all)._1
    val approx = arithmetic(floorArm)._1
    println(s"   arithmetic floor differs from the wrap on ${exact.zip(approx).count(_ != _)} of $lineCount source lines (words wrap early, tabs, wide glyphs)")

    println()
    println("B4: lines whose count at the old width is 1 keep it at a wider width.")
    List(arms(0), arms(2)).foreach { arm =>
      val atOld   = counts(arm, all)._1
      val widerArm = arm.copy(widthPx = wider)
      val atNew   = counts(widerArm, all)._1
      val single  = atOld.map(_ == 1)
      val broken  = atOld.indices.count(line => single(line) && atNew(line) != 1)
      val skipped = (0 until passes).toVector.map(_ => counts(widerArm, line => !single(line))._2)
      val full    = (0 until passes).toVector.map(_ => counts(widerArm, all)._2)
      println(f"  ${arm.label}: ${single.count(identity)}%d of $lineCount lines are one row at $width px; $broken lines of those are not one row at $wider px")
      report(s"  all lines at $wider px", full, lineCount)
      report(s"  only the lines that were not one row", skipped, lineCount - single.count(identity))
    }
