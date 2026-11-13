package com.serenity.ui.layout

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import com.serenity.ui.layout.TextMeasureBaseline.Entry
import com.serenity.ui.layout.TextMeasureCorpus.{Line, PinnedFont}
import com.serenity.ui.layout.TextMeasurer.LineFonts
import com.serenity.ui.renderer.FontSpec
import com.serenity.ui.skia.SkiaTextMeasurer

/** Where [[SkiaTextMeasurer]] differs from what AWT measures (#2660 item 2, step B): the numbers the owner sets the
  * tolerance from. It only reports; the spec that writes it asserts nothing about how large a difference is.
  */
object TextMeasureParity:

  /** One number on each side. `key` is a baseline key, `index` the position inside its list of values. */
  final case class Compared(key: String, index: Int, awt: Double, skia: Double, isFloat: Boolean):
    def delta: Double = skia - awt

    def size: Double = math.abs(delta)

  final case class Skipped(key: String, reason: String)

  /** Lines whose app measurement is not just the measurer's: a tab is expanded by layout, wide and emoji clusters are
    * snapped to cells and drawn in fallback fonts by layout, and the right-to-left line is only counted.
    */
  val notComparable: Map[String, String] = Map(
    "tabs"           -> "tabs are expanded by layout, not measured by the measurer",
    "cjk"            -> "wide characters are snapped to cells and use fallback fonts in layout",
    "emoji_clusters" -> "emoji clusters are snapped to cells and use fallback fonts in layout",
    "ltr_rtl"        -> "only the number of stops is pinned, no bundled font holds the right-to-left glyphs"
  )

  private val OneRowWidthPx = 600

  /** The numbers this measurer gives for `pinned`, under the baseline's keys, for what both sides can compare. */
  def skiaEntries(pinned: PinnedFont, measurer: SkiaTextMeasurer): Vector[Entry] =
    val spec = FontSpec.fromAwt(pinned.font)
    val cell = measurer.cellMetrics(spec)
    val perFont = Vector(
      Entry(s"${pinned.label}.cell", Vector(cell.charWidth, cell.lineHeight, cell.ascent).map(_.toString)),
      Entry(s"${pinned.label}.measured", Vector(if measurer.measuresProportionally(spec) then "1" else "0"))
    )
    perFont ++ TextMeasureCorpus.lines.filterNot(line => notComparable.contains(line.id)).flatMap { line =>
      val fonts  = LineFonts(spec, Vector.empty)
      val carets = measurer.caretXs(line.text, 0, fonts, measuredLayout = true, cell)
      val height = measurer.lineMetrics(fonts, 0, line.text.length).heightPx
      Vector(
        Entry(s"${pinned.label}.${line.id}.carets", carets.toVector.map(value => f"$value%.4f")),
        Entry(s"${pinned.label}.${line.id}.heights@$OneRowWidthPx", Vector(height.toString))
      )
    }

  /** Every number both sides hold under a key, and why the others were not compared. */
  def compare(reference: Seq[Entry], skia: Seq[Entry]): (Vector[Compared], Vector[Skipped]) =
    val referenceByKey = reference.map(entry => entry.key -> entry).toMap
    val results = skia.toVector.map { entry =>
      referenceByKey.get(entry.key) match
        case None => Right(Skipped(entry.key, "no reference value"))
        case Some(awt) if awt.values.length != entry.values.length =>
          Right(Skipped(entry.key, "wrapped to several rows in layout; wrap points are not comparable before step D"))
        case Some(awt) =>
          Left(awt.values.zip(entry.values).zipWithIndex.map {
            case ((a, s), index) =>
              Compared(entry.key, index, a.toDouble, s.toDouble, awt.isFloat)
          })
    }
    (results.collect { case Left(values) => values }.flatten, results.collect { case Right(skipped) => skipped })

  private val Buckets: List[(String, Double)] =
    List(
      "exact"   -> 0.0,
      "<= 0.01" -> 0.01,
      "<= 0.05" -> 0.05,
      "<= 0.1"  -> 0.1,
      "<= 0.5"  -> 0.5,
      "> 0.5"   -> Double.MaxValue
    )

  private def bucketOf(size: Double): String =
    Buckets.find((_, limit) => size <= limit + 1e-9).map(_._1).getOrElse("> 0.5")

  /** `font.line.kind` to `kind`, so the table reads by what was measured. */
  private def kindOf(key: String): String = key.split('.').last.takeWhile(_ != '@')

  final case class Section(title: String, note: String, compared: Vector[Compared], skipped: Vector[Skipped])

  def summary(header: Seq[String], sections: Seq[Section]): String =
    val out = new StringBuilder
    out.append("# Skia text measurer against AWT (step B, #2660 item 2)\n\n")
    header.foreach(line => out.append(s"- $line\n"))
    sections.foreach { section =>
      out.append(s"\n## ${section.title}\n\n${section.note}\n\n")
      out.append("| kind | numbers | " + Buckets.map(_._1).mkString(" | ") + " | max |\n")
      out.append("|---|---|" + Buckets.map(_ => "---").mkString("|") + "|---|\n")
      section.compared.groupBy(c => kindOf(c.key)).toVector.sortBy(_._1).foreach { (kind, values) =>
        val counts = values.groupBy(c => bucketOf(c.size)).view.mapValues(_.size).toMap
        val row    = Buckets.map((name, _) => counts.getOrElse(name, 0).toString).mkString(" | ")
        out.append(f"| $kind | ${values.size} | $row | ${values.map(_.size).max}%.4f |\n")
      }
      val worst = section.compared.filter(_.size > 0.0).sortBy(c => -c.size).take(12)
      if worst.nonEmpty then
        out.append("\nLargest differences (key, index, AWT, Skia):\n\n")
        worst.foreach(c => out.append(f"- ${c.key} [${c.index}]: ${c.awt}%.4f, ${c.skia}%.4f (${c.delta}%+.4f)\n"))
      val reasons = section.skipped.groupBy(_.reason).toVector.sortBy(_._1)
      if reasons.nonEmpty then
        out.append("\nNot compared:\n\n")
        reasons.foreach((reason, items) => out.append(s"- ${items.size} keys: $reason\n"))
    }
    out.toString

  def tsv(sections: Seq[Section]): String =
    "section\tkey\tindex\tawt\tskia\tdelta\n" +
      sections
        .flatMap(section =>
          section.compared.map(c => s"${section.title}\t${c.key}\t${c.index}\t${c.awt}\t${c.skia}\t${c.delta}")
        )
        .mkString("", "\n", "\n")

  def write(directory: Path, text: String, rows: String): Unit =
    val _ = Files.createDirectories(directory)
    val _ = Files.write(directory.resolve("summary.md"), text.getBytes(StandardCharsets.UTF_8))
    val _ = Files.write(directory.resolve("diffs.tsv"), rows.getBytes(StandardCharsets.UTF_8))
