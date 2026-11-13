package com.serenity.ui.layout

import java.awt.Font
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.ui.layout.TextMeasureCorpus.{Line, PinnedFont}
import com.serenity.ui.renderer.{FontFirstUse, FontProbes}

/** What AWT measures for the bundled fonts and the corpus lines, as data: cell metrics, whether a font takes the
  * measured layout, whether a ligature font's certificate holds, and for each line its caret x positions on one row and
  * its rows, their end columns, widths and heights when wrapped to two pane widths. These are the numbers the Skia text
  * measurer has to reproduce or the owner has to accept differing (#2660 item 2). Integers must match exactly; a float
  * may differ by [[FloatTolerance]] so one host's rounding does not fail another's.
  *
  * The checked-in file is the reference environment's (CI's Linux runner), not a portable truth: a host whose AWT
  * measures differently fails the spec here as it does for `frame-counts.tsv`.
  */
object TextMeasureBaseline:

  given Balance = Balance.default

  val Resource          = "/text-measure/baseline.tsv"
  val RegenerateCommand = "sbt writeTextMeasureBaseline"
  val FloatTolerance    = 0.01

  private val Unwrapped    = 100_000
  private val WrapWidthsPx = List(240, 600)

  private val FloatKinds = Set("carets", "widths")

  private val Header =
    s"""# Text measurement pinned by TextMeasureBaselineSpec: font.line.kind, comma-separated values.
       |# Integers are exact; carets and widths (pixels) may differ by $FloatTolerance. If a change is intended, run
       |# `$RegenerateCommand` and commit this file with a line in the PR saying why the numbers moved.
       |""".stripMargin

  /** One pinned measurement: `key` is `font.kind` or `font.line.kind`, `values` the numbers as text. */
  final case class Entry(key: String, values: Vector[String]):
    def kind: String = key.split('.').last.takeWhile(_ != '@')

    def isFloat: Boolean = FloatKinds.contains(kind)

  final case class Change(key: String, baseline: Option[Vector[String]], now: Option[Vector[String]])

  private def ints(key: String, values: Seq[Int]): Entry = Entry(key, values.map(_.toString).toVector)

  private def floats(key: String, values: Seq[Float]): Entry = Entry(key, values.map(v => f"$v%.4f").toVector)

  def entries(): Vector[Entry] = TextMeasureCorpus.fonts.flatMap(measureFont)

  private def measureFont(pinned: PinnedFont): Vector[Entry] =
    val frc         = TextLayoutSnapshot.defaultFontRenderContext()
    val metrics     = CellMetrics.fromFont(pinned.font)
    val measured    = TextLayoutSnapshot.shouldUseMeasuredLayout(pinned.font, frc, FontProbes())
    val certificate = LigatureCertificate.of(pinned.font, frc).isDefined
    val perFont = Vector(
      ints(s"${pinned.label}.cell", Seq(metrics.charWidth, metrics.lineHeight, metrics.ascent)),
      ints(s"${pinned.label}.measured", Seq(if measured then 1 else 0)),
      ints(s"${pinned.label}.certificate", Seq(if certificate then 1 else 0))
    )
    perFont ++ TextMeasureCorpus.lines.flatMap(measureLine(pinned, _))

  private def snapshot(pinned: PinnedFont, line: Line, widthPx: Int): TextLayoutSnapshot =
    TextLayoutSnapshot.fromBuffer(
      Buffer.fromString(BufferId(1), line.text),
      panelWidthPx = widthPx,
      pinned.font,
      wordWrapEnabled = widthPx != Unwrapped,
      fallback = pinned.fallback
    )

  /** How many glyphs AWT shapes the line to in the font alone, and the character each starts at: what shaping did,
    * before layout snaps cells or draws a fallback font. Read straight from `layoutGlyphVector`, left to right.
    */
  private def glyphEntries(pinned: PinnedFont, line: Line, prefix: String): Vector[Entry] =
    val frc = TextLayoutSnapshot.defaultFontRenderContext()
    FontFirstUse.settle(pinned.font, frc)
    val chars  = line.text.toCharArray
    val shaped = pinned.font.layoutGlyphVector(frc, chars, 0, chars.length, Font.LAYOUT_LEFT_TO_RIGHT)
    val count  = shaped.getNumGlyphs
    Vector(
      ints(s"$prefix.glyphs", Seq(count)),
      ints(s"$prefix.clusters", shaped.getGlyphCharIndices(0, count, new Array[Int](count)).toSeq)
    )

  private def measureLine(pinned: PinnedFont, line: Line): Vector[Entry] =
    val prefix = s"${pinned.label}.${line.id}"
    glyphEntries(pinned, line, prefix) ++ measureLayout(pinned, line, prefix)

  private def measureLayout(pinned: PinnedFont, line: Line, prefix: String): Vector[Entry] =
    val onOne =
      snapshot(pinned, line, Unwrapped).visualLines.headOption.map(_.row.caretXs.toVector).getOrElse(Vector.empty)
    if TextMeasureCorpus.structuralOnly(pinned, line) then Vector(ints(s"$prefix.stops", Seq(onOne.length)))
    else
      Vector(floats(s"$prefix.carets", onOne)) ++ WrapWidthsPx.flatMap { widthPx =>
        val rows = snapshot(pinned, line, widthPx).visualLines.map(_.row)
        Vector(
          ints(s"$prefix.rows@$widthPx", Seq(rows.length)),
          ints(s"$prefix.ends@$widthPx", rows.map(_.endColumn)),
          floats(s"$prefix.widths@$widthPx", rows.map(_.widthPx)),
          ints(s"$prefix.heights@$widthPx", rows.map(_.heightPx))
        )
      }

  def render(entries: Seq[Entry]): String =
    Header + entries.map(entry => s"${entry.key}\t${entry.values.mkString(",")}").mkString("", "\n", "\n")

  def parse(lines: Seq[String]): Either[String, Vector[Entry]] =
    lines
      .map(_.trim)
      .filterNot(line => line.isEmpty || line.startsWith("#"))
      .toVector
      .foldLeft(Right(Vector.empty[Entry]): Either[String, Vector[Entry]]) { (parsed, line) =>
        parsed.flatMap { entries =>
          line.split('\t') match
            case Array(key, values) if key.nonEmpty && values.split(',').forall(_.toDoubleOption.isDefined) =>
              Right(entries :+ Entry(key, values.split(',').toVector))
            case _ => Left(s"expected 'font.line.kind<TAB>numbers' in the text-measure baseline, got '$line'")
        }
      }

  def load(): Either[String, Vector[Entry]] =
    Option(getClass.getResourceAsStream(Resource)) match
      case None => Left(s"no $Resource on the test classpath")
      case Some(stream) =>
        val text =
          try new String(stream.readAllBytes(), StandardCharsets.UTF_8)
          finally stream.close()
        parse(text.linesIterator.toList)

  private def sameValues(entry: Entry, other: Vector[String]): Boolean =
    entry.values.length == other.length &&
      entry.values.zip(other).forall { (a, b) =>
        if entry.isFloat then math.abs(a.toDouble - b.toDouble) <= FloatTolerance + 1e-6 else a == b
      }

  /** Every key whose numbers differ between the baseline and now, in baseline order, then keys the baseline lacks. */
  def changes(baseline: Seq[Entry], now: Seq[Entry]): List[Change] =
    val current  = now.map(entry => entry.key -> entry).toMap
    val recorded = baseline.map(_.key).toSet
    val moved = baseline.toList.flatMap { entry =>
      current.get(entry.key) match
        case Some(measured) if sameValues(entry, measured.values) => None
        case other => Some(Change(entry.key, Some(entry.values), other.map(_.values)))
    }
    moved ++ now
      .filterNot(entry => recorded.contains(entry.key))
      .map(entry => Change(entry.key, None, Some(entry.values)))

  def main(args: Array[String]): Unit =
    val target = Paths.get(args.headOption.getOrElse(sys.error("usage: TextMeasureBaseline <baseline.tsv>")))
    val _      = Files.write(target, render(entries()).getBytes(StandardCharsets.UTF_8))
